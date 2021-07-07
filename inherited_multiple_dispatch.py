"""

### RULES ###


(1) All classes wishing to access "inherited multiple dispatch features" must inherit from InheritedDispatchBase.

(2) You must use the @inherited_dispatch decorator on all functions
    you want to be able to access "inherited multiple dispatch" features.

(3) Methods decorated with @inherited_dispatch must have all arguments type-annotated.
    (This includes type-annotating 'self', 'class', etc. --
    it will be cleaner if you do from __future__ import annotations.)

(4) There must be exactly one registered implementation of a multiple-dispatch function
    that has the argument base_impl=True passed to the @inherited_dispatch decorator.
    This informs the module that this implementation should be seen as the "fallback" implementation.

(5) The types of the arguments passed to a function must exactly match the types given in an implementation annotation
    in order for that implementation to be returned. Anything else, and the fallback implementation is returned.

(6) Allowed "special types" for annotations are:
    - typing.Annotated                                          (Resolves to a normal type when typing.get_type_hints() is called on it)
    - typing.Any                                                (CAN'T be used with isinstance())
    - typing.Union                                              (CAN'T be used with isinstance())
    - Paramaterised generics (list[str] etc.)                   (CAN'T be used with isinstance())
    - Parameterised typing.Final                                (CAN'T be used with isinstance())

    - typing.Protocol if it's marked as being runtime_checkable (CAN be used with isinstance())
    - Paramaterised collections.abc (MutableSequence[str] etc)  (CAN be used with isinstance(), but only if NOT parameterised)
    - typing.Callable                                           (CAN be used with isinstance(), but only if NOT parameterised)

(7) Using the | operator between types in annotations is disallowed (use typing.Union instead)

(8) Disallowed types for annotations are the following.
    (Implementation for some of these would be possible but would probably slow it down/complicate it more.)

    - typing.Literal
    - typing.Protocol if it's not marked as being runtime_checkable
    - typing.Generic
    - typing.TypeVar
    - typing.ClassVar
    - unparamaterised typing.Final

(9) classmethods and staticmethods are possible,
    but you cannot use the standard builtin @classmethod and @staticmethod decorators.
    Instead, mark the methods as classmethods or staticmethods by passing class_method=True or static_method=True
    into the @inherited_dispatch decorator.

(10) All implementations of any one method must be the same 'kind' of method
    -- they must all either be instance methods, all static methods, or all class methods.

(11) You can't use the @inherited_dispatch decorator on dunder methods (that would be crazy!)

(12) Only a base implementation of a function is allowed to have a variable number of arguments.
    All other implementations of a function must have a fixed number of positional and keyword arguments.

(13) The base implementation may have a variable number of positional arguments,
    or a variable number of keyword arguments. However, it may not have both.

"""

from __future__ import annotations

from functools import partial, cache, update_wrapper
from collections import UserList, UserDict
from abc import ABCMeta
from types import GenericAlias
from inspect import signature, Parameter, getsource
from itertools import chain, groupby

import typing as t
import warnings

STATIC = 'static'
CLASS = 'cls'
INSTANCE = 'instance'

if t.TYPE_CHECKING:
	from collections.abc import Callable, Iterable, Sequence


	class InheritedDispatchFunc(t.Protocol):
		def __call__(*args: t.Any, **kwargs: t.Any) -> t.Any:
			...

		__annotations__: dict[str, t.Any]
		__name__: str
		inherited_dispatch: bool
		base_impl: bool
		suppress_warnings: bool
		kind: str
		checked: bool


	Callable_Any_Type = Callable[[t.Any], t.Any]

	AnnotationType = t.Union[
		type,
		t._UnionGenericAlias,
		t._ProtocolMeta,
		t._CallableType,
		t._GenericAlias,
		t._SpecialForm,
		GenericAlias,
		ABCMeta
	]


class InheritedDispatchWarning(UserWarning):
	pass


def raise_for_invalid_hint(hint: t.Any) -> None:
	if hint == t.Final:
		raise TypeError("inherited_dispatch decorator won't work with unparameterised typing.Final")
	if isinstance(hint, t.TypeVar):
		raise TypeError("inherited_dispatch decorator won't work with typing.TypeVar")
	if getattr(hint, '_is_protocol', False) and (not getattr(hint, '_is_runtime_protocol', False)):
		raise TypeError(
			"inherited_dispatch decorator won't work with classes that inherit from typing.Protocol, "
			"unless they are decorated with @typing.runtime_checkable."
		)
	if type(hint) == t.Literal:
		raise TypeError("inherited_dispatch decorator won't work with typing.Literal")
	if type(hint) == t._GenericAlias and (origin := hint.__origin__) in (t.Generic, t.ClassVar):
		raise TypeError(f"inherited_dispatch decorator won't work with {origin}")


def paramaterised_hint_detected(hint) -> bool:
	# Unparameterised abcs have type of ABCMeta, but we don't need to worry about those in this function.
	# This function is only for warning the user that we won't take care of *paramaterised* collection-types;
	# unparamaterised collection types are still fine.

	if isinstance(hint, (
			# E.g. directly parameterised builtins:                 isinstance(list[str], types.GenericAlias) -> True
			GenericAlias,

			# E.g. typing aliases for builtins or collections.abc:  isinstance(typing.List[str], typing._GenericAlias) -> True
			# isinstance(typing.MutableSequence[str], typing._GenericAlias) -> True
			t._GenericAlias
	)):
		return True

	# Detect whether typing.Callable has been parameterised or not -- no need to emit a warning if not.
	if isinstance(hint, t.Callable):
		return bool(getattr(hint, '__args__', False))
	return False


@t.overload
def inherited_dispatch(
		base_impl: bool = False,
		static_method: bool = False,
		class_method: bool = False,
		suppress_warnings: bool = False
) -> t.Callable[[Callable_Any_Type], InheritedDispatchFunc]:
	...


@t.overload
def inherited_dispatch(func: Callable_Any_Type) -> InheritedDispatchFunc:
	...


def inherited_dispatch(
		func: Callable_Any_Type | None = None,
		*,
		base_impl: bool = False,
		static_method: bool = False,
		class_method: bool = False,
		suppress_warnings: bool = False
) -> t.Union[InheritedDispatchFunc, partial[InheritedDispatchFunc]]:

	"""
	Decorator to mark out the functions that you want to operate by the rules of inheritable singledispatch.
	See docstring at the top of this module for rules on usage.
	"""

	if func is None:
		return partial(
			inherited_dispatch,
			base_impl=base_impl,
			static_method=static_method,
			class_method=class_method,
			suppress_warnings=suppress_warnings
		)

	# Check syntax of the type annotations (get_type_hints doesn't work with | operator between types)
	# It would be possible to use regular expressions to coerce it into the correct syntax,
	# but that would be very complex...)
	ann = func.__annotations__

	if any('|' in annot for annot in ann.values()):
		raise TypeError(
			"You cannot use the | operator between types for inherited_dispatch annotations. "
			"Use typing.Union instead."
		)

	# Check if the decorator has been called with invalid arguments (can't be both an instance or static method)
	if static_method and class_method:
		raise TypeError("A function cannot be both a class method and a static method.")

	# Check the decorator is being used on a callable object that doesn't start with a dunder
	if not callable(func):
		raise TypeError("This decorator can only be used on functions")

	if func.__name__.startswith('__'):
		raise TypeError("Can't use the inherited_dispatch decorator on dunder methods")

	# Check the name of the first argument (depends on whether it's a static, instance or class method)
	params = signature(func).parameters
	first_arg = tuple(params.keys())[0]

	if static_method:
		if first_arg in ('self', 'cls'):
			raise TypeError(
				'A static method does not have access to the attributes of the instance or the class. '
				'As such, you must not call the first argument of a staticmethod "self" or "cls".'
			)
		func.kind = STATIC
	elif class_method:
		if first_arg != CLASS:
			raise TypeError(
				"The first argument for a class method should always be called 'cls' when using this decorator"
			)
		func.kind = CLASS
	else:
		if first_arg != 'self':
			raise TypeError(
				"The first argument for an instance method should always be called 'self' when using this decorator. "
				"If the method is a static method or a class method, "
				"it must be explicitly labelled as such in every implementation."
			)
		func.kind = INSTANCE

	# Check all arguments in the function have been annotated (we don't care about the return argument)
	if not (
			(len(ann) == len(params))
			or (len(ann) == (len(params) + 1) and 'return' in ann)
	):
		raise TypeError(
			"You must type-hint every single argument when using the inherited_dispatch decorator, "
			"or it will not work correctly."
		)

	# Check to see if there are variable numbers of arguments (allowed in a base_impl, otherwise not.
	# And a base_impl can't have variable numbers of positional arguments *and* variable numbers of keyword-arguments.
	variable_param_types = {
		p.kind for p in params.values() if p.kind in (Parameter.VAR_POSITIONAL, Parameter.VAR_KEYWORD)
	}

	if base_impl:
		if len(variable_param_types) == 2:
			raise TypeError(
				"The base implementation of a function may have variable numbers "
				"of either key-word or positional arguments, "
				"but may not have variable positional arguments *and* variable keyword arguments"
			)
		else:
			func.variable_args_no = bool(variable_param_types)
	elif variable_param_types:
		raise TypeError("Only the base implementation of a function is allowed to have variable numbers of arguments.")

	func.inherited_dispatch = True
	func.base_impl = base_impl
	func.suppress_warnings = suppress_warnings
	func.checked = False
	return func


class CallableFunctionList(UserList):
	"""
	Container object for a list of function implementations.

	When a user attempts to access a multiple-dispatch method, they are in fact returned an instance of this class.
	When they attempt to call the method they have accessed,
	this class is able to ascertain which implementation is required from the arguments that have been supplied.
	"""

	def __init__(
			self,
			data: list | None = None,
			name: str = ''
	) -> None:

		super().__init__(data if data is not None else [])
		self.name = name
		self.base_impl: Callable_Any_Type | None = None
		self.base_impl_args_len = 0
		self.variable_args_no = False
		self.kind: t.Literal['static'] | t.Literal['cls'] | t.Literal['instance'] | None = None
		self.checked = False

	def extend_flag_unchecked(self, iterable: Iterable) -> None:
		self.extend(iterable)
		self.checked = False

	@cache
	def arg_compares_equal(self, annotated_type: AnnotationType, arg_type: type) -> bool:
		# If the types match or an implementation has typing.Any, return True
		if annotated_type == arg_type or annotated_type == t.Any:
			return True

		if type(annotated_type) == type:
			return False

		# typing.Protocol (ones that aren't runtime-checkable won't have made it this far)
		# also unparameterised collections.abc
		if isinstance(annotated_type, (t.Protocol, ABCMeta)):
			return isinstance(arg_type, annotated_type)

		# Parameterised AND unparameterised typing.Callable
		if isinstance(annotated_type, t.Callable) and callable(arg_type):
			return True

		# typing.Union
		if type(annotated_type) == t._UnionGenericAlias:
			return any(self.arg_compares_equal(ann_type, arg_type) for ann_type in annotated_type.__args__)

		# Parameterised collections.abc, parameterised builtins, parameterised typing aliases (List, Tuple, etc.)
		if isinstance(annotated_type, (GenericAlias, t._GenericAlias)):
			return self.arg_compares_equal(annotated_type.__origin__, arg_type)

		# typing.Final
		if type(annotated_type) == t._SpecialForm and annotated_type.__origin__ == t.Final:
			return self.arg_compares_equal(annotated_type.__args__[0], arg_type)

		return False

	@cache
	def all_args_compare_equal(
			self,
			annotated_types: Sequence[AnnotationType],
			arg_types: Sequence[type]
	) -> bool:

		"""
		Comparisons for base implementation are done differently --
		for any other implementations, we need an exact match in numbers of arguments
		"""

		if len(annotated_types) != len(arg_types):
			return False
		return all(
			self.arg_compares_equal(annotated_type, arg_type)
			for annotated_type, arg_type in zip(annotated_types, arg_types)
		)

	@cache
	def find_impl(self, *argtypes: type) -> Callable_Any_Type:
		try:
			return next(func for func in self.data if self.all_args_compare_equal(func.type_hints, argtypes))
		except StopIteration:
			args_no, exception_needed = len(argtypes), False
			if (
					(self.variable_args_no and args_no < self.base_impl_args_len)
					or (not self.variable_args_no and args_no != self.base_impl_args_len)
			):
				raise TypeError(
					f"No registered implementation exists for that number of arguments and those types of arguments. "
					f"The base implementation of function {self.name} takes "
					f"{'at least' if self.variable_args_no else 'exactly'} "
					f"{self.base_impl_args_len} argument(s). {args_no} arguments were supplied."
				) from None
			return self.base_impl

	def _check_function_impls(self) -> None:
		"""This function should only be accessed by InheritedDispatchMeta"""

		if self.checked:
			return None

		g = groupby(func.kind for func in self.data)
		if not (next(g, True) and not next(g, False)):
			raise TypeError(
				"Multiple implementations of one function, all with the same name, must be of the same kind. "
				"E.g. they must all be class methods, instance methods or static methods"
			)

		base_impls = [func for func in self.data if func.base_impl]

		if len(base_impls) != 1:
			raise TypeError(
				f"Error in function {self.name}: "
				"There must be exactly one function implementation registered as a fallback implementation."
			)

		if (base_impl := base_impls[0]) != self.base_impl:
			self.base_impl = base_impl

			ann = base_impl.__annotations__
			ann_len = len(ann)

			self.base_impl_args_len = (ann_len - 1) if 'return' in ann else ann_len
			self.variable_args_no = base_impl.variable_args_no
			self.kind = base_impl.kind

		for func in filter(lambda func: not func.checked, self.data):
			# Convert the annotations into types, add it to the function,
			# emit a UserWarning if Generic types or GenericAliases are being used.
			try:
				type_hints_dict = t.get_type_hints(func)
			except Exception as err:
				raise TypeError(
					"When using @inherited_dispatch, all annotations must be resolvable to a type "
					"using the typing.get_type_hints function."
				) from err

			if 'return' in type_hints_dict:
				del type_hints_dict['return']

			type_hints = list(type_hints_dict.values())

			for hint in type_hints:
				try:
					raise_for_invalid_hint(hint)
				except TypeError as err:
					raise TypeError(
						f'Invalid annotation in function {func} with annotations {type_hints_dict}'
					) from err

			# Type-checking the first argument for a classmethod works differently
			# as the type of nearly all classes == type
			if self.kind == CLASS and type(type_hints[0]) == GenericAlias:
				type_hints[0] = type_hints[0].__args__[0]

			if not func.suppress_warnings and any(paramaterised_hint_detected(hint) for hint in type_hints):
				warnings.warn(
					"inherited_dispatch: Warning regarding function {func.__name__}: "
					"It is permitted to use parameterised alias types "
					"(e.g. list[str], MutableMapping[str, int]) in combination with this decorator. "
					"However, be aware that, for example, "
					"in a situation where an argument is annotated as being of type list[str], "
					"the algorithm will *only* check that the argument inputted is a list -- "
					"it will not check whether the items in the list are all of type str."
					"The only parameterised annotations that do not raise this warning "
					"are typing.Union and typing.Annotated."
					"Use the suppress_warnings=True) argument in the decorator to suppress this warning."
				)

			func.type_hints = tuple(type_hints)
			func.checked = True

		self.checked = True

	# Can't cache this -- we don't know if the function we're calling will be pure or not.
	def __call__(self, *args: t.Any, **kwargs: t.Any) -> t.Any:
		if self.kind == CLASS:
			impl = self.find_impl(args[0], *(type(arg) for arg in chain(args[1:], kwargs.values())))
		else:
			impl = self.find_impl(*(type(arg) for arg in chain(args, kwargs.values())))
		return impl(*args, **kwargs)

	def __hash__(self) -> int:
		return id(self)

	def __str__(self) -> str:
		return f'Implementation list for function {self.name}'

	def __repr__(self) -> str:
		return f'Implementation list for function {self.name}: {self.data}'


class _DictAllowingDuplicates(UserDict):
	"""
	A dict that will stash multiple copies of a function with the same name
	if they're marked with the @inherited_dispatch decorator
	"""

	def __init__(
			self,
			data: dict | None = None
	) -> None:

		data = data if data else {}
		data['_annotated_funcdict'] = {}
		super().__init__(data)

	def __setitem__(
			self,
			key: str,
			value: t.Any
	) -> None:

		if not hasattr(value, 'inherited_dispatch'):
			self.data[key] = value
		else:
			funcdict = self.data['_annotated_funcdict']

			if key not in funcdict:
				funcdict[key] = CallableFunctionList(name=key)
			funcdict[key].append(value)

	def to_dict(self) -> dict[str, t.Any]:
		return self.data


class InheritedDispatchMeta(type):
	"""
	Metaclass that works with the @inheritable_dispatch decorator
	to allow for inheritable multiple-dispatch functions
	"""

	def __prepare__(self, *args: t.Any) -> _DictAllowingDuplicates:
		return _DictAllowingDuplicates()

	@staticmethod
	def get_funcdict(some_class: InheritedDispatchMeta) -> dict[str, CallableFunctionList]:
		"""Helper method for __new__() and __init__() below"""

		return some_class.__dict__['_annotated_funcdict']

	def __new__(
			metacls: type[InheritedDispatchMeta],
			cls_name: str,
			cls_bases: tuple[type],
			cls_dict: _DictAllowingDuplicates
	) -> InheritedDispatchMeta:

		new_dict = cls_dict.to_dict()

		def added_init_fragment(self) -> None:
			for func_list in self.annotated_funcdict.values():
				func_list._check_function_impls()

		new_init_required = False

		try:
			old_init = new_dict['__init__']
		except KeyError:
			if not cls_bases:
				added_init_fragment.__name__ = '__init__'
				__init__ = added_init_fragment
				new_init_required = True
		else:
			if cls_bases:
				old_init_co_names = old_init.__code__.co_names

			if not cls_bases or not all((
					'super().__init__(' in getsource(old_init),
					'super' in old_init_co_names,
					'__init__' in old_init_co_names
			)):
				new_init_required = True

				def __init__(self, *args: t.Any, **kwargs: t.Any) -> None:
					old_init(self, *args, **kwargs)
					added_init_fragment(self)

				__init__.__signature__ = signature(old_init)
				update_wrapper(__init__, old_init)

		if new_init_required:
			new_dict['__init__'] = __init__

		return super().__new__(metacls, cls_name, cls_bases, new_dict)

	def __init__(
			cls: InheritedDispatchMeta,
			cls_name: str,
			cls_bases: tuple[InheritedDispatchMeta],
			cls_dict: dict[str, t.Any]
	) -> None:

		for parent in filter(lambda x: '_annotated_funcdict' in x.__dict__, cls.__mro__):
			for k, v in cls.get_funcdict(parent).items():
				if k not in cls.get_funcdict(cls):
					cls.get_funcdict(cls)[k] = CallableFunctionList(name=k)
				cls.get_funcdict(cls)[k].extend_flag_unchecked(filter(lambda f: f not in cls.get_funcdict(cls)[k], v))

		super().__init__(cls_name, cls_bases, cls_dict)


class NiceReprPartial:
	"""
	A wrapper around functools.partial so that CallableFunctionList
	doesn't have a horrible __repr__ when it's partialised
	"""

	__slots__ = 'original_callable', 'partialised_callable', 'calling_object'

	def __init__(
			self,
			original_callable: CallableFunctionList,
			calling_object: t.Any
	) -> None:

		self.original_callable = original_callable
		self.calling_object = calling_object
		self.partialised_callable = self.partialise(original_callable, calling_object)

	@staticmethod
	def partialise(original_callable: CallableFunctionList, calling_object):
		if (k := original_callable.kind) == STATIC:
			return original_callable
		return partial(original_callable, (calling_object.__class__ if k == CLASS else calling_object))

	def __iter__(self) -> Iterable[Callable_Any_Type]:
		return self.original_callable.__iter__()

	def __getitem__(self, index: int) -> Callable_Any_Type:
		return self.original_callable.__getitem__(index)

	def __repr__(self) -> str:
		return f'{repr(self.original_callable)}. Bound as a method to {self.calling_object}.'

	def __call__(
			self,
			*args: t.Any,
			**kwargs: t.Any
	) -> t.Any:

		return self.partialised_callable(*args, **kwargs)

	def print_argtypes(self) -> None:
		"""Mainly here for debugging purposes"""
		for tup in [tuple(t.get_type_hints(func).values()) for func in self.original_callable]:
			print(tup)


class InheritedDispatchBase(metaclass=InheritedDispatchMeta):
	"""
	All classes wishing to use inheritable multiple-dispatch must inherit from this base class,
	due to the special __getattribute__ method defined here.
	"""

	def __getattr__(self, key: str) -> t.Any:
		ann_funcs = self.__class__.__dict__['_annotated_funcdict']

		if key == '_annotated_funcdict':
			return ann_funcs

		if key in ann_funcs:
			return NiceReprPartial(ann_funcs[key], self)

		raise AttributeError(f'No such attribute or function in class "{self.__class__}" (key "{key}" was provided)')

	@property
	def annotated_funcdict(self) -> dict[str, CallableFunctionList]:
		"""A dictionary containing all of the multiple-dispatch functions for this class"""
		return self._annotated_funcdict

	@annotated_funcdict.setter
	def annotated_funcdict(self, value: t.Any) -> None:
		raise AttributeError('Not allowed to set the special atribute "annotated_funcdict" at runtime')
