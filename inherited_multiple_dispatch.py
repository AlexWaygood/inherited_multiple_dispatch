"""

### RULES ###


(1) All classes wishing to access "inherited multiple dispatch features" must use InheritedDispatchMeta as the metaclass.

(2)	InheritedDispatchBase is provided as a convenient base class that can be used (see examples.py),
	but it is not essential to inherit from InheritedDispatchBase.
	You could very easily write your own base class, if it uses InheritedDispatchMeta as its metaclass.

(3) Overriding __getattr__ in any classes using InheritedDispatchMeta as the metaclass is not allowed.
	If you try to do this, the metaclass will throw a TypeError.

(4) You must use the @inherited_dispatch decorator on all functions
	you want to be able to access "inherited multiple dispatch" features.

(5) Methods decorated with @inherited_dispatch must have all arguments type-annotated.
	(This includes type-annotating 'self', 'class', etc. --
	it will be cleaner if you do from __future__ import annotations.)

(6) There must be exactly one registered implementation of a multiple-dispatch function
	that has the argument base_impl=True passed to the @inherited_dispatch decorator.
	This informs the module that this implementation should be seen as the "fallback" implementation.

(7) The types of the arguments passed to a function must exactly match the types given in an implementation annotation
	in order for that implementation to be returned. Anything else, and the fallback implementation is returned.

	There are two exceptions to this: if you annotate an argument with TypeVar and "bind" the TypeVar to a class,
	any subclasses of the bound class will "match" the annotation.
	TypeVars may be bound to typing.Union as well as to a single class.
	However, they may not be bound to forward references.

	The other exception is typing.Any, which will match with any argument type.

(8) Be aware that if you use flexible/ambiguous types such as typing.TypeVar, typing.Any or typing.Union,
	this module makes no guarantee that it will find the "best" implementation
	that "most closely" matches the types of the arguments provided.
	It will return the first implementation it finds that matches all of the argument types --
	it's on you if you're not specific enough!

(9) Allowed "special types" for annotations are:
	- typing.Annotated                                          (Resolves to a normal type when typing.get_type_hints() is called on it)
	- typing.Any                                                (CAN'T be used with isinstance())
	- typing.Union                                              (CAN'T be used with isinstance())
	- Paramaterised generics (list[str] etc.)                   (CAN'T be used with isinstance())
	- Parameterised typing.Final                                (CAN'T be used with isinstance())

	- typing.TypeVar, if it's been bound to a type.             (CAN be used with isinstance(), but only if it's bound to an actual type - typing.Union won't cut it.)
	- typing.Protocol if it's marked as being runtime_checkable (CAN be used with isinstance())
	- Paramaterised collections.abc (MutableSequence[str] etc)  (CAN be used with isinstance(), but only if NOT parameterised)
	- typing.Callable                                           (CAN be used with isinstance(), but only if NOT parameterised)

(10) Using the | operator between types in annotations is disallowed (use typing.Union instead).
	Other features introduced in python 3.10 may not work either; this has only been tested on python 3.9.

(11) Disallowed types for annotations are the following.
	(Implementation for some of these would be possible but would probably slow it down/complicate it more.)

	- typing.Literal
	- typing.Protocol if it's not marked as being runtime_checkable
	- typing.Generic
	- typing.ClassVar
	- unparamaterised typing.Final

(12) classmethods and staticmethods are possible,
	but you cannot use the standard builtin @classmethod and @staticmethod decorators.
	Instead, mark the methods as classmethods or staticmethods by passing class_method=True or static_method=True
	into the @inherited_dispatch decorator.

(13) All implementations of any one method must be the same 'kind' of method
	-- they must all either be instance methods, all static methods, or all class methods.

(14) You can't use the @inherited_dispatch decorator on dunder methods (that would be crazy!)

(15) Only a base implementation of a function is allowed to have a variable number of arguments.
	All other implementations of a function must have a fixed number of positional and keyword arguments.

(16) The base implementation may have a variable number of positional arguments,
	or a variable number of keyword arguments. However, it may not have both.

"""

from __future__ import annotations

from functools import partial, cache, update_wrapper
from abc import ABCMeta
from types import GenericAlias
from inspect import signature, Parameter, getsource
from itertools import chain, groupby

import typing as t
import warnings


# Type-checking stuff that doesn't affect performance of the module at runtime.
if t.TYPE_CHECKING:
	StaticType = t.Literal['static']
	ClassType = t.Literal['cls']
	InstanceType = t.Literal['instance']
	FunctionKindType = t.Union[StaticType, ClassType, InstanceType]

	from typing_extensions import ParamSpec
	P = ParamSpec('P')
	R = t.TypeVar('R')
	CallableTypeVar = t.TypeVar('CallableTypeVar', bound=t.Callable)
	FunctionKindTypeVar = t.TypeVar('FunctionKindTypeVar', bound=t.Optional[FunctionKindType])

	class InheritedDispatchFuncType(t.Generic[CallableTypeVar, FunctionKindTypeVar]):
		__call__: CallableTypeVar
		__annotations__: dict[str, t.Any]
		__name__: str
		inherited_dispatch: bool
		base_impl: bool
		suppress_warnings: bool
		kind: FunctionKindTypeVar
		checked: bool
		variable_args_no: bool


	AnyList = list[t.Any]
	CallableAnyType = t.Callable[..., t.Any]

	class ImplementationIterator(t.Generic[FunctionKindTypeVar]):
		def __iter__(self, /) -> t.Iterable[InheritedDispatchFuncType[CallableAnyType, FunctionKindTypeVar]]: ...


	class CallableListType(t.Protocol[FunctionKindTypeVar]):
		__name__: str
		__call__: CallableAnyType
		base_impl: t.Optional[InheritedDispatchFuncType[CallableAnyType, FunctionKindTypeVar]]
		base_impl_args_len: int
		variable_args_no: bool
		kind: FunctionKindTypeVar
		checked: bool
		def find_impl(self, /, *argtypes: type) -> InheritedDispatchFuncType[CallableAnyType, FunctionKindTypeVar]: ...
		def _check_function_impls(self, /) -> None: ...
		def __hash__(self, /) -> int: ...
		def __str__(self, /) -> str: ...
		def __repr__(self, /) -> str: ...
		def __add__(self, other: AnyList, /) -> CallableListType[FunctionKindTypeVar]: ...
		def __contains__(self, item: t.Any, /) -> bool: ...
		def __delitem__(self, key: int, /) -> None: ...
		def __eq__(self, other: t.Any, /) -> bool: ...
		def __ge__(self, other: AnyList, /) -> bool: ...
		def __getattribute__(self, item: str, /) -> t.Any: ...
		def __getitem__(self, item: int) -> InheritedDispatchFuncType[CallableAnyType, FunctionKindTypeVar]: ...
		def __gt__(self, other: AnyList, /) -> bool: ...
		def __iadd__(self, other: AnyList, /) -> None: ...
		def __imul__(self, other: int, /) -> t.NoReturn: ...
		def __iter__(self, /) -> ImplementationIterator: ...
		def __le__(self, other: AnyList, /) -> bool: ...
		def __len__(self, /) -> int: ...
		def __lt__(self, other: AnyList, /) -> bool: ...
		def __mul__(self, other: int, /) -> t.NoReturn: ...
		def __ne__(self, other: t.Any, /) -> bool: ...
		def __reversed__(self, /) -> ImplementationIterator: ...
		def __rmul__(self, other: int, /) -> t.NoReturn: ...
		def __sizeof__(self, /) -> int: ...
		def append(self, value: InheritedDispatchFuncType[CallableAnyType, FunctionKindTypeVar], /) -> None: ...
		def clear(self, /) -> None: ...
		def copy(self, /) -> CallableListType[FunctionKindTypeVar]: ...
		def count(self, value: InheritedDispatchFuncType[CallableAnyType, FunctionKindTypeVar], /) -> int: ...
		def extend(self, iterable: ImplementationIterator[FunctionKindTypeVar], /) -> None: ...
		def index(self, value, start=0, stop=9223372036854775807, /) -> int: ...
		def pop(self, index: int = -1, /) -> InheritedDispatchFuncType[CallableAnyType, FunctionKindTypeVar]: ...
		def remove(self, value: InheritedDispatchFuncType[CallableAnyType, FunctionKindTypeVar], /) -> None: ...
		def reverse(self, /) -> None: ...
		def sort(self, *args, **kwargs) -> t.NoReturn: ...
		def extend_flag_unchecked(self, iterable: ImplementationIterator) -> None: ...
		def arg_compares_equal(self, annotated_type: AnnotationType, arg_type: type) -> bool: ...

		def all_args_compare_equal(
				self,
				annotated_types: t.Sequence[AnnotationType],
				arg_types: t.Sequence[type]
		) -> bool: ...

		def __init__(
				self,
				data: t.Optional[list[InheritedDispatchFuncType[CallableAnyType, FunctionKindTypeVar]]],
				name: str,
				/
		) -> None:
			...

		def __setitem__(
				self,
				key: int,
				value: InheritedDispatchFuncType[CallableAnyType, FunctionKindTypeVar],
				/
		) -> None:
			...

		def insert(
				self,
				index: int,
				value: InheritedDispatchFuncType[CallableAnyType, FunctionKindTypeVar],
		        /
		) -> None:
			...


	InheritedDispatchTypeVar = t.TypeVar('InheritedDispatchTypeVar', bound=InheritedDispatchFuncType)

	class PartialedDispatchFuncType(t.Protocol[t.Callable[[P], R]]):
		def __call__(
				self,
				func: t.Callable[[P], R]
		) -> InheritedDispatchFuncType[t.Callable[[P], R], FunctionKindTypeVar]:
			...

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

	InstanceOrMethodListTypeVar = t.TypeVar(
		'InstanceOrMethodListTypeVar',
		bound=t.Union[CallableListType[StaticType], CallableListType[ClassType]]
	)

	ClassDictType = dict[str, t.Any]
	InheritedDispatchBases = tuple[type, ...]

# Some magic strings that are used by the decorator
STATIC: FunctionKindType = 'static'
CLASS: FunctionKindType = 'cls'
INSTANCE: FunctionKindType = 'instance'

GENERIC_FUNCDICT_PRIVATE = '_generic_annotated_funcdict'
GENERIC_FUNCDICT_PUBLIC = 'generic_annotated_funcdict'
BOUND_FUNCDICT_PRIVATE = '_bound_annotated_funcdict'
BOUND_FUNCDICT_PUBLIC = 'bound_annotated_funcdict'


class InheritedDispatchWarning(UserWarning):
	"""
	Warning that is raised in situations where the user has done things that will not make the module fail,
	but may not result in the outcome the user is expecting.
	"""
	pass


def raise_for_invalid_hint(hint: t.Any, /) -> None:
	"""
	Raises an exception if a user has attempted to use an invalid type hint
	on a function decorated with the @inherited_dispatch decorator.
	"""

	if hint in (t.Final, t.Literal):
		raise TypeError(f"inherited_dispatch decorator won't work with unparameterised {hint}")

	if isinstance(hint, t.TypeVar) and (not hint.__bound__ or isinstance(hint.__bound__, t.ForwardRef)):
		raise TypeError(
			"inherited_dispatch decorator won't work with an unbound typing.TypeVar, "
			"or a TypeVar bound to a forward reference. "
			"Either use a different type hint, or bind the TypeVar to instances of "
			"(and instances of subclasses of) a class that has already been instantiated."
		)

	if getattr(hint, '_is_protocol', False) and (not getattr(hint, '_is_runtime_protocol', False)):
		raise TypeError(
			"inherited_dispatch decorator won't work with classes that inherit from typing.Protocol, "
			"unless they are decorated with @typing.runtime_checkable."
		)

	if (origin := getattr(hint, '__origin__', None)) in (t.Generic, t.ClassVar, t.Literal):
		raise TypeError(f"inherited_dispatch decorator won't work with {origin}")


def paramaterised_hint_detected(hint: AnnotationType, /) -> bool:
	"""
	Function to detect if the user is using a parameterised type hint when annotating a function.
	A parameterised type hint will not cause the programme to crash, but may not result in the outcome the user expects.
	"""

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


class FunctionInfo(t.NamedTuple):
	"""Return type for the check_function_compatibility function below."""
	kind: FunctionKindType
	variable_args_no: bool


def check_function_compatibility(
		func: CallableAnyType,
		/,
		static_method: bool,
		class_method: bool,
		base_impl: bool
) -> FunctionInfo:

	"""
	Conducts a series of tests on a function decorated with the inherited_dispatch decorator,
	to determine whether the function is compatible with the design of this module.
	"""

	ann = getattr(func, '__annotations__', None)

	if ann is None:
		raise TypeError("Can't use this decorator on callables that don't have type annotations")

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
		kind = STATIC
	elif class_method:
		if first_arg != CLASS:
			raise TypeError(
				"The first argument for a class method should always be called 'cls' when using this decorator"
			)
		kind = CLASS
	else:
		if first_arg != 'self':
			raise TypeError(
				"The first argument for an instance method should always be called 'self' when using this decorator. "
				"If the method is a static method or a class method, "
				"it must be explicitly labelled as such in every implementation."
			)
		kind = INSTANCE

	# Check all arguments in the function have been annotated (we don't care about the return argument)

	ann_no, param_no, return_annotated = len(ann), len(params), ('return' in ann)

	if (
			(return_annotated and (ann_no < (param_no + 1)))
			or ((not return_annotated) and (ann_no < param_no))
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
	elif variable_param_types:
		raise TypeError("Only the base implementation of a function is allowed to have variable numbers of arguments.")

	return FunctionInfo(kind, bool(variable_param_types))


@t.overload
def inherited_dispatch(
		func: None = None,
		base_impl: bool = False,
		static_method: bool = False,
		class_method: bool = False,
		suppress_warnings: bool = False
) -> PartialedDispatchFuncType[CallableAnyType]:
	"""Signature if the decorator has been used without arguments"""
	...


@t.overload
def inherited_dispatch(
		func: t.Callable[[P], R],
		base_impl: bool = False,
		static_method: bool = False,
		class_method: bool = False,
		suppress_warnings: bool = False
) -> InheritedDispatchFuncType[t.Callable[[P], R], FunctionKindTypeVar]:
	"""Signature if the decorator has been used with arguments"""
	...


def inherited_dispatch(
		func: t.Optional[t.Callable[[P], R]] = None,
		*,
		base_impl: bool = False,
		static_method: bool = False,
		class_method: bool = False,
		suppress_warnings: bool = False
) -> t.Union[
	InheritedDispatchFuncType[t.Callable[[P], R], FunctionKindTypeVar],
	PartialedDispatchFuncType[CallableAnyType]
]:

	"""
	Decorator to mark out the functions that you want to operate by the rules of inheritable singledispatch.
	See docstring at the top of this module for rules on usage.
	"""

	# If func is None, the decorator has been called with arguments
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

	kind, variable_args_no = check_function_compatibility(
		func,
		static_method=static_method,
		class_method=class_method,
		base_impl=base_impl
	)

	func.__dict__.update({
		'kind':                 kind,
		'variable_args_no':     variable_args_no,
		'inherited_dispatch':   True,
		'base_impl':            base_impl,
		'suppress_warnings':    suppress_warnings,
		'checked':              False
	})

	return func


@cache
def arg_compares_equal(annotated_type: AnnotationType, arg_type: type, /) -> bool:
	"""
	Determine whether an argument that has been supplied "matches" a type-annotation in a given implementation.
	Sometimes easier said than done.
	"""

	# If the types are identical or an implementation has typing.Any, return True
	if annotated_type in (arg_type, t.Any):
		return True

	if type(annotated_type) is type:
		return False

	# Bound TypeVars (unbound TypeVars won't have made it this far).
	if isinstance(annotated_type, t.TypeVar):
		bound: AnnotationType = annotated_type.__bound__
		return typevar_compares_equal(bound, arg_type)

	# typing.Protocol (ones that aren't runtime-checkable won't have made it this far)
	# also unparameterised collections.abc
	if isinstance(annotated_type, (t.Protocol, ABCMeta)):
		return isinstance(arg_type, annotated_type)

	# Parameterised AND unparameterised typing.Callable
	if isinstance(annotated_type, t.Callable) and callable(arg_type):
		return True

	# typing.Union
	if type(annotated_type) is t._UnionGenericAlias:
		return any(arg_compares_equal(ann_type, arg_type) for ann_type in annotated_type.__args__)

	# Parameterised collections.abc, parameterised builtins, parameterised typing aliases (List, Tuple, etc.)
	if isinstance(annotated_type, (GenericAlias, t._GenericAlias)):
		return arg_compares_equal(annotated_type.__origin__, arg_type)

	# typing.Final
	if type(annotated_type) is t._SpecialForm and annotated_type.__origin__ is t.Final:
		return arg_compares_equal(annotated_type.__args__[0], arg_type)

	return False


@cache
def typevar_compares_equal(typevar_binding: AnnotationType, arg_type: type, /) -> bool:
	# If the TypeVar is bound to a single type, typing.Protocol or ABCMeta
	if isinstance(typevar_binding, (type, t.Protocol, ABCMeta)):
		return issubclass(arg_type, typevar_binding)

	# If the TypeVar is bound to a typing.Union
	if type(typevar_binding) is t._UnionGenericAlias:
		return any(typevar_compares_equal(b, arg_type) for b in typevar_binding.__args__)

	if isinstance(typevar_binding, t.Callable) and callable(arg_type):
		return True

	if isinstance(typevar_binding, (GenericAlias, t._GenericAlias)):
		return typevar_compares_equal(typevar_binding.__origin__, arg_type)

	raise NotImplementedError(
		f"Don't know how to compare TypeVar bound to {typevar_binding} with {arg_type}"
	)


@cache
def all_args_compare_equal(annotated_types: t.Sequence[AnnotationType], arg_types: t.Sequence[type], /) -> bool:
	"""
	Comparisons for base implementation are done differently --
	for any other implementations, we need an exact match in numbers of arguments
	"""

	if len(annotated_types) != len(arg_types):
		return False
	try:
		return all(
			arg_compares_equal(annotated_type, arg_type)
			for annotated_type, arg_type in zip(annotated_types, arg_types)
		)
	except NotImplementedError as err:
		raise NotImplementedError(
			f"ERROR while attempting to compare function with annotations "
			f"'{annotated_types}' and arguments with '{arg_types}'"
		) from err


class CallableFunctionList(list):
	"""
	Container object for a list of function implementations.

	When a user attempts to access a multiple-dispatch method, they are in fact returned an instance of this class.
	When they attempt to call the method they have accessed,
	this class is able to ascertain which implementation is required from the arguments that have been supplied.
	"""

	def __init__(
			self,
			data: t.Optional[list[InheritedDispatchFuncType[CallableAnyType, FunctionKindTypeVar]]] = None,
			name: str = ''
	) -> None:

		super().__init__(data if data is not None else [])
		self.__name__ = name
		self.base_impl: t.Optional[InheritedDispatchFuncType[CallableAnyType, FunctionKindTypeVar]] = None
		self.base_impl_args_len = 0
		self.variable_args_no = False
		self.kind: t.Optional[FunctionKindType] = None
		self.checked = False

	def extend_flag_unchecked(self, iterable: ImplementationIterator[FunctionKindTypeVar], /) -> None:
		self.extend(iterable)
		self.checked = False

	@cache
	def find_impl(self, /, *argtypes: type) -> InheritedDispatchFuncType[CallableAnyType, FunctionKindTypeVar]:
		"""Determine which implementation is most suitable for this set of arguments."""

		try:
			return next(func for func in self if all_args_compare_equal(func.type_hints, argtypes))
		except StopIteration:
			args_no, exception_needed = len(argtypes), False
			if (
					(self.variable_args_no and args_no < self.base_impl_args_len)
					or (not self.variable_args_no and args_no != self.base_impl_args_len)
			):
				raise TypeError(
					f"No registered implementation exists for that number of arguments and those types of arguments. "
					f"The base implementation of function {self.__name__} takes "
					f"{'at least' if self.variable_args_no else 'exactly'} "
					f"{self.base_impl_args_len} argument(s). {args_no} arguments were supplied."
				) from None
			return self.base_impl

	def _check_function_impls(self, /) -> None:
		"""This function should only be accessed by InheritedDispatchMeta"""
		g = groupby(func.kind for func in self)
		if not (next(g, True) and not next(g, False)):
			raise TypeError(
				"Multiple implementations of one function, all with the same name, must be of the same kind. "
				"E.g. they must all be class methods, instance methods or static methods"
			)

		base_impls = [func for func in self if func.base_impl]

		if len(base_impls) != 1:
			raise TypeError(
				f"Error in function {self.__name__}: "
				"There must be exactly one function implementation registered as a fallback implementation."
			)

		if (base_impl := base_impls[0]) != self.base_impl:
			self.base_impl = base_impl

			ann = base_impl.__annotations__
			ann_len = len(ann)

			self.base_impl_args_len = (ann_len - 1) if 'return' in ann else ann_len
			self.variable_args_no = base_impl.variable_args_no
			self.kind = base_impl.kind

		for func in filter(lambda func: not func.checked, self):
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
			# as the type of nearly all classes is type
			if self.kind == CLASS and type(type_hints[0]) is GenericAlias:
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

	def __hash__(self, /) -> int: return id(self)

	def __str__(self, /) -> str: return f'Implementation list for function "{self.__name__}"'

	def __repr__(self, /) -> str: return f'Implementation list for function "{self.__name__}": {super().__repr__()}'

	def __mul__(self, other: int, /) -> t.NoReturn:
		raise NotImplementedError("Can't multiply a list of function implementations")

	def __imul__(self, other: int, /) -> t.NoReturn:
		raise NotImplementedError("Can't multiply a list of function implementations")

	def __rmul__(self, other: int, /) -> t.NoReturn:
		raise NotImplementedError("Can't multiply a list of function implementations")

	def sort(self, *args, **kwargs) -> t.NoReturn:
		raise NotImplementedError("Cannot sort a list of function implementations")


class NiceReprPartial:
	"""
	A wrapper around functools.partial so that CallableFunctionList
	doesn't have a horrible __repr__ when it's partialised
	"""

	__slots__ = 'original_callable', 'partialised_callable', 'calling_object', '__name__'

	def __init__(
			self,
			original_callable: CallableFunctionList,
			calling_object: t.Annotated[t.Any, "Either a class or an instance of a class"]
	) -> None:

		self.__name__ = original_callable.__name__
		self.original_callable = original_callable
		self.calling_object = calling_object
		self.partialised_callable = partial(original_callable, calling_object)

	def __iter__(self, /) -> t.Iterable[InheritedDispatchFuncType[CallableAnyType, FunctionKindTypeVar]]:
		"""Iterates through the functions in the CallableFunctionList contained within"""
		return self.original_callable.__iter__()

	def __getitem__(self, index: int, /) -> InheritedDispatchFuncType[CallableAnyType, FunctionKindTypeVar]:
		"""
		Makes individual implementations in the CallableFunctionList
		directly accessible through indexing the NiceReprPartial
		"""

		return self.original_callable.__getitem__(index)

	def __repr__(self, /) -> str:
		return f'{repr(self.original_callable)}. Bound as a method to {self.calling_object}.'

	def __call__(self, *args: t.Any, **kwargs: t.Any) -> t.Any:
		return self.partialised_callable(*args, **kwargs)

	def print_argtypes(self, /) -> None:
		"""Mainly here for debugging purposes: will print out all of the implementations of the function within."""
		for tup in [tuple(t.get_type_hints(func).values()) for func in self.original_callable]:
			print(tup)


class _DictAllowingDuplicates(dict):
	"""
	A dict that will stash multiple copies of a function with the same name
	if they're marked with the @inherited_dispatch decorator
	"""

	def __init__(self, data: t.Optional[ClassDictType] = None) -> None:
		data = data if data else {}
		data[GENERIC_FUNCDICT_PRIVATE] = {}
		super().__init__(data)

	def __setitem__(self, key: str, value: t.Any, /) -> None:
		"""
		Treat the vast majority of objects normally,
		but implement special treatment for functions marked with the inherited_dispatch decorator.
		"""

		if not hasattr(value, 'inherited_dispatch'):
			return super().__setitem__(key, value)

		if key not in (funcdict := self[GENERIC_FUNCDICT_PRIVATE]):
			funcdict[key] = CallableFunctionList(name=key)
		funcdict[key].append(value)


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

		return some_class.__dict__[GENERIC_FUNCDICT_PRIVATE]

	def __new__(
			metacls: type[InheritedDispatchMeta],
			cls_name: str,
			cls_bases: InheritedDispatchBases,
			cls_dict: _DictAllowingDuplicates
	) -> InheritedDispatchMeta:

		new_dict = dict(cls_dict)

		# Add some read-only properties for convenient access to the annotated_funcdicts.
		for public_name, private_name in (
				(GENERIC_FUNCDICT_PUBLIC, GENERIC_FUNCDICT_PRIVATE),
				(BOUND_FUNCDICT_PUBLIC, BOUND_FUNCDICT_PRIVATE)
		):
			new_dict[public_name] = property(fget=lambda x: x.__class__.__dict__[private_name])

		new_dict['_funcdict_checked'] = False

		# Add an __init__ function or alter an __init__ function in any classes using this metaclass.
		def added_init_fragment(self) -> None:
			cls = self.__class__
			generic_funcdict = cls.__dict__[GENERIC_FUNCDICT_PRIVATE]
			funcdict_checked = cls._funcdict_checked

			if not funcdict_checked:
				for func_list in filter(lambda x: not x.checked, generic_funcdict.values()):
					func_list._check_function_impls()

			for k, v in generic_funcdict.items():
				if v.kind == INSTANCE:
					setattr(self, k, NiceReprPartial(v, self))
				elif not funcdict_checked:
					setattr(cls, k, (v if v.kind == STATIC else NiceReprPartial(v, cls)))

			cls._funcdict_checked = True

		new_init_required, __init__ = False, None  # It makes the type-checker happy...

		try:
			old_init = new_dict['__init__']
		except KeyError:
			if not cls_bases:
				added_init_fragment.__name__ = '__init__'
				__init__ = added_init_fragment
				new_init_required = True
		else:
			if (not cls_bases) or not all((
					'super().__init__(' in getsource(old_init),
					'super' in (old_init_co_names := old_init.__code__.co_names),
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

		return t.cast(InheritedDispatchMeta, super().__new__(metacls, cls_name, cls_bases, new_dict))

	def __init__(
			cls: InheritedDispatchMeta,
			cls_name: str,
			cls_bases: InheritedDispatchBases,
			cls_dict: ClassDictType
	) -> None:

		# Merge all the funcdicts in the class's __mro__.

		for k, v in chain.from_iterable(
				cls.get_funcdict(parent).items() for parent in cls.__mro__ if isinstance(parent, InheritedDispatchMeta)
		):
			if k not in cls.get_funcdict(cls):
				cls.get_funcdict(cls)[k] = CallableFunctionList(name=k)
			cls.get_funcdict(cls)[k].extend_flag_unchecked(filter(lambda f: f not in cls.get_funcdict(cls)[k], v))

		super().__init__(cls_name, cls_bases, cls_dict)


class InheritedDispatchBase(metaclass=InheritedDispatchMeta):
	"""Base class provided for easy access to the metaclass"""
	pass
