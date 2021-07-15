"""

### RULES ###


(1) All classes wishing to access "inherited multiple dispatch features" must use InheritedDispatchMeta as the metaclass.

(2)	InheritedDispatchBase is provided as a convenient base class that can be used (see examples.py),
	but it is not essential to inherit from InheritedDispatchBase.
	You could very easily write your own base class, if it uses InheritedDispatchMeta as its metaclass.

(3) You must use the @inherited_dispatch decorator on all functions
	you want to be able to access "inherited multiple dispatch" features.

(4) Methods decorated with @inherited_dispatch must have all arguments type-annotated.
	(This includes type-annotating 'self', 'cls', etc. --
	it will be cleaner if you do from __future__ import annotations.)

(5) There must be exactly one registered implementation of a multiple-dispatch function
	that has the argument base_impl=True passed to the @inherited_dispatch decorator.
	This informs the module that this implementation should be seen as the "fallback" implementation.

(6) The types of the arguments passed to a function must exactly match the types given in an implementation annotation
	in order for that implementation to be returned. Anything else, and the fallback implementation is returned.

	There are two exceptions to this: if you annotate an argument with typing.TypeVar and "bind" the TypeVar to a class,
	any subclasses of the bound class will "match" the annotation.
	TypeVars may be bound to typing.Union as well as to a single class.
	However, they may not be bound to forward references.

	The other exception is typing.Any, which will match with any argument type.

(7) Be aware that if you use flexible/ambiguous types such as typing.TypeVar, typing.Any or typing.Union,
	this module makes no guarantee that it will find the "best" implementation
	that "most closely" matches the types of the arguments provided.
	It will return the first implementation it finds that matches all of the argument types --
	it's on you if you're not specific enough!

(8) Allowed "special types" for annotations are:
	- typing.Annotated                                          (Resolves to a normal type when typing.get_type_hints() is called on it)
	- typing.Any                                                (CAN'T be used with isinstance())
	- typing.Union                                              (CAN'T be used with isinstance())
	- Paramaterised generics (list[str] etc.)                   (CAN'T be used with isinstance())
	- Parameterised typing.Final                                (CAN'T be used with isinstance())

	- typing.TypeVar, if it's been bound to a type.             (CAN be used with isinstance(), but only if it's bound to an actual type - typing.Union won't cut it.)
	- typing.Protocol if it's marked as being runtime_checkable (CAN be used with isinstance())
	- Paramaterised collections.abc (MutableSequence[str] etc)  (CAN be used with isinstance(), but only if NOT parameterised)
	- typing.Callable                                           (CAN be used with isinstance(), but only if NOT parameterised)

(9) Using the | operator between types in annotations is disallowed (use typing.Union instead).
	Other features introduced in python 3.10 may not work either; this has only been tested on python 3.9.

(10) Disallowed types for annotations are the following.
	(Implementation for some of these would be possible but would probably slow it down/complicate it more.)

	- typing.Literal
	- typing.Protocol if it's not marked as being runtime_checkable
	- typing.Generic
	- typing.ClassVar
	- unparamaterised typing.Final

(11) classmethods and staticmethods are possible,
	but you cannot use the standard builtin @classmethod and @staticmethod decorators.
	Instead, mark the methods as classmethods or staticmethods by passing class_method=True or static_method=True
	into the @inherited_dispatch decorator.

(12) All implementations of any one method must be the same 'kind' of method
	-- they must all either be instance methods, all static methods, or all class methods.

(13) You can't use the @inherited_dispatch decorator on dunder methods (that would be crazy!)

(14) Only a base implementation of a function is allowed to have a variable number of arguments.
	All other implementations of a function must have a fixed number of positional and keyword arguments.

(15) The base implementation may have a variable number of positional arguments,
	or a variable number of keyword arguments. However, it may not have both.

(16) If the base implementation does not have a variable number of arguments,
	and is passed the incorrect number of arguments, the function call will fail,
	as with a normal function call supplied the incorrect arguments.

(16) __slots__ cannot be defined in any classes using "inherited multiple dispatch" features.
	This module will raise a BadClassDefinition error if you do so, due to unpredictable outcomes.
	While it would not necessarily result in an exception otherwise,
	it is unlikely that defining __slots__ would have any of the usual benefits
	in terms of speed optimisations, immutability of fields, etc.

(17) As far as I am aware, classes that use the InheritedDispatchMeta metaclass can be freely combined,
	using multiple inheritance,	with classes that do not use a metaclass.
	However, you cannot combine classes using InheritedDispatchMeta with classes using a different metaclass
	(leads to a nasty error message about "metaclass conflict").

"""

from __future__ import annotations

import typing as t

from functools import partial, cache, update_wrapper
from abc import ABCMeta, ABC, abstractmethod
from types import GenericAlias
from inspect import signature, Parameter, getsource, getmro, isfunction
from itertools import chain, groupby
from dataclasses import dataclass
from copy import deepcopy
from warnings import warn

# A function can be of three kinds and three kinds only: instance, static, and class
FunctionKind = t.Literal['instance', 'static', 'class']
_FunctionKindTypeVar = t.TypeVar('_FunctionKindTypeVar', bound=t.Optional[FunctionKind])

# Aliases to the three types so we don't have to use magic strings everywhere
_STATIC: FunctionKind = 'static'
_INSTANCE: FunctionKind = 'instance'
_CLASS: FunctionKind = 'class'

# Two other magic strings that are used a lot
_SELF, _CLS = 'self', 'cls'


@dataclass
class DispatchableFuncInfo(t.Generic[_FunctionKindTypeVar]):
	"""
	Metadata class for multiple-dispatch methods.
	Kept in a separate class rather than added to the function's dictionary directly,
	as we don't want this metadata to be accidentally overridden by a user in the future.
	"""

	__slots__ = 'base_impl', 'suppress_warnings', 'checked', 'variable_args_no', 'kind', 'type_hints'

	base_impl: bool
	suppress_warnings: bool
	checked: bool
	variable_args_no: bool
	kind: _FunctionKindTypeVar
	type_hints: t.Optional[t.Sequence[type]]


# Only used for class _P below
_T = t.TypeVar('_T', covariant=True)


class _P(t.Protocol[_T]):
	"""
	Dummy class solely here to infer the type of a Protocol with a parameter.
	At the moment it has the same type as typing.Protocol, but this is marked as an implementation detail.
	So using isinstance() on that directly is unsafe.
	"""
	pass


# The types of a lot of the things in the typing module are marked as implementation details.
# To avoid using implementation details directly, we infer all the types of these annotations at the top, like so:

_UNPARAMATERISED_PROTOCOL_TYPE = type(t.Protocol)               # typing._ProtocolMeta
_PARAMATERISED_PROTOCOL_TYPE = type(_P)                         # At the moment also typing._ProtocolMeta,
																# but presumably not guaranteed that it will always be the same

_UNPARAMATERISED_UNION_TYPE = type(t.Union)                     # typing._SpecialForm
_PARAMATERISED_UNION_TYPE = type(t.Union[int, str])             # typing._UnionGenericAlias

_UNPARAMETERISED_CALLABLE_TYPE = type(t.Callable)               # typing._CallableType
_PARAMETERISED_CALLABLE_TYPE = type(t.Callable[[int], str])     # typing._CallableType

_UNPARAMATERISED_TYPING_ALIAS = type(t.List)                    # typing._SpecialGenericAlias
_PARAMATERISED_TYPING_ALIAS = type(t.List[str])                 # typing._GenericAlias

_UNPARAMATERISED_TYPING_FINAL_TYPE = type(t.Final)              # typing._SpecialForm
_PARAMATERISED_TYPING_FINAL_TYPE = type(t.Final[str])           # typing._GenericAlias

_CallableTypeVar = t.TypeVar('_CallableTypeVar', bound=t.Callable)

# Type-checking stuff that doesn't affect performance of the module at runtime.
if t.TYPE_CHECKING:
	# All the acceptable forms of annotations that may be added to a function decorated with @inherited_dispatch.
	AnnotationType = t.Union[
		type,
		_PARAMATERISED_UNION_TYPE,
		_PARAMATERISED_PROTOCOL_TYPE,
		_UNPARAMETERISED_CALLABLE_TYPE,
		_PARAMETERISED_CALLABLE_TYPE,
		_UNPARAMATERISED_TYPING_ALIAS,
		_PARAMATERISED_TYPING_ALIAS,
		_PARAMATERISED_TYPING_FINAL_TYPE,
		t.TypeVar,
		GenericAlias,
		ABCMeta
	]

	class PartialedDispatchFuncType(t.Protocol[_CallableTypeVar, _FunctionKindTypeVar]):
		"""
		When called with arguments, the @inherited_dispatch decorator returns a partialised version of itself.
		Said partialised version is a callable that looks like this.
		This type is defined as a subclass of typing.Protocol so that it can be paramaterised.
		"""
		def __call__(self, func: _CallableTypeVar) -> InheritedDispatchFuncType[_CallableTypeVar, _FunctionKindTypeVar]:
			...

	ClassDictType = dict[str, t.Any]
	InheritedDispatchBases = tuple[type, ...]
	AnyList = list[t.Any]

	LiteralFalse = t.Literal[False]
	LiteralTrue = t.Literal[True]

	StaticLiteral = t.Literal['static']
	InstanceLiteral = t.Literal['instance']
	ClassLiteral = t.Literal['class']

	# noinspection PyTypeChecker
	G = t.TypeVar('G', bound='GenericDispatcher')

	@t.overload
	def inherited_dispatch(
			func: None = None,
			base_impl: bool = False,
			static_method: LiteralFalse = False,
			class_method: LiteralFalse = False,
			suppress_warnings: bool = False
	) -> PartialedDispatchFuncType[_CallableAnyType, InstanceLiteral]:
		"""
		Signature for the decorator, applied to an instance method,
		in the eventuality that it returns a partialed form of itself.
		"""
		...


	@t.overload
	def inherited_dispatch(
			func: None = None,
			base_impl: bool = False,
			static_method: LiteralTrue = True,
			class_method: LiteralFalse = False,
			suppress_warnings: bool = False
	) -> PartialedDispatchFuncType[_CallableAnyType, StaticLiteral]:
		"""
		Signature for the decorator, applied to a staticmethod,
		in the eventuality that it returns a partialed form of itself.
		"""
		...


	@t.overload
	def inherited_dispatch(
			func: None = None,
			base_impl: bool = False,
			static_method: LiteralFalse = False,
			class_method: LiteralTrue = True,
			suppress_warnings: bool = False
	) -> PartialedDispatchFuncType[_CallableAnyType, ClassLiteral]:
		"""
		Signature for the decorator, applied to a classmethod,
		in the eventuality that it returns a partialed form of itself.
		"""
		...


	@t.overload
	def inherited_dispatch(
			func: _CallableTypeVar,
			base_impl: bool = False,
			static_method: LiteralFalse = False,
			class_method: LiteralFalse = False,
			suppress_warnings: bool = False
	) -> InheritedDispatchFuncType[_CallableTypeVar, InstanceLiteral]:
		"""Signature for when the decorator is applied to instance methods"""
		...


	@t.overload
	def inherited_dispatch(
			func: _CallableTypeVar,
			base_impl: bool = False,
			static_method: LiteralTrue = True,
			class_method: LiteralFalse = False,
			suppress_warnings: bool = False
	) -> InheritedDispatchFuncType[_CallableTypeVar, StaticLiteral]:
		"""Signature for when the decorator is applied static methods"""
		...


	@t.overload
	def inherited_dispatch(
			func: _CallableTypeVar,
			base_impl: bool = False,
			static_method: LiteralFalse = False,
			class_method: LiteralTrue = True,
			suppress_warnings: bool = False
	) -> InheritedDispatchFuncType[_CallableTypeVar, ClassLiteral]:
		"""Signature for when teh decorator is applied to class methods"""
		...


# This has to be accessible at runtime as it's used in class definitions
_CallableAnyType = t.Callable[..., t.Any]


# Some more magic strings
_UNBOUND_FUNCDICT_PRIVATE = '_generic_annotated_funcdict'
_UNBOUND_FUNCDICT_PUBLIC = 'generic_annotated_funcdict'
_BOUND_FUNCDICT_PRIVATE = '_bound_annotated_funcdict'
_BOUND_FUNCDICT_PUBLIC = 'bound_annotated_funcdict'

_INHERITED_DISPATCH_FUNC_INFO = 'inherited_dispatch_func_info'
__INIT__ = '__init__'
_IS_PROTOCOL = '_is_protocol'


# Some more specific exceptions for the implementation of this module, mostly subclasses of TypeError.


class InheritedDispatchWarning(UserWarning):
	"""
	Warning that is raised in situations where the user has done things that will not make the module fail,
	but may not result in the outcome the user is expecting.
	"""
	pass


class InvalidDecoratorUse(TypeError):
	"""Exception raised if a user has attempted to use the multiple_dispatch decorator on an invalid function."""
	pass


class NoGoodImplementation(TypeError):
	"""Exception raised if there is no good implementation of a function for the given arguments supplied."""
	pass


class BadClassDefinition(TypeError):
	"""
	Raised if a user attempts to define a class that uses InheritedDispatchMeta as a metaclass
	but has features that will cause the metaclass not to work as expected.
	"""
	pass


class AmbiguityError(BadClassDefinition):
	"""Exception raised when more than one function definition matches the types of the argument supplied."""
	pass


class IncomparableAnnotation(TypeError):
	"""Raised when the module encounters an annotation it doesn't know how to compare to a type."""
	pass


@t.runtime_checkable
class InheritedDispatchFuncType(t.Protocol[_CallableTypeVar, _FunctionKindTypeVar]):
	"""
	An inherited-dispatch function is defined as a function with a name,
	annotations, and a bundle of extra metadata attached.
	"""

	__call__: _CallableTypeVar
	__annotations__: dict[str, t.Any]
	__name__: str
	inherited_dispatch_func_info: DispatchableFuncInfo[_FunctionKindTypeVar]


def raise_for_invalid_hint(hint: t.Any, /) -> None:
	"""
	Raises an exception if a user has attempted to use an invalid type hint
	on a function decorated with the @inherited_dispatch decorator.

	To avoid using implementation details in the typing module,
	this function errs on the side of explicitness and extra checks
	before determining the kind of type-hint we're dealing with.
	"""

	# Only checking for unparamaterised Literal/Generic
	# Paramaterised Literal/Generic are explicitly checked later down
	if hint in {t.Literal, t.Generic}:
		raise InvalidDecoratorUse(
			f"inherited_dispatch decorator won't work with unparameterised typing.Literal or typing.Generic"
		)

	# Only checking for unparamaterised Final - paramaterised is okay
	if isinstance(hint, _UNPARAMATERISED_TYPING_FINAL_TYPE) and not getattr(hint, '__args__', None):
		raise InvalidDecoratorUse(f"inherited_dispatch decorator won't work with unparamaterised typing.Final")

	# Only checking for unbound TypeVars or TypeVars that are bound to forward references.
	# TypeVars that are bound, and not bound to forward references, are okay.
	if isinstance(hint, t.TypeVar):
		if not (bound := hint.__bound__) or isinstance(bound, t.ForwardRef):
			raise InvalidDecoratorUse(
				"inherited_dispatch decorator won't work with an unbound typing.TypeVar, "
				"or a TypeVar bound to a forward reference. "
				"Either use a different type hint, or bind the TypeVar to instances of "
				"(and instances of subclasses of) a class that has already been instantiated."
			)

	# Unparamaterised Protocols are not okay.
	if (
			isinstance(hint, _UNPARAMATERISED_PROTOCOL_TYPE)
			and getattr(hint, _IS_PROTOCOL, False)
			and not any(getattr(hint, x, None) for x in {'__parameters__', '__annotations__'})
	):
		raise InvalidDecoratorUse(
			"You cannot use typing.Protocol with no parameters in conjunction with this decorator."
			"If you wish to indicate that any type should match, use typing.Any."
		)

	# Paramaterised protocols are only okay if they are marked with the @runtime_checkable decorator.
	if (
			isinstance(hint, _PARAMATERISED_PROTOCOL_TYPE)
			and getattr(hint, _IS_PROTOCOL, False)
			and (not getattr(hint, '_is_runtime_protocol', False))
	):
		raise InvalidDecoratorUse(
			"inherited_dispatch decorator won't work with classes that inherit from typing.Protocol, "
			"unless they are decorated with @typing.runtime_checkable."
		)

	# Generics, ClassVars and paramaterised Literals are not okay.
	if (origin := getattr(hint, '__origin__', None)) in {t.Generic, t.ClassVar, t.Literal}:
		raise InvalidDecoratorUse(f"inherited_dispatch decorator won't work with {origin}")


def paramaterised_hint_detected(hint: AnnotationType, /) -> bool:
	"""
	Function to detect if the user is using a parameterised type hint when annotating a function.
	A parameterised type hint will not cause the programme to crash, but may not result in the outcome the user expects.
	This function returns True if the hint supplied is a paramaterised hint, and False if not.
	"""

	# Unparameterised abcs have type of ABCMeta, but we don't need to worry about those in this function.
	# This function is only for warning the user that we won't take care of *paramaterised* collection-types;
	# unparamaterised collection types are still fine.

	if isinstance(hint, (
			# E.g. directly parameterised builtins:                 isinstance(list[str], types.GenericAlias) -> True
			GenericAlias,

			# E.g. typing aliases for builtins or collections.abc:  isinstance(typing.List[str], typing._GenericAlias) -> True
			#                                                       isinstance(typing.MutableSequence[str], typing._GenericAlias) -> True
			_PARAMATERISED_TYPING_ALIAS
	)):
		return True

	# Detect whether typing.Callable has been parameterised or not -- no need to emit a warning if not.
	if isinstance(hint, t.Callable):
		return bool(getattr(hint, '__args__', False))

	return False


class PartialFunctionInfo(t.NamedTuple):
	"""Return type for the check_function_compatibility function below."""
	kind: FunctionKind
	variable_args_no: bool


def check_function_compatibility(
		func: _CallableAnyType,
		/,
		static_method: bool,
		class_method: bool,
		base_impl: bool,
		suppress_warnings: bool = False
) -> PartialFunctionInfo:

	"""
	Conducts a series of tests on a function decorated with the inherited_dispatch decorator,
	to determine whether the function is compatible with the design of this module.
	"""

	# The function has to have type annotations.
	if (ann := getattr(func, '__annotations__', None)) is None:
		raise InvalidDecoratorUse("Can't use this decorator on callables that don't have type annotations")

	# New syntax introduced in python 3.10 is not allowed -- has to work on 3.9.
	if any('|' in annot for annot in ann.values()):
		raise InvalidDecoratorUse(
			"You cannot use the | operator between types for inherited_dispatch annotations. "
			"Use typing.Union instead."
		)

	# Check if the decorator has been called with invalid arguments (can't be both an instance or static method)
	if static_method and class_method:
		raise InvalidDecoratorUse("A function cannot be both a class method and a static method.")

	# Check the decorator is being used on a callable object
	if not isfunction(func):
		raise InvalidDecoratorUse("This decorator can only be used on functions")

	# Check the user isn't being insane and trying to use this decorator on a dunder method.
	if func.__name__.startswith('__'):
		raise InvalidDecoratorUse("Can't use the inherited_dispatch decorator on dunder methods")

	# Check the name of the first argument (depends on whether it's a static, instance or class method)
	params = signature(func).parameters
	first_arg = tuple(params.keys())[0]

	# The method for creating staticmethods and classmethods is different when using this decorator.
	# As a result, it makes sense to check that the user hasn't made an error
	# when naming the first argument in the method.
	if static_method:
		if not suppress_warnings and first_arg in {_SELF, _CLS}:
			warn(
				'A static method does not have access to the attributes of the instance or the class. '
				'As such, you should usually not call the first argument of a staticmethod "self" or "cls".'
				'Use the keyword argument suppress_warnings=True in the @inherited_dispatch decorator '
				'to turn off this warning.'
				,
				InheritedDispatchWarning
			)
		kind = _STATIC
	elif class_method:
		if not suppress_warnings and first_arg != _CLS:
			warn(
				"The first argument for a class method should usually be called 'cls'. "
				"Use the keyword argument suppress_warnings=True in the @inherited_dispatch decorator "
				"to turn off this warning.",
				InheritedDispatchWarning
			)
		kind = _CLASS
	else:
		if not suppress_warnings and first_arg != _SELF:
			warn(
				"The first argument for an instance method should usually be called 'self'. "
				"Remember that if the method is a static method or a class method, "
				"it must be explicitly labelled as such in every implementation when using inherited multiple-dispatch."
				"Use the keyword argument suppress_warnings=True in the @inherited_dispatch decorator "
				"to turn off this warning.",
				InheritedDispatchWarning
			)
		kind = _INSTANCE

	# Check all arguments in the function have been annotated (we don't care about the return argument)
	ann_no, param_no, return_annotated = len(ann), len(params), ('return' in ann)

	if (
			(return_annotated and (ann_no < (param_no + 1)))
			or ((not return_annotated) and (ann_no < param_no))
	):
		raise InvalidDecoratorUse(
			"You must type-hint every single argument when using the inherited_dispatch decorator, "
			"or it will not work correctly."
		)

	# Check to see if there are variable numbers of arguments (allowed in a base_impl, otherwise not.
	# And a base_impl can't have variable numbers of positional arguments *and* variable numbers of keyword-arguments.
	var_param_types = {Parameter.VAR_POSITIONAL, Parameter.VAR_KEYWORD}.intersection(p.kind for p in params.values())

	if base_impl:
		if len(var_param_types) == 2:
			raise InvalidDecoratorUse(
				"The base implementation of a function may have variable numbers "
				"of either key-word or positional arguments, "
				"but may not have variable positional arguments *and* variable keyword arguments"
			)
	elif var_param_types:
		raise InvalidDecoratorUse(
			"Only the base implementation of a function is allowed to have variable numbers of arguments."
		)

	return PartialFunctionInfo(kind, bool(var_param_types))


def inherited_dispatch(
		func: t.Optional[_CallableTypeVar] = None,
		*,
		base_impl: bool = False,
		static_method: bool = False,
		class_method: bool = False,
		suppress_warnings: bool = False
) -> t.Union[
	InheritedDispatchFuncType[_CallableTypeVar, _FunctionKindTypeVar],
	PartialedDispatchFuncType[_CallableAnyType, _FunctionKindTypeVar],
	t.NoReturn
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
		base_impl=base_impl,
		suppress_warnings=suppress_warnings
	)

	setattr(
		func,
		_INHERITED_DISPATCH_FUNC_INFO,
		DispatchableFuncInfo(
			base_impl=base_impl,
			variable_args_no=variable_args_no,
			suppress_warnings=suppress_warnings,
			checked=False,
			kind=kind,
			type_hints=None
		)
	)

	return func


@cache
def arg_compares_equal(annotated_type: AnnotationType, arg_type: type, /) -> bool:
	"""
	Determine whether an argument that has been supplied "matches" a type-annotation in a given implementation.
	Sometimes easier said than done.
	"""

	# If the types are identical or an implementation has typing.Any, return True
	if annotated_type in {arg_type, t.Any}:
		return True

	# We do NOT want to use isinstance() here because isinstance(collections.abc.Sequence, type) -> True
	if type(annotated_type) is type:
		return False

	# Bound TypeVars (unbound TypeVars won't have made it this far).
	if isinstance(annotated_type, t.TypeVar):
		bound: AnnotationType = annotated_type.__bound__
		return typevar_compares_equal(bound, arg_type)

	# typing.Protocol (ones that aren't runtime-checkable won't have made it this far)
	# also unparameterised collections.abc

	# typing.Protocol is a regular class, so it's safe to use isinstance(annotated type, Protocol here).
	# It's only the Protocol metaclass that's marked as being an implementation detail.
	# Including PARAMATERISED_PROTOCOL_TYPE just for saftey
	if isinstance(annotated_type, (t.Protocol, _PARAMATERISED_PROTOCOL_TYPE, ABCMeta, t.Callable)):
		return isinstance(arg_type, annotated_type)

	# Paramaterised typing.Union (unparamaterised won't have got this far)
	if isinstance(annotated_type, _PARAMATERISED_UNION_TYPE):
		return any(arg_compares_equal(ann_type, arg_type) for ann_type in annotated_type.__args__)

	# Parameterised collections.abc, parameterised builtins, parameterised typing aliases (List, Tuple, etc.)
	if isinstance(annotated_type, (GenericAlias, _PARAMATERISED_TYPING_ALIAS)):
		return arg_compares_equal(annotated_type.__origin__, arg_type)

	# Paramaterised typing.Final (unparamaterised won't have got this far)
	if (
			isinstance(annotated_type, _PARAMATERISED_TYPING_FINAL_TYPE)
			and (getattr(annotated_type, '__origin__', None) is t.Final)
	):
		return arg_compares_equal(annotated_type.__args__[0], arg_type)

	return False


@cache
def typevar_compares_equal(typevar_binding: AnnotationType, arg_type: type, /) -> bool:
	"""
	Determine whether an argument that has been supplied "matches" a TypeVar bound to a type or a Union of types.
	Sometimes easier said than done.
	"""

	# If the TypeVar is bound to a single type, paramaterised typing.Protocol or an abstract class from collections.abc
	if isinstance(typevar_binding, (type, t.Protocol, ABCMeta)):
		return issubclass(arg_type, typevar_binding)

	# If the TypeVar is bound to a typing.Union
	if isinstance(typevar_binding, _PARAMATERISED_UNION_TYPE):
		return any(typevar_compares_equal(b, arg_type) for b in typevar_binding.__args__)

	# If the TypeVar is bound to typing.Callable (paramaterised or unparamaterised)
	if isinstance(typevar_binding, t.Callable) and callable(arg_type):
		return True

	# If the TypeVar is bound to a paramaterised generic type (list[str], List[str], etc.)
	if isinstance(typevar_binding, (GenericAlias, _PARAMATERISED_TYPING_ALIAS)):
		return typevar_compares_equal(typevar_binding.__origin__, arg_type)

	raise IncomparableAnnotation(
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
	except IncomparableAnnotation as err:
		raise IncomparableAnnotation(
			f"ERROR while attempting to compare function with annotations "
			f"'{annotated_types}' and arguments with '{arg_types}'"
		) from err


def all_equal(seq: t.Iterable) -> bool:
	"""From the 'recipes' section in the itertools documentation"""
	g = groupby(seq)
	return next(g, True) and not next(g, False)


def _check_function_impls(funclist: UnboundMethodDispatcher, /) -> None:
	"""This function should only be accessed by the FuncDict class"""

	# All methods of the same name must be either instance methods, static methods, or classmethods.
	if not all_equal(func.inherited_dispatch_func_info.kind for func in funclist):
		raise BadClassDefinition(
			"Multiple implementations of one function, all with the same name, must be of the same kind. "
			"E.g. they must all be class methods, instance methods or static methods"
		)

	# We have to do this check here (and not when we initialise the UnboundMethodDispatcher),
	# as the __mro__ of the class is traversed top-down,
	# and the base implementation is often found at the bottom of the __mro__.
	base_impls = [func for func in funclist if func.inherited_dispatch_func_info.base_impl]

	if len(base_impls) != 1:
		raise BadClassDefinition(
			f"Error in function {funclist.__name__}: "
			"There must be exactly one function implementation registered as a fallback implementation."
		)

	funclist.check_base_impl(base_impls[0])

	# Convert the annotations into types, add it to the function,
	for func in (func for func in funclist if not func.inherited_dispatch_func_info.checked):
		try:
			type_hints_dict = t.get_type_hints(func)
		except Exception as err:
			raise InvalidDecoratorUse(
				"When using @inherited_dispatch, all annotations must be resolvable to a type "
				"using the typing.get_type_hints function."
			) from err

		if 'return' in type_hints_dict:
			del type_hints_dict['return']

		type_hints = list(type_hints_dict.values())

		for hint in type_hints:
			try:
				raise_for_invalid_hint(hint)
			except InvalidDecoratorUse as err:
				raise InvalidDecoratorUse(
					f'Invalid annotation in function {func} with annotations {type_hints_dict}'
				) from err

		# Type-checking the first argument for a classmethod works differently
		# as the type of nearly all classes is type
		func_info = func.inherited_dispatch_func_info

		if func_info.kind == _CLASS and isinstance(type_hints[0], GenericAlias):
			type_hints[0] = type_hints[0].__args__[0]

		# Emit an InheritedDispatchWarning if Generic types or GenericAliases are being used.
		if (not func_info.suppress_warnings) and any(paramaterised_hint_detected(hint) for hint in type_hints):
			warn(
				"inherited_dispatch: Warning regarding function {func.__name__}: "
				"It is permitted to use parameterised alias types "
				"(e.g. list[str], MutableMapping[str, int]) in combination with this decorator. "
				"However, be aware that, for example, "
				"in a situation where an argument is annotated as being of type list[str], "
				"the algorithm will *only* check that the argument inputted is a list -- "
				"it will not check whether the items in the list are all of type str."
				"The only parameterised annotations that do not raise this warning "
				"are typing.Union and typing.Annotated."
				"Use the suppress_warnings=True argument in the decorator to suppress this warning.",
				InheritedDispatchWarning
			)

		func_info.type_hints = tuple(type_hints)
		func_info.checked = True
	funclist.has_been_checked()


class GenericDispatcher(t.Generic[_FunctionKindTypeVar], ABC):
	"""

	A container object for a list of function implementations.

	When a user attempts to access a multiple-dispatch method, they are in fact returned an instance of this class.
	When they attempt to call the method they have accessed,
	this class is able to ascertain which implementation is required from the arguments that have been supplied.

	Dispatchers come in two flavours:
	- UnboundMethodDispatchers, which hold lists of functions that are not bound to any one instance or class.
	- BoundMethodDispatchers, which hold lists of functions that have been bound to a specific instance and class.

	"""

	@abstractmethod
	def __init__(self, *args: t.Any, **kwargs: t.Any) -> None: ...

	@abstractmethod
	def __call__(self, *args: t.Any, **kwargs: t.Any) -> t.Any: ...

	@abstractmethod
	def __str__(self, /) -> str: ...

	@abstractmethod
	def __repr__(self, /) -> str: ...

	@abstractmethod
	def __iter__(self, /) -> t.Iterator[InheritedDispatchFuncType[_CallableAnyType, _FunctionKindTypeVar]]: ...

	@abstractmethod
	def __getitem__(self, item: int, /) -> InheritedDispatchFuncType[_CallableAnyType, _FunctionKindTypeVar]: ...

	@abstractmethod
	def __contains__(self, item: InheritedDispatchFuncType[_CallableAnyType, _FunctionKindTypeVar], /) -> bool: ...

	def __copy__(self, /) -> t.NoReturn:
		"""Shallow copy returns a new object with a reference to the same list, which is never what we want"""

		cls = type(self)

		raise NotImplementedError(
			f"Can't copy an instance of {cls}; use {cls}.deepcopy() instead"
		)

	def copy(self, /) -> t.NoReturn:
		"""Alias to self.__copy__()"""
		return self.__copy__()

	def deepcopy(self: G, /) -> G:
		"""
		A deepcopy returns an identical instance,
		but with a new list of the same functions rather than a reference to the same list.
		"""
		return deepcopy(self)


class UnboundMethodDispatcher(GenericDispatcher[_FunctionKindTypeVar]):
	"""
	Container object for a list of function implementations.

		"""

	__slots__ = '__name__', 'base_impl', 'base_impl_args_len', 'variable_args_no', 'kind', 'checked', 'funclist'

	def __init__(self, /, *, name: str = '') -> None:
		super().__init__()
		self.funclist: list[InheritedDispatchFuncType[_CallableAnyType, _FunctionKindTypeVar]] = []
		self.__name__ = name

		# We can't insert the base implementation in the __init__ method, as we traverse the __mro__ top-down
		# The base implementation will usually be at the bottom of the inheritance chain.
		self.base_impl: t.Optional[InheritedDispatchFuncType[_CallableAnyType, _FunctionKindTypeVar]] = None
		self.base_impl_args_len = 0
		self.variable_args_no = False
		self.kind: t.Optional[FunctionKind] = None
		self.checked = False

	# Can't cache this -- we don't know if the function we're calling will be pure or not.
	def __call__(self, *args: t.Any, **kwargs: t.Any) -> t.Any:
		if self.kind == _CLASS:
			impl = self.find_impl(args[0], *map(type, chain(args[1:], kwargs.values())))
		else:
			impl = self.find_impl(*map(type, chain(args, kwargs.values())))
		return impl(*args, **kwargs)

	def __str__(self, /) -> str:
		return f'Implementation list for function "{self.__name__}"'

	def __repr__(self, /) -> str:
		return f'Implementation list for function "{self.__name__}": {self.funclist}'

	def __iter__(self) -> t.Iterator[InheritedDispatchFuncType[_CallableAnyType, _FunctionKindTypeVar]]:
		return self.funclist.__iter__()

	def __getitem__(self, item: int) -> InheritedDispatchFuncType[_CallableAnyType, _FunctionKindTypeVar]:
		return self.funclist.__getitem__(item)

	def __contains__(self, item: InheritedDispatchFuncType[_CallableAnyType, _FunctionKindTypeVar]) -> bool:
		return self.funclist.__contains__(item)

	def has_been_checked(self) -> None:
		"""
		Set the `checked` flag to True so that each new instance of a class
		doesn't need to do the check in its __init__ method
		"""
		self.checked = True

	def merge_with_other(self, other: UnboundMethodDispatcher[_FunctionKindTypeVar], /) -> None:
		"""Add new implementations of the function to the list, and change the `checked` flag to False."""
		self.funclist.extend(filter(lambda f: f not in self, other))
		self.checked = False

	def check_base_impl(self, impl: InheritedDispatchFuncType[_CallableAnyType, _FunctionKindTypeVar], /) -> None:
		"""
		Registers a base implementation of the function if one hasn't already been registered,
		and copies the metadata from the base implementation to the CallableList.
		"""

		if impl is not self.base_impl:
			self.base_impl = impl

			ann = impl.__annotations__
			ann_len = len(ann)

			self.base_impl_args_len = (ann_len - 1) if 'return' in ann else ann_len

			base_impl_func_info = impl.inherited_dispatch_func_info

			self.variable_args_no = base_impl_func_info.variable_args_no
			self.kind = base_impl_func_info.kind

	@cache
	def find_impl(self, /, *argtypes: type) -> InheritedDispatchFuncType[_CallableAnyType, _FunctionKindTypeVar]:
		"""
		Cycle through the list of function implementations until we find one that matches the supplied argument types.
		As soon as we find one that does, we stop looking and return it.

		If none of the implementations fits, we look at the registered base implementation.
		If the number of arguments in the base implementation is the same as the number supplied, we return that one.
		If not, we raise an exception.
		"""

		try:
			return next(
				func for func in self if all_args_compare_equal(func.inherited_dispatch_func_info.type_hints, argtypes)
			)
		except StopIteration:
			args_no, exception_needed = len(argtypes), False
			if (
					(self.variable_args_no and args_no < self.base_impl_args_len)
					or (not self.variable_args_no and args_no != self.base_impl_args_len)
			):
				raise NoGoodImplementation(
					f"No registered implementation exists for that number of arguments and those types of arguments. "
					f"The base implementation of function {self.__name__} takes "
					f"{'at least' if self.variable_args_no else 'exactly'} "
					f"{self.base_impl_args_len} argument(s). {args_no} arguments were supplied."
				) from None
			return self.base_impl

	def add_impl(self, item: InheritedDispatchFuncType[_CallableAnyType, _FunctionKindTypeVar]) -> None:
		"""Add a new implementation of the method"""
		return self.funclist.append(item)


class BoundMethodDispatcher(GenericDispatcher[_FunctionKindTypeVar]):
	"""
	A wrapper around functools.partial so that UnboundMethodDispatcher
	doesn't have a horrible __repr__ when it's partialised
	"""

	__slots__ = 'unbound_dispatcher', 'partialised_callable', 'calling_object', '__name__', 'unbound_funclist'

	def __init__(
			self,
			unbound_dispatcher: UnboundMethodDispatcher,
			calling_object: t.Annotated[t.Any, "Either a class or an instance of a class"]
	) -> None:
		super().__init__()
		self.__name__ = unbound_dispatcher.__name__
		self.unbound_dispatcher = unbound_dispatcher
		self.unbound_funclist = unbound_dispatcher.funclist
		self.calling_object = calling_object
		self.partialised_callable = partial(unbound_dispatcher, calling_object)

	def __call__(self, *args: t.Any, **kwargs: t.Any) -> t.Any:
		return self.partialised_callable(*args, **kwargs)

	def __str__(self, /) -> str:
		return f'{self.unbound_dispatcher}. Bound as a method to {self.calling_object}.'

	def __repr__(self, /) -> str:
		return f'{repr(self.unbound_dispatcher)}. Bound as a method to {repr(self.calling_object)}.'

	def __iter__(self, /) -> t.Iterator[InheritedDispatchFuncType[_CallableAnyType, _FunctionKindTypeVar]]:
		"""Iterates through the functions in the UnboundMethodDispatcher contained within"""
		return self.unbound_funclist.__iter__()

	def __getitem__(self, index: int, /) -> InheritedDispatchFuncType[_CallableAnyType, _FunctionKindTypeVar]:
		"""
		Makes individual implementations in the UnboundMethodDispatcher
		directly accessible through indexing the BoundMethodDispatcher
		"""

		return self.unbound_funclist.__getitem__(index)

	def __contains__(self, item: InheritedDispatchFuncType[_CallableAnyType, _FunctionKindTypeVar], /) -> bool:
		return self.unbound_funclist.__contains__(item)

	def print_argtypes(self, /) -> None:
		"""Mainly here for debugging purposes: will print out all of the implementations of the function within."""
		for tup in [tuple(t.get_type_hints(func).values()) for func in self.unbound_dispatcher]:
			print(tup)


# We have to define this here as it needs to be defined below UnboundMethodDispatcher and BoundMethodDispatcher
# The values have to be t.Union[UnboundMethodDispatcher, BoundMethodDispatcher]
# as staticmethods are never actually bound to a class or instance.
_BoundFuncdict = dict[str, t.Union[UnboundMethodDispatcher, BoundMethodDispatcher]]


class UnboundFuncDict(dict[str, UnboundMethodDispatcher]):
	"""
	Class for holding multiple-dispatch methods for a class,
	that have not yet been bound to any one instance of that class.

	A light wrapper around a dict.
	"""

	__slots__ = 'checked'
	all_unbound_funcdicts: dict[str, UnboundFuncDict] = {}

	def __init__(
			self,
			cls_name: str,
			data: t.Optional[dict[str, UnboundMethodDispatcher]] = None
	) -> None:

		super().__init__(data if data is not None else {})
		self.checked = False
		self.all_unbound_funcdicts[cls_name] = self

	@classmethod
	def merge_traversing_mro(cls, some_class: InheritedDispatchMeta) -> None:
		"""Merge the dictionary with all other FuncDicts along a supplied __mro__"""

		funcdict, mro = cls.unbound_funcdict_of_class(some_class), getmro(some_class)

		func_name: str
		func_list: UnboundMethodDispatcher

		for func_name, func_list in chain.from_iterable(
				cls.unbound_funcdict_of_class(parent).items() for parent in mro
				if isinstance(parent, InheritedDispatchMeta)
		):
			# If a UnboundMethodDispatcher of that name already exists in the dictionary,
			# merge the existing list with the new one.
			#
			# Otherwise, deepcopy the supplied UnboundMethodDispatcher and add it to the dictionary.

			if func_name in funcdict:
				funcdict[func_name].merge_with_other(func_list)
			else:
				funcdict[func_name] = deepcopy(func_list)

	@classmethod
	def unbound_funcdict_of_class(cls, some_class: InheritedDispatchMeta) -> UnboundFuncDict:
		"""Enter a class, get back the class's funcdict"""
		return cls.all_unbound_funcdicts[some_class.__name__]

	def create_bound_funcdict(
			self,
			instance: InheritedDispatchMeta,
			instance_cls: InheritedDispatchMeta
	) -> _BoundFuncdict:

		"""
		Create a new dictionary of multiple-dispatch methods bound to an instance passed to this method.

		If the instance passed to this method is the first instance of a certain class,
		the classmethods will also be bound to the class in this method.
		"""

		bound_funcdict, checked = {}, self.checked

		if not checked:
			for func_list in filter(lambda x: not x.checked, self.values()):
				_check_function_impls(func_list)

		key: str
		val: UnboundMethodDispatcher
		func: t.Union[UnboundMethodDispatcher, BoundMethodDispatcher]

		for key, val in self.items():
			if val.kind == _INSTANCE:
				func = BoundMethodDispatcher(val, instance)
				setattr(instance, key, func)
			elif not checked:
				func = val if val.kind == _STATIC else BoundMethodDispatcher(val, instance_cls)
				setattr(instance_cls, key, func)
			else:
				func = getattr(instance_cls, key)

			bound_funcdict[key] = func

		self.checked = True
		return bound_funcdict


class _ClassDictAllowingDuplicates(dict[str, t.Any]):
	"""
	A dict that will stash multiple copies of a function with the same name
	if they're marked with the @inherited_dispatch decorator
	"""

	__slots__: t.Sequence[str] = tuple()

	def __init__(self, cls_name: str) -> None:
		super().__init__({
			_UNBOUND_FUNCDICT_PRIVATE: UnboundFuncDict(cls_name),
			_UNBOUND_FUNCDICT_PUBLIC: property(fget=cache(lambda inst: getattr(type(inst), _UNBOUND_FUNCDICT_PRIVATE))),
			# BOUND_FUNCDICT_PRIVATE deliberately not included, as it has to be bound to the *instance*, not the class.
			_BOUND_FUNCDICT_PUBLIC: property(fget=cache(lambda inst: getattr(inst, _BOUND_FUNCDICT_PRIVATE))),
		})

	def __setitem__(self, key: str, value: t.Any, /) -> None:
		"""
		Treat the vast majority of objects normally,
		but implement special treatment for functions marked with the inherited_dispatch decorator.
		"""

		if key == '__slots__':
			raise BadClassDefinition(
				"You cannot define __slots__ in any classes that use the InheritedDispatchMeta metaclass."
			)

		if (
				isinstance(value, InheritedDispatchFuncType)
				and isinstance(value.inherited_dispatch_func_info, DispatchableFuncInfo)
		):
			funcdict: UnboundFuncDict = self[_UNBOUND_FUNCDICT_PRIVATE]
			if key not in funcdict:
				funcdict[key] = UnboundMethodDispatcher(name=key)
			return funcdict[key].add_impl(value)

		return super().__setitem__(key, value)


class InheritedDispatchMeta(type):
	"""
	Metaclass that works with the @inheritable_dispatch decorator
	to allow for inheritable multiple-dispatch functions
	"""

	# Can't define __slots__ in a metaclass or we'll break Python.

	@classmethod
	def __prepare__(
			metacls: type[InheritedDispatchMeta],
			cls_name: str,
			cls_bases: InheritedDispatchBases
	) -> _ClassDictAllowingDuplicates:
		return _ClassDictAllowingDuplicates(cls_name)

	def __new__(
			metacls: type[InheritedDispatchMeta],
			cls_name: str,
			cls_bases: InheritedDispatchBases,
			cls_dict: _ClassDictAllowingDuplicates
	) -> InheritedDispatchMeta:

		# Add an __init__ function or alter an __init__ function in any classes using this metaclass.
		def _added_init_fragment(self) -> None:
			cls: InheritedDispatchMeta = type(self)
			generic_funcdict: UnboundFuncDict = getattr(cls, _UNBOUND_FUNCDICT_PRIVATE)
			setattr(self, _BOUND_FUNCDICT_PRIVATE, generic_funcdict.create_bound_funcdict(self, cls))

		__init__ = None

		try:
			old_init = cls_dict[__INIT__]
		except KeyError:
			if not cls_bases:
				_added_init_fragment.__name__ = __INIT__
				__init__ = _added_init_fragment
		else:
			if (not cls_bases) or not all((
					'super().__init__(' in getsource(old_init),
					'super' in (old_init_co_names := old_init.__code__.co_names),
					__INIT__ in old_init_co_names
			)):

				def __init__(self, *args: t.Any, **kwargs: t.Any) -> None:
					old_init(self, *args, **kwargs)
					_added_init_fragment(self)

				setattr(__init__, '__signature__', signature(old_init))
				update_wrapper(__init__, old_init)
		finally:
			if __init__ is not None:
				cls_dict[__INIT__] = __init__

		# noinspection PyTypeChecker
		return super().__new__(metacls, cls_name, cls_bases, dict(cls_dict))  # type: ignore

	def __init__(
			cls: InheritedDispatchMeta,
			cls_name: str,
			cls_bases: InheritedDispatchBases,
			cls_dict: ClassDictType
	) -> None:
		"""
		We need to merge all the funcdicts in the class's __mro__.
		This needs to be done in __init__ rather than __new__ as the __mro__ doesn't exist at the time __new__ is called.
		"""

		UnboundFuncDict.merge_traversing_mro(cls)
		super().__init__(cls_name, cls_bases, cls_dict)


class InheritedDispatchBase(metaclass=InheritedDispatchMeta):
	"""
	Base class provided for easy access to the metaclass.
	You are not allowed to define __slots__ in this class, or any other class using the InheritedDispatchMeta.
	"""
	pass
