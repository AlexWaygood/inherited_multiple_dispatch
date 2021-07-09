# An Python implementation of multiple-dispatch

Works with inheritance and all kinds of methods (instance methods, classmethods and staticmethods).

Lots of metaclass magic. Use with caution.

<br><br>

## What's this repo?
This repo is an implementation of multiple-dispatch in python using metaclasses.

## Should I use this module?
Probably not! Only the most basic tests have been done on it.
If you're looking for multiple-dispatch solutions, you should probably look at [`plum-dispatch`](https://github.com/wesselb/plum) or [`multipledispatch`](https://pypi.org/project/multipledispatch/#description), both of which are much more mature projects than this one.

This repo is more for demonstration purposes than anything else. However, it does have some features that neither of the above projects have.

## What does this do that the other projects don't?
`multipledispatch` is a really great implementation of what we're trying to do here.
It's really nice for the simplicity of the syntax, and the fact that there are so few dependencies.
However, it doesn't work particularly well with an object-oriented approach to programming, one of the most popular programming paradigms in Python.

`plum-dispatch` is much more featureful than multipledispatch.
It's still light on performance overhead, but works much better with class inheritance: a subclass can inherit a "fallback" implementation from a base class, which is a really valuable feature.
Unlike `multipledispatch`, `numpy` is listed as a requirement, but there are no other large dependencies in the package.

It also does other useful things that this implementation doesn't (yet) do:
1. Forward references are permitted when describing the argument-types that a function implementation should accept.
2. It raises a useful `AmbiguityLookupError` in situations where it's not clear which implementation would be the best fit.

However, both of these great projects have one important failing (in my view): neither of them support classmethods or staticmethods! This makes me sad :frowning_face:
This new implementation *does* (although you won't be able to use the usual decorators; instead, you'll need to pass special keyword arguments to the `@inherited_dispatch` decorator provided here).

N.B. This implementation is specifically for multiple-dispatch methods, *not* multiple-dispatch functions outside of classes.

## How does it work?
Lots of evil magic involving metaclasses, inheritance and dunder methods.

## How fast is it?
I haven't done rigorous performance benchmarks, but a basic test indicates that instantiation of class instances takes around 10x the time as a class using `functools.singledispatchmethod`. 
It is likely that other aspects of performance will also be slower, due to the use of metaclasses.

This is not an apples-to-apples comparison: this module provides a large number of features not available with `functools.singledispathmethod`.
However, you should think about whether performance might be an issue before using this module.

## How do I use it?
Rules for use are at the bottom of this README. Examples for how to use it can be seen in examples.py.

## How do I install it?
This is not on pip, as it is a single file with 0 external dependencies, so just copy it into your code base if you are crazy enough to use it. Python 3.9+ is required.
Please credit me in some form, ideally linking to this repo.

## Has this been rigorously tested?
No!!

<br><br>


# Rules for use


1. All classes wishing to access "inherited multiple dispatch features" must use `InheritedDispatchMeta` as the metaclass.

2. `InheritedDispatchBase` is provided as a convenient base class that can be used (see examples.py),
	but it is not essential to inherit from `InheritedDispatchBase`.
	You could very easily write your own base class, if it uses `InheritedDispatchMeta` as its metaclass.

3. You must use the `@inherited_dispatch decorator` on all functions
	you want to be able to access "inherited multiple dispatch" features.

4. Methods decorated with `@inherited_dispatch` must have all arguments type-annotated.
	(This includes type-annotating `self`, `cls`, etc. --
	it will be cleaner if you do `from __future__ import annotations`.)

5. There must be exactly one registered implementation of a multiple-dispatch function
	that has the argument `base_impl=True` passed to the `@inherited_dispatch` decorator.
	This informs the module that this implementation should be seen as the "fallback" implementation.

6. The types of the arguments passed to a function must exactly match the types given in an implementation annotation
	in order for that implementation to be returned. Anything else, and the fallback implementation is returned.

	There are two exceptions to this: if you annotate an argument with `typing.TypeVar` and "bind" the `TypeVar` to a class,
	any subclasses of the bound class will "match" the annotation.
	`TypeVar`s may be bound to `typing.Union` as well as to a single class.
	However, they may not be bound to forward references.

	The other exception is `typing.Any`, which will match with any argument type.

7. Be aware that if you use flexible/ambiguous types such as `typing.TypeVar`, `typing.Any` or `typing.Union`,
	this module makes no guarantee that it will find the "best" implementation
	that "most closely" matches the types of the arguments provided.
	It will return the first implementation it finds that matches all of the argument types --
	it's on you if you're not specific enough!

8. Allowed "special types" for annotations are:
    - `typing.Annotated` (Resolves to a normal type when `typing.get_type_hints()` is called on it)
    - `typing.Any`
    - `typing.Union`
    - Paramaterised generics (`list[str]` etc.)
    - Parameterised `typing.Final`
    - `typing.TypeVar`, if it's been bound to a type.
    - `typing.Protocol` if it's marked as being runtime_checkable
    - Paramaterised `collections.abc` (`MutableSequence[str]` etc)
    - `typing.Callable`

9. Using the `|` operator between types in annotations is disallowed (use `typing.Union` instead).
	Other features introduced in python 3.10 may not work either; this has only been tested on python 3.9.

10. Disallowed types for annotations are the following.
	(Implementation for some of these would be possible but would probably slow it down/complicate it more.)

	- `typing.Literal`
	- `typing.Protocol` if it's not marked as being runtime_checkable
	- `typing.Generic`
	- `typing.ClassVar`
	- unparamaterised `typing.Final`

11. classmethods and staticmethods are possible,
	but you cannot use the standard builtin `@classmethod` and `@staticmethod` decorators.
	Instead, mark the methods as classmethods or staticmethods by passing `class_method=True` or `static_method=True`
	into the `@inherited_dispatch` decorator.

12. All implementations of any one method must be the same 'kind' of method
	-- they must all either be instance methods, all static methods, or all class methods.

13. You can't use the `@inherited_dispatch` decorator on dunder methods (that would be crazy!)

14. Only a base implementation of a function is allowed to have a variable number of arguments.
	All other implementations of a function must have a fixed number of positional and keyword arguments.

15. The base implementation may have a variable number of positional arguments,
	or a variable number of keyword arguments. However, it may not have both.
