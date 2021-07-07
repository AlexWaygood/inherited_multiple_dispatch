from __future__ import annotations
from inherited_multiple_dispatch import InheritedDispatchBase, inherited_dispatch


class A(InheritedDispatchBase):
	def __init__(self) -> None:
		self.spam = 'A_Spam'

	@inherited_dispatch(base_impl=True)
	def handle(self: A, arg: object):
		return f'{self.spam}: Base implementation'

	@inherited_dispatch(base_impl=True, static_method=True)
	def a_static_method(arg1: str):
		return arg1


class B(A):
	def __init__(self) -> None:
		super().__init__()
		self.spam = 'B_Spam'

	@inherited_dispatch
	def handle(self: B, arg: int):
		return f'{self.spam}: Implementation for B and ints'

	@inherited_dispatch
	def handle(self: B, arg: str, arg2: str):
		return f'{self.spam}: Implementation for B, strs and strs'

	@inherited_dispatch(static_method=True)
	def a_static_method(arg1: str, arg2: int) -> str:
		return arg1 * arg2


class C(A):
	def __init__(self) -> None:
		super().__init__()
		self.spam = 'C_Spam'

	@inherited_dispatch
	def handle(self: C, arg: str):
		return f'{self.spam}: Implementation for C and strs'


class One(InheritedDispatchBase):
	blah = 'One_Spam'

	@inherited_dispatch(base_impl=True, class_method=True)
	def something_else(cls: type[One], *args: str):
		return f'{cls.blah}: Base implementation'


class Two(One):
	blah = 'Two_Spam'

	@inherited_dispatch(class_method=True)
	def something_else(cls: type[Two], arg1: str):
		return f'{cls.blah}: inherited version'


a, b, c, o, two = A(), B(), C(), One(), Two()

### INSTANCE METHOD TESTS

print(a.handle('hi'))           # prints "A_Spam: Base implementation"
print()
print(b.handle('hi'))           # prints "B_Spam: Base implementation"
print()
print(b.handle('hi', 'hi'))     # prints "B_Spam: Implementation fr B, strs and strs"
print()
print(b.handle(3))              # prints "C_Spam: Implementation for B and ints"
print()
print(c.handle('hi'))           # prints "C_Spam: Implementation for C and strs"
print()
print(c.handle(3))              # prints "C_Spam: Base implementation"

### CHECKING INHERITANCE

print()

try:
	print(o.handle('hi'))
except AttributeError as e:
	print(f'ERROR: {e}')
	print(
		'^This error is expected, and is supposed to happen -- '
		'although One inherits from InheritedDispatchBase, it is not part of the inheritance chain '
		'that has access to the base implementation of handle()'
	)

### CLASS METHOD TESTS

print()
print(o.something_else('hi'))       # prints "One_Spam: Base implementation"
print()
print(two.something_else('hi'))     # prints "Two_Spam: inherited version"
print()
print(two.something_else(3))        # prints "Two_Spam: Base implementation"

### STATIC METHOD TESTS

print()
print(a.a_static_method('hi'))      # prints "hi"
print()
print(b.a_static_method('hi'))      # prints "hi"
print()
print(b.a_static_method('hi', 2))   # prints "hihi"
print()
print(c.a_static_method('hi'))      # prints "hi"
print()

try:
	print(c.a_static_method('hi', 3))
except TypeError as e:
	print(f'ERROR: {e}')
	print(
		"^This error is expected, and is supposed to happen -- C inherits directly from A, "
		"so only has access to A's implementation of the method, not B's."
	)