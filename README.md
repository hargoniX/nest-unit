# `nest-unit`
An `IsTest` implementation for [`nest-core`](https://github.com/hargonix/nest-core).

The main contribution of this implementation consists of three things:
1. The `IsTest UnitTest` instance which allows us to write assertion based unit tests.
2. The `assert t` macro. This is the main entry points for users of the library.
   It checks whether `t` holds and fails the test if it doesn't.
   In order to allow for `t : Prop` it performs a case distinction on the type of `t`:
   - if `t : Prop` it will look for an instance `Assertable (Proxy t)`
     and use that to check whether `t` holds
   - otherwise it will look for an instance `Assertable t` and use that
	 to check whether `t` holds
3. The `Assertable α` type class, it allows you to provide a way to test whether
   a property `(t : α)`  holds. If it doesn't use `assertFailure msg` to indicate
   failure. For examples, in particular how this can be used together with `Prop`,
   we refer to the very minimal implementation itself.

Here is a full example on the usage of `assert` and the library in general:
```lean
import NestCore
import NestUnit

open Nest.Core
open Nest.Unit

def tests : TestTree := [nest|
  group "Self Tests"
    group "Basic"
      test "succeeds on true" : UnitTest := do
        assert true
      test "fails on false (expected to fail)" : UnitTest := do
        assert false
    group "Instances"
      test "positive eq" : UnitTest := do
        assert "hello" = "hello"
      test "negative eq (expected to fail)" : UnitTest := do
        assert "hello" = "world"
]

def main : IO UInt32 := Nest.Core.defaultMain tests
```
