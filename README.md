# `nest-unit`
An `IsTest` implementation for [`nest-core`](https://github.com/hargonix/nest-core).

The main contribution of this implementation consists of three things:
1. The `IsTest UnitTest` instance which allows us to write assertion based unit tests.
2. The `assert` function. This is the main entry points for users of the library.
   It checks whether `t` holds and fails the test if it doesn't.
3. The `Assertable` type class, it allows you to provide a way to test whether
   a property holds. If it doesn't use `assertFailure msg` to indicate failure.
   For examples on implementations we refer to the rather minimal implementation of
   `nest-unit` itself.

Here is a full example on the usage of `assert` and the library in general:
```lean
import NestCore
import NestUnit

open Nest.Core
open Nest.Unit

group "Self Tests"
  group "Basic"
    test "succeeds on true" : UnitTest := do
      assert true
    test "fails on false (expected to fail)" : UnitTest := do
      assert false
  group "Instances"
    test "positive eq" : UnitTest := do
      assert <| "hello" = "hello"
    test "negative eq (expected to fail)" : UnitTest := do
      assert <| "hello" = "world"

def main : IO UInt32 := Nest.Core.defaultMain tests
```
