import NestCore
import NestUnit

open Nest.Core
open Nest.Unit

def fileRes (path : System.FilePath) (mode : IO.FS.Mode) : ResourceSpec IO.FS.Handle where
  get := IO.FS.Handle.mk path mode
  release handle := handle.flush
  description := s!"A file handle to {path}"

def tests : TestTree := [nest|
  group "Self Tests"
    group "Basic"
      test "succeeds on true" : UnitTest := do
        assert true
      test "fails on false (expected to fail)" : UnitTest := do
        assert false
    group "Instances"
      test "positive list" : UnitTest := do
        assert [[true, true], [true, true]]
      test "negative list (expected to fail)" : UnitTest := do
        assert [[true, true], [true, false]]
      test "positive array" : UnitTest := do
        assert #[#[true, true], #[true, true]]
      test "negative array (expected to fail)" : UnitTest := do
        assert #[#[true, true], #[true, false]]
      test "positive eq" : UnitTest := do
        assert "hello" = "hello"
      test "negative eq (expected to fail)" : UnitTest := do
        assert "hello" = "world"
      test "positive neq" : UnitTest := do
        assert "hello" ≠ "world"
      test "negative neq (expected to fail)" : UnitTest := do
        assert "hello" ≠ "hello"
      test "positive lt" : UnitTest := do
        assert (2 : Nat) < 10
      test "negative lt (expected to fail)" : UnitTest := do
        assert (10 : Nat) < 2
      test "positive gt" : UnitTest := do
        assert (10 : Nat) > 2
      test "negative gt (expected to fail)" : UnitTest := do
        assert (2 : Nat) > 10
      test "positive le" : UnitTest := do
        assert (2 : Nat) ≤ 2
        assert (2 : Nat) ≤ 3
      test "negative le (expected to fail)" : UnitTest := do
        assert (3 : Nat) ≤ 2
      test "positive ge" : UnitTest := do
        assert (2 : Nat) ≥ 2
        assert (3 : Nat) ≥ 2
      test "negative ge (expected to fail)" : UnitTest := do
        assert (2 : Nat) ≥ 3
    group "Resource based"
      with resource fileRes "/dev/zero" .read as res
        test "assertion 3" : UnitTest := do
          let data ← res.read 12
          assert data.size = 12
    group "Option based"
      with options fun x => x.insert `Hello "foo"
        with options as x
          test "assertion 4" : UnitTest := do
            assert x.contains `Hello
]

def main : IO UInt32 := Nest.Core.defaultMain tests
