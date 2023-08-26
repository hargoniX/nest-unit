import NestCore
import NestUnit

open Nest.Core
open Nest.Unit

def fileRes (path : System.FilePath) (mode : IO.FS.Mode) : ResourceSpec IO.FS.Handle where
  get := IO.FS.Handle.mk path mode
  release handle := handle.flush
  description := s!"A file handle to {path}"

def tests : TestTree := [nest|
  group "Main Tests"
    group "Group 1"
      test "assertion 1" : UnitTest := do
        let x : Nat := 12
        let y := x + x + x
        let z := 3 * x
        assert y ≠ z -- supposed to fail
      test "assertion 2" : UnitTest := do
        assert true
    group "Group 2"
      with resource fileRes "/dev/zero" .read as res
        test "assertion 3" : UnitTest := do
          let data ← res.read 12
          assert data.size = 12
    group "Group 3"
      with options fun x => x.insert `Hello "foo"
        with options as x
          test "assertion 4" : UnitTest := do
            assert x.contains `Hello
]

def main : IO Unit := Nest.Core.defaultMain tests
