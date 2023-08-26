import NestCore
import Lean.Elab

namespace Nest
namespace Unit

/--
Thrown by `UnitM` when an assertion failed.
-/
structure AssertionError where
  /--
  The failure message, this is supposed to be set by the asseertion itself
  -/
  message : String
  /--
  The position where it failed, this will be set by the framework through
  the `assert` macro.
  -/
  pos : Option (String × Lean.Position) := none
deriving Repr

/--
A monad to run assertions in.
-/
abbrev UnitM (α : Type) := ExceptT AssertionError IO α

/--
The type for which this library implements `Nest.Core.IsTest`, it is `UnitM Unit`
so we can run arbitrary assertions inside of it.
-/
abbrev UnitTest := UnitM Unit

instance : Nest.Core.IsTest UnitTest where
  run _ assertion := do
    let result ← ExceptT.run assertion
    match result with
    | .ok .. =>
      return {
        outcome := .success,
        description := "all assertions succeeded",
        shortDescription := "all assertions succeeded",
        details := ""
      }
    | .error err =>
      let (filename, ⟨line, column⟩) := err.pos.get!
      return {
        outcome := .failure .generic
        description := s!"assertion failure: {err.message}",
        shortDescription := s!"assertion failure: {err.message}",
        details := s!"file: '{filename}', line: {line}, col: {column}"
      }

/--
Used by `Assertable` implementations to indicate failure. 
-/
def assertFailure (str : String) : UnitM Unit := do
  throw <| { message := str }

/--
Marks a type as a property that can be checked as an assertion
-/
class Assertable {α : Sort u} (x : α) where
  /--
  Checks the property that is passed to it and calls `assertFailure` if appropriate. 
  Note that `assert` is the intended API for this function
  -/
  assertThis : UnitM Unit

export Assertable (assertThis)

instance : Assertable (b : Bool) where
  assertThis := do
    unless b do
      assertFailure "boolean was false"

instance [forall (x : t), Assertable x] (xs : List t) : Assertable xs where
  assertThis := xs.forM (assertThis ·)

instance [forall (x : t), Assertable x] (xs : Array t) : Assertable xs where
  assertThis := xs.forM (assertThis ·)

-- TODO: PrintableProp style stuff like with SlimCheck
instance (priority := low) {p : Prop} [Decidable p] : Assertable p where
  assertThis := do
    unless decide p do
      assertFailure "property was not true"

instance [DecidableEq α] [Repr α] {x y : α} : Assertable (x = y) where
  assertThis := do
    unless x = y do
      assertFailure s!"equality failed, left: '{repr x}', right: '{repr y}'"

instance [DecidableEq α] [Repr α] {x y : α} : Assertable (x ≠ y) where
  assertThis := do
    unless x ≠ y do
      assertFailure s!"inequality failed, left: '{repr x}', right: '{repr y}'"

instance [LT α] [DecidableRel (· < · : α → α → Prop)] [Repr α] {x y : α} : Assertable (x < y) where
  assertThis := do
    unless x < y do
      assertFailure s!"less than failed, left: '{repr x}', right: '{repr y}'"

instance [LT α] [DecidableRel (· < · : α → α → Prop)] [Repr α] {x y : α} : Assertable (x > y) where
  assertThis := do
    unless x > y do
      assertFailure s!"greater than failed, left: '{repr x}', right: '{repr y}'"

instance [LE α] [DecidableRel (· ≤ · : α → α → Prop)] [Repr α] {x y : α} : Assertable (x ≤ y) where
  assertThis := do
    unless x ≤ y do
      assertFailure s!"less than or equal failed, left: '{repr x}', right: '{repr y}'"

instance [LE α] [DecidableRel (· ≥ · : α → α → Prop)] [Repr α] {x y : α} : Assertable (x ≥ y) where
  assertThis := do
    unless x ≥ y do
      assertFailure s!"greater than or equal failed, left: '{repr x}', right: '{repr y}'"

open Lean in
scoped elab "pos%" : term => do
  let some pos := (← getRef).getPos?
    | throwError "no source info"
  let pos := (← getFileMap).toPosition pos
  let posExpr := toExpr (← getFileName, pos)
  return posExpr

def assert (t : α) [Assertable t] (pos : (String × Lean.Position) := by exact pos%) : UnitM Unit :=
  try
    assertThis t
  catch e =>
    throw <| { e with pos := some pos }

end Unit
end Nest
