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
class Assertable (α : Sort u) where
  /--
  Checks the property that is passed to it and calls `assertFailure` if appropriate. 
  Note that `assert` is the intended API for this function
  -/
  assertThis : α → UnitM Unit

export Assertable (assertThis)

/--
Used by the `assert` macro as entrypoints to `asserThis` to provide the
source position in case of failure.
-/
def runAssertThis [Assertable α] (assertion : α) (pos : (String × Lean.Position)) : UnitM Unit := do
  try
    assertThis assertion
  catch e =>
    throw <| { e with pos := some pos }

instance : Assertable Bool where
  assertThis b := do
    unless b do
      assertFailure "boolean was false"

instance [Assertable t] : Assertable (List t) where
  assertThis xs := xs.forM assertThis

instance [Assertable t] : Assertable (Array t) where
  assertThis xs := xs.forM assertThis

structure Proxy (p : Sort u) where

instance : CoeDep (Sort u) p (Proxy p) where
  coe := Proxy.mk

-- TODO: PrintableProp style stuff like with SlimCheck
instance (priority := low) {p : Prop} [Decidable p] : Assertable (Proxy p) where
  assertThis _ := do
    unless decide p do
      assertFailure "property was not true"

instance [DecidableEq α] [Repr α] {x y : α} : Assertable (Proxy (x = y)) where
  assertThis _ := do
    unless x = y do
      assertFailure s!"equality failed, left: '{repr x}', right: '{repr y}'"

instance [DecidableEq α] [Repr α] {x y : α} : Assertable (Proxy (x ≠ y)) where
  assertThis _ := do
    unless x ≠ y do
      assertFailure s!"inequality failed, left: '{repr x}', right: '{repr y}'"

instance [LT α] [DecidableRel (· < · : α → α → Prop)] [Repr α] {x y : α} : Assertable (Proxy (x < y)) where
  assertThis _ := do
    unless x < y do
      assertFailure s!"less than failed, left: '{repr x}', right: '{repr y}'"

instance [LT α] [DecidableRel (· < · : α → α → Prop)] [Repr α] {x y : α} : Assertable (Proxy (x > y)) where
  assertThis _ := do
    unless x > y do
      assertFailure s!"greater than failed, left: '{repr x}', right: '{repr y}'"

instance [LE α] [DecidableRel (· ≤ · : α → α → Prop)] [Repr α] {x y : α} : Assertable (Proxy (x ≤ y)) where
  assertThis _ := do
    unless x ≤ y do
      assertFailure s!"less than or equal failed, left: '{repr x}', right: '{repr y}'"

instance [LE α] [DecidableRel (· ≥ · : α → α → Prop)] [Repr α] {x y : α} : Assertable (Proxy (x ≥ y)) where
  assertThis _ := do
    unless x ≥ y do
      assertFailure s!"greater than or equal failed, left: '{repr x}', right: '{repr y}'"

open Lean in
/--
The intended entrypoint to run assertions. The parameter here being the assertion.
In order to allow for `t : Prop` it performs a case distinction on the type of `t`:
- if `t : Prop` it will look for an instance `Assertable (Proxy t)`
  and use that to check whether `t` holds
- otherwise it will look for an instance `Assertable t` and use that
  to check whether `t` holds
-/
scoped elab "assert " t:term : term => do
  let assertion ← Elab.Term.elabTerm t none
  let assertionTyp ← Meta.inferType assertion
  let some pos := (← getRef).getPos?
    | throwError "no source info"
  let pos := (← getFileMap).toPosition pos
  let posExpr := toExpr (← getFileName, pos)

  if assertionTyp.isProp then
    let proxyAssertion ← Meta.mkAppOptM ``Nest.Unit.Proxy.mk #[assertion]
    return ← Meta.mkAppM ``Nest.Unit.runAssertThis #[proxyAssertion, posExpr]
  else
    return ← Meta.mkAppM ``Nest.Unit.runAssertThis #[assertion, posExpr]

end Unit
end Nest
