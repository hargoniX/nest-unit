import NestCore
import Lean.Elab

namespace Nest
namespace Unit

structure AssertionError where
  message : String
  pos : Option (String × Lean.Position) := none
deriving Repr

abbrev UnitM (α : Type) := ExceptT AssertionError IO α
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

def assertFailure (str : String) : UnitM Unit := do
  throw <| { message := str }

class Assertable (α : Sort u) where
  assertThis : α → UnitM Unit

export Assertable (assertThis)

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
