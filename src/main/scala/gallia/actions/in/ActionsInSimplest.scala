package gallia.actions.in

import gallia._

// ===========================================================================
case class RawContentU(input: InputUrlLike) extends ActionIUd with TodoV0 {
  def _meta: Cls = Cls.Content
  def atomiu = AtomsIX._RawContentU(input) }

// ===========================================================================
case class RawLinesZ(input: InputUrlLike, inMemoryMode: Boolean) extends ActionIZd with TodoV0 {
  def _meta: Cls = Cls.Line
  def atomiz = AtomsIX._RawLinesZ(input, inMemoryMode) }

// ===========================================================================
