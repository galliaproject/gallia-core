package gallia
package actions
package in

// ===========================================================================
case class RawContentU(input: InputUrlLike) extends ActionIU01 with TodoV0 {
  def _meta: Cls = Cls.Content
  def atomiu = AtomsIX._RawContentU(input) }

// ===========================================================================
case class RawLinesZ(input: InputUrlLike, inMemoryMode: Boolean) extends ActionIZ01 with TodoV0 {
  def _meta: Cls = Cls.Line
  def atomiz = AtomsIX._RawLinesZ(input, inMemoryMode) }

// ===========================================================================
