package gallia
package actions

import aptus.Anything_
import atoms._UWrapper

// ===========================================================================
trait CanForceAs1[T] { def forceAs(key: Key): T } // TODO: t201208111414 - macro for copy boilerplate...; t210124100009 - better naming

  // ===========================================================================
  // TODO: reuse Rename?
  case class AsRename private (target: Ren) extends ActionZZa with CanForceAs1[AsRename] {
    override def forceAs(key: Key): AsRename = AsRename(target.assert(!_.isActual /* see 201203143058 */).from ~> key)

    // ---------------------------------------------------------------------------
    def  vldt(c: Cls): Errs    = Nil//TODO
    def _meta(c: Cls): Cls     = c.pipeIf(target.isActual)(_.rename(target))
    def atomzzs      : AtomZZs = target.actualOpt.map(_Rename).map(_UWrapper).toSeq
  }

  // ===========================================================================
  trait CanForceAs2[T] extends CanForceAs1[T] {
    val asOpt: Option[Key]
    @aptus.nonfinl val defaultKey: Key = _count_all

    final def as: Key = asOpt.getOrElse(defaultKey)
  }


// ===========================================================================
