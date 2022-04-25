package gallia
package actions

import target._
import atoms.AtomsUUReducer0._

// ===========================================================================
@NumberAbstraction
@deprecated("TODO: t210122151934 - use reducer rather") object ActionsUUReducer0 {
  //TODO: vals: ensure arrays + numerical

  // ---------------------------------------------------------------------------
  //FIXME: needs to return 0 if missing + allow renamings
  @deprecated("use reducer rather") case class ToSize(targets: TqRPathz) extends ActionUUc with TodoV1 { def hack(c: Cls) = targets.resolve(c).from1KFX
      def _meta (c: Cls): Cls    = hack(c).pipe(c.updateSoleInfo(_, Info.oneInt))
      def atomuu(c: Cls): AtomUU = _ToSize(hack(c)) }

    // ---------------------------------------------------------------------------
    @deprecated("use reducer rather") case class ToSum(targets: TqRPathz) extends ActionUUc with TodoV1 { def hack(c: Cls) = targets.resolve(c).from1KFX
      def _meta(c: Cls): Cls  =
        hack(c).pipe(c.field).forceNumericalType match {
          case BasicType._Int    => hack(c).pipe(c.updateSoleInfo(_, Info.oneInt))
          case BasicType._Double => hack(c).pipe(c.updateSoleInfo(_, Info.oneDouble))
          case _ => ??? /* TODO: though scheduled for removal */ }

      def atomuu(c: Cls): AtomUU =
        if (c.field(hack(c)).info1.isInt) _ToIntSum   (hack(c))
        else                              _ToDoubleSum(hack(c))
    }

    // ---------------------------------------------------------------------------
    @deprecated("use reducer rather") case class ToMean(targets: TqRPathz) extends ActionUUc with TodoV1 { def hack(c: Cls) = targets.resolve(c).from1KFX
      def _meta(c: Cls): Cls  = hack(c).pipe(c.updateSoleInfo(_, Info.oneDouble))

      def atomuu(c: Cls): AtomUU =
        if (c.field(hack(c)).info1.isInt) _ToIntMean   (hack(c))
        else                              _ToDoubleMean(hack(c))
    }

    // ---------------------------------------------------------------------------
    @deprecated("use reducer rather") case class ToStdev(targets: TqRPathz) extends ActionUUc with TodoV1 { def hack(c: Cls) = targets.resolve(c).from1KFX
      def _meta(c: Cls): Cls  = hack(c).pipe(c.updateSoleInfo(_, Info.oneDouble)) 

      def atomuu(c: Cls): AtomUU =
        if (c.field(hack(c)).info1.isInt) _ToIntStdev   (hack(c))
        else                              _ToDoubleStdev(hack(c))
    }

}

// ===========================================================================
