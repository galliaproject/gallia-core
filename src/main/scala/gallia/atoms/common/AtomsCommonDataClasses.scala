package gallia
package atoms
package common

// ===========================================================================
object AtomsCommonDataClasses {

  case class _CotransformViaDataClassWithErasing(rest: Keyz, f: _ff11) extends AtomUU { def naive(o: Obj): Obj =
      o.retainOpt(rest) match {
        case None    =>         f(o).asInstanceOf[Obj]
        case Some(x) => x.merge(f(o).asInstanceOf[Obj]) } }

  // ---------------------------------------------------------------------------
  case class _CotransformViaDataClassWithoutErasing(f: _ff11) extends AtomUU { def naive(o: Obj): Obj =
    f(o) match {
      case None    => o
      case o2: Obj => o.merge(o2) } }

  // ===========================================================================
  case class _CotransformViaDataClassAsWithErasing(rest: Keyz, as: Key ,f: _ff11) extends AtomUU { def naive(o: Obj): Obj =
    o.retainOpt(rest) match {
      case None    =>        obj(as -> f(o))
      case Some(x) => x.addEntry(as,   f(o)) } }

  // ---------------------------------------------------------------------------
  case class _CotransformViaDataClassAsWithoutErasing(as: Key ,f: _ff11) extends AtomUU { def naive(o: Obj): Obj =
    o.putEntry(as, f(o)) }

}

// ===========================================================================

