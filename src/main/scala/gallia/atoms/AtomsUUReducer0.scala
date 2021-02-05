package gallia.atoms

import aptus.Seq_

import gallia._

// ===========================================================================
object AtomsUUReducer0 {

  @deprecated("use reducer rather") case class _ToSize(key: Key) extends AtomUU { def naive(o: Obj) =
      o.transformPath(key, _.asInstanceOf[Seq[_]].size) }

    // ---------------------------------------------------------------------------
    @deprecated("use reducer rather") case class _ToIntSum(key: Key) extends AtomUU { def naive(o: Obj) =
        o.transformInts(key, _.sum) }
      @deprecated("use reducer rather") case class _ToDoubleSum(key: Key) extends AtomUU { def naive(o: Obj) =
        o.transformDoubles(key, _.sum) }

    // ---------------------------------------------------------------------------
    @deprecated("use reducer rather") case class _ToIntMean(key: Key) extends AtomUU { def naive(o: Obj) =
        o.transformInts(key, _.mean) }
      @deprecated("use reducer rather") case class _ToDoubleMean(key: Key) extends AtomUU { def naive(o: Obj) =
        o.transformDoubles(key, _.mean) }

    // ---------------------------------------------------------------------------
    @deprecated("use reducer rather") case class _ToIntStdev(key: Key) extends AtomUU { def naive(o: Obj) =
        o.transformInts(key, _.stdev) }
      @deprecated("use reducer rather") case class _ToDoubleStdev(key: Key) extends AtomUU { def naive(o: Obj) =
        o.transformDoubles(key, _.stdev) }

}

// ===========================================================================

