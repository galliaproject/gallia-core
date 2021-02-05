package gallia.atoms

import gallia._

// ===========================================================================
@deprecated object AtomsStats {

  @deprecated case class _StatsInts(groupee: Key, groupers: Keyz, as: Key) extends AtomZZ { def naive(z: Objs) =
      z.numberStats(groupee, groupers, as) }

    @deprecated case class _StatsDoubles(groupee: Key, groupers: Keyz, as: Key) extends AtomZZ { def naive(z: Objs) =
      z.numberStats(groupee, groupers, as) }

}

// ===========================================================================
