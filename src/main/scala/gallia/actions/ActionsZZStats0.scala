package gallia
package actions

// ===========================================================================
@deprecated("see 210118083814 for new version") object ActionsZZStats0 {
  import gallia.atoms.AtomsStats._

  // ---------------------------------------------------------------------------
  @deprecated case class _Tmp(groupee : Key , grouper: Keyz, as: Key) { def triplet = (groupee , grouper, as) }

  // ---------------------------------------------------------------------------
  @NumberAbstraction
  @deprecated("see 210118083814 for new version") case class Stats0(groupee: Ren, groupers: Renz, asOpt: Option[Key]) extends ActionZZc with CanForceAs2[Stats0] {
        /* "as" boilerplate: */ override val defaultKey = _stats; override def forceAs(key: Key) = copy(asOpt = Some(key))

    def vldt (in: Cls ): Errs   = Nil// TODO
    def _meta(in: Cls ): Cls = in.stats(groupers, as)

    def _tmp2 = _Tmp(groupee.fromFX, groupers.fromsFX, as)

    def atomzz(in: Cls): AtomZZ = in.numericalType(groupee) match {
        case BasicType._Int    => _StatsInts   .tupled(_tmp2.triplet)
        case BasicType._Double => _StatsDoubles.tupled(_tmp2.triplet)
        case x => ??? }
  }

}

// ===========================================================================
