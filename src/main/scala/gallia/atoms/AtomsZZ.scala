package gallia.atoms

import gallia._

// ===========================================================================
object AtomsZZ {

  @gallia.Distributivity
  case class _LogProgress(nOpt: Option[Int], debug: Obj => String) extends AtomZZ { def naive(z: Objs) =
    utils.AtomsHelper.logProgress(z, nOpt, debug) }

  // ---------------------------------------------------------------------------
  @gallia.Distributivity
  case class _Take(n: Int) extends AtomZZ { def naive(z: Objs) =
      if (z.isEmpty) throw new RuntimeError(s"TODO:210114170445:Empty:${n}")
      else           z.take(n) }

  // ===========================================================================
  case object _Distinct extends AtomZZ { def naive(z: Objs) =
    z.distinct }

  // ---------------------------------------------------------------------------
  case object _EnsureUniqueness extends AtomZZ { def naive(z: Objs) =
      z.ensureDistinct.fold(sizes => _Error.Runtime.NotUnique(None, sizes).throwRuntimeError(z), identity) }

    // ---------------------------------------------------------------------------
    case class _EnsureUniquenessBy(keys: Keyz) extends AtomZZ { def naive(z: Objs) =
      z.ensureDistinct.fold(sizes => _Error.Runtime.NotUnique(Some(keys), sizes).throwRuntimeError(z), identity) }

  // ===========================================================================
  // grouping
  // TODO: t210114170853 - optimization: separate at least 11/1N/N1/NN?

  case class _Group1N(groupee: Key, groupers: Keyz) extends AtomZZ { def naive(z: Objs) =
    z.group1N(groupee, groupers) }

  // ---------------------------------------------------------------------------
  case class _GroupNN(groupees: Keyz, grouper: Keyz, as: Key) extends AtomZZ { def naive(z: Objs) =
    z.groupNN(groupees, grouper, as) }

}

// ===========================================================================
