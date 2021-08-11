package gallia.atoms

import gallia._
import scala.util.chaining._

// ===========================================================================
object AtomsZZ {

  @gallia.Distributivity
  case class _LogProgress(nOpt: Option[Int], debug: Obj => String) extends AtomZZ { def naive(z: Objs) =
    utils.AtomsHelper.logProgress(z, nOpt, debug) }

  // ---------------------------------------------------------------------------
  @gallia.Distributivity
  case class _Take(n: Int) extends AtomZZ { def naive(z: Objs) = z.take(n) }

  // ---------------------------------------------------------------------------
  @gallia.Distributivity
  case class _AddIndex(key: Key, oneBased: Boolean) extends AtomZZ { def naive(z: Objs) =
      z .toListAndTrash
        .zipWithIndex
        .map { case (o, value) => 
          o.add(key, if (oneBased) value + 1 else value) }
        .pipe(Objs.from) }

  // ===========================================================================
  case object _Distinct extends AtomZZ { def naive(z: Objs) =
    z.distinct }

  // ---------------------------------------------------------------------------
  case object _EnsureUniqueness extends AtomZZ { def naive(z: Objs) =
      z.ensureDistinct.fold(sizes => _Error.Runtime.NotUnique(None, sizes).throwDataError(z), identity) }

    // ---------------------------------------------------------------------------
    case class _EnsureUniquenessBy(keys: Keyz) extends AtomZZ { def naive(z: Objs) =
      z.ensureDistinctBy(keys).fold(sizes => _Error.Runtime.NotUnique(Some(keys), sizes).throwDataError(z), identity) }

  // ===========================================================================
  // grouping
  // TODO: t210114170853 - optimization: separate at least 11/1N/N1/NN?

  case class _Group1N(groupee: Ren, groupers: Renz) extends AtomZZ { def naive(z: Objs) =
    z.group1N(groupee, groupers) }

  // ---------------------------------------------------------------------------
  case class _GroupNN(groupees: Renz, grouper: Renz, as: Key) extends AtomZZ { def naive(z: Objs) =
    z.groupNN(groupees, grouper, as) }

}

// ===========================================================================
