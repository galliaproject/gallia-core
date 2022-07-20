package gallia
package atoms

import domain.GroupingPair._

// ===========================================================================
object AtomsZZ {

  // ---------------------------------------------------------------------------
  case object _AsViewBased     extends AtomZZ { def naive(z: Objs) = z._asViewBased     }
  case object _AsIteratorBased extends AtomZZ { def naive(z: Objs) = z._asIteratorBased }

  // ===========================================================================
  case class _MapU2U(o2o: Obj => Obj) extends AtomZZ { def naive(z: Objs) = z.map          (o2o) }
  case class _MapU2V(o2v: Obj => Vle) extends AtomZV { def naive(z: Objs) = z.mapToStreamer(o2v).toList }

  // ---------------------------------------------------------------------------
  case class _FlatMap(o2z: Obj => Objs) extends AtomZZ { def naive(z: Objs) = z.flatMap(o2z(_).toListAndTrash) }

  // ===========================================================================
  @Distributivity
  case class _LogProgress(nOpt: Option[Int], debug: Obj => String) extends AtomZZ { def naive(z: Objs) =
    utils.AtomsHelper.logProgress(z, nOpt, debug) }

  // ---------------------------------------------------------------------------
  @Distributivity case class _Take(n: Int) extends AtomZZ { def naive(z: Objs) = z.take(n) }
  @Distributivity case class _Drop(n: Int) extends AtomZZ { def naive(z: Objs) = z.drop(n) }

  // ---------------------------------------------------------------------------
  @Distributivity
  case class _AddIndex(key: Key, oneBased: Boolean) extends AtomZZ { def naive(z: Objs) =
      z .toListAndTrash
        .zipWithIndex
        .map { case (o, value) => 
          o.addKey(key, if (oneBased) value + 1 else value) }
        .pipe(Objs.from) }

  // ===========================================================================
  case class _Distinct(c: Cls) extends AtomZZ { def naive(z: Objs) = z.distinct(c) }

  // ---------------------------------------------------------------------------
  case class _EnsureUniquenessBy(c: Cls, keys: Keyz) extends AtomZZ { def naive(z: Objs) =
      z.ensureDistinctBy(c)(keys).fold(sizes => _Error.Runtime.NotUnique(Some(keys), sizes).throwDataError(z), identity) }

    // ---------------------------------------------------------------------------
    object _EnsureUniquenessBy {
      def error(sizes: (Int, Int))(z: Objs) = _Error.Runtime.NotUnique(None, sizes).throwDataError(z)
    }

  // ===========================================================================
  // grouping
  // TODO: t210114170853 - optimization: separate at least 11/1N/N1/NN?

  case class _Group1N(pair: GroupingPairN1, groupee: Ren, groupers: Renz) extends AtomZZ {
    def naive(z: Objs) = z.group1N(pair)(groupee, groupers) }

  // ---------------------------------------------------------------------------
  case class _GroupNN(pair: GroupingPairNN, groupees: Renz, grouper: Renz, as: Key) extends AtomZZ { def naive(z: Objs) =
    z.groupNN(pair)(groupees, grouper, as) }

}

// ===========================================================================
