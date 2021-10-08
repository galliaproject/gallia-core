package gallia.atoms

import gallia._
import gallia.heads.reducing._

// ===========================================================================
object AtomsReducing {

  @Scalability case class _Reduce(triplets: Seq[ReducingDataTriplet]) extends AtomZU { def naive(z: Objs) = {
      z .toListAndTrash
        .pipe { list =>
          triplets
            .map { triplet =>
              list
               .map(_.opt(triplet.key))
               .pipe(triplet.dataEntry ) }
          .pipe(gallia.obj) } } }

  // ===========================================================================
  case class _Agg1(groupee: ReducingDataTriplet1, groupers: Keyz, as: Key) extends AtomZZ { def naive(z: Objs) =
      z.aggregateNumbers1(groupee.key, groupers, as)(groupee.data) }

    // ---------------------------------------------------------------------------
    case class _AggN(groupees: ReducingDataTriplet1s, groupers: Keyz, as: Key) extends AtomZZ { def naive(z: Objs) =
      z.aggregateNumbersN(groupees.keys, groupers, as) {
        _ .asInstanceOf[Seq[Obj]]
          .pipe(Objs.from)
          .pipe(_Reduce(groupees.values).naive) } }

  // ===========================================================================
  case class _CountBy(groupees: Keyz, ctipe: CountLikeType, groupers: Keyz, as: Key) extends AtomZZ { def naive(z: Objs) = {
    z.countLike(groupees, groupers, as)(ctipe.data) } }

}

// ===========================================================================
