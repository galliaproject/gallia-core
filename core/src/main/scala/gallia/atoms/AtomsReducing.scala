package gallia
package atoms

import heads.reducing._
import domain.GroupingPair._

// ===========================================================================
object AtomsReducing {

  @Scalability case class _Reduce(triplets: Seq[ReducingDataTriplet]) extends AtomZU { def naive(z: Objs) = {
      z .toListAndTrash
        .pipe { list =>
          triplets
            .map { triplet =>
              list
               .map(_.attemptKey(triplet.key))
               .pipe(triplet.dataEntry ) }
          .pipe(gallia.obj) } } }

  // ===========================================================================
  case class _Agg1(pair: GroupingPairN1, groupee: ReducingDataTriplet1, groupers: Keyz, as: Key) extends AtomZZ { def naive(z: Objs) =
      z.aggregateNumbers1(pair)(groupee.key, groupers, as)(groupee.data) }

    // ---------------------------------------------------------------------------
    case class _AggN(pair: GroupingPairNN, groupees: ReducingDataTriplet1s, groupers: Keyz, as: Key) extends AtomZZ { def naive(z: Objs) =
      z.aggregateNumbersN(pair)(groupees.keys, groupers, as) {
        _ .asInstanceOf[List[Obj]]
          .pipe(Objs.from)
          .pipe(_Reduce(groupees.values).naive) } }

  // ===========================================================================
  case class _CountBy(pair: GroupingPairNN, groupees: Keyz, ctipe: CountLikeType, groupers: Keyz, as: Key) extends AtomZZ { def naive(z: Objs) = {
    z.countLike(pair)(groupees, groupers, as)(ctipe.data) } }

}

// ===========================================================================
