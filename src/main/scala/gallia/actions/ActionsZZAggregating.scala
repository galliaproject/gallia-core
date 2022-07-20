package gallia
package actions

import target.{TqRenz, TqRen}
import domain.GroupingPair._

// ===========================================================================
object ActionsZZAggregating {
  import gallia.heads.reducing._
  import gallia.atoms.AtomsReducing._

  // ---------------------------------------------------------------------------
  //TODO: rtipe: distinguish counts from others

  // ---------------------------------------------------------------------------
  case class CountBy(groupers: TqRenz, ctipe: CountLikeType, asOpt: Option[Key]) extends ActionZZc with TodoV1
      /* "as" boilerplate: */ with CanForceAs2[CountBy] { override val defaultKey: Key = ctipe.defaultKey; def forceAs(key: Key) = copy(asOpt = Some(key))

    // ---------------------------------------------------------------------------
    def _meta(in: Cls ): Cls = groupers.resolve(in).pipe(in.countAll(_, as))

    // ---------------------------------------------------------------------------
    def atomzz(in: Cls): AtomZZ = {
      val r: Renz = groupers.resolve(in)
      val e: Keyz = in.complementKeyz(r.froms)

      _CountBy(
        GroupingPairNN.from(in)(e.renz, r)._1,
        e,
        ctipe,
        r.fromsFX, as) } }

  // ===========================================================================
  case class Agg1(groupee: TqRen, groupers: TqRenz, rtipe: ReducingType) extends ActionZZc with TodoV1 {

    def _meta(in: Cls ): Cls    =  {
      val e = groupee .resolve(in)
      val r = groupers.resolve(in)

      in.aggregate1(rtipe)(e, r) }

    // ---------------------------------------------------------------------------
    def atomzz(in: Cls): AtomZZ = {
      val e = groupee .resolve(in)
      val r = groupers.resolve(in)

      _Agg1(
        GroupingPairN1.from(in)(e, r)._1,
        dataTriplet1(in, rtipe)(e.from),
        r.fromsFX,
        as = e.to) } }

  // ===========================================================================
  case class AggN(groupees: TqRenz, rtipe: ReducingType, groupers: TqRenz, asOpt: Option[Key]) extends ActionZZc with TodoV1
      /* "as" boilerplate: */ with CanForceAs2[AggN] { override val defaultKey: Key = rtipe.defaultPluralKey; def forceAs(key: Key) = copy(asOpt = Some(key))

    // ---------------------------------------------------------------------------
    def _meta(in: Cls ): Cls = {
      val e = groupees.resolve(in)
      val r = groupers.resolve(in)

      in.aggregateN(rtipe)(r, e, as) }

    // ---------------------------------------------------------------------------
    def atomzz(in: Cls): AtomZZ = {
      val e = groupees.resolve(in)
      val r = groupers.resolve(in)

      _AggN(
        GroupingPairNN.from(in)(e, r)._1,
        e.fromsFX.pipe(dataTriplet1s(in, rtipe)),
        r.fromsFX,
        as) } }

  // ===========================================================================
  private def dataTriplet1s(c: Cls, rtipe: ReducingType)(groupees: Keyz): ReducingDataTriplet1s =
      groupees
        .map(dataTriplet1(c, rtipe))
        .pipe(ReducingDataTriplet1s)

    // ---------------------------------------------------------------------------
    private def dataTriplet1(c: Cls, rtipe: ReducingType)(groupee: Key): ReducingDataTriplet1 =
      ReducingDataTriplet1(groupee, rtipe, c.isOptional(groupee), c.field(groupee).numericalTypeOpt)

}

// ===========================================================================
