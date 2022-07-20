package gallia
package data
package multiple

import domain.GroupingPair._
import atoms.utils.GalliaSpilling._

// ===========================================================================
trait ObjsAggregations { self: Objs => // 220629103917 - by-pass mechanism for iterator
  import ObjsAggregations._

  // ---------------------------------------------------------------------------
  def group1N(pair: GroupingPairN1)(groupee: Ren, groupers: Renz): Objs =
      _modifyUnderlyingStreamer {
        _group1N(pair)(groupee, groupers)(_)
          .map(recombine(as = groupee.to, _.flatten)) }

    // ---------------------------------------------------------------------------
    def groupNN(pair: GroupingPairNN)(groupees: Renz, groupers: Renz, as: Key): Objs =
      _modifyUnderlyingStreamer {
        _groupNN(pair)(groupees, groupers)(_)
          .map(recombine(as, _.flatten)) }

  // ===========================================================================
  // TODO: if empty? wrap runtime error properly, or just make field optional? or NaN?
  def aggregateNumbers1(pair: GroupingPairN1)(groupee: Key, groupers: Keyz, as: Key)(f: List[Option[Any]] => Any): Objs =
      _modifyUnderlyingStreamer {
        _group1N(pair)(Ren.from(groupee), groupers.renz)(_)
            .map(recombine(as, f)) }

    // ---------------------------------------------------------------------------
    def aggregateNumbersN(pair: GroupingPairNN)(groupees: Keyz, groupers: Keyz, as: Key)(f: List[Any] => Any): Objs =
      _modifyUnderlyingStreamer {
        _groupNN(pair)(groupees.renz, groupers.renz)(_)
          .map(recombine(as, x => f(x.flatten))) }

  // ===========================================================================
  def countLike(pair: GroupingPairNN)(groupees: Keyz, groupers: Keyz, as: Key)(f: List[Option[Obj]] => Any): Objs =
    _modifyUnderlyingStreamer {
      _groupNN(pair)(groupees.renz, groupers.renz)(_)
        .map(recombine(as, f)) }

  // ===========================================================================
  private def _group1N(pair: GroupingPairN1)(groupee: Ren, groupers: Renz)(input: Streamer[Obj]): Streamer[(Option[Obj], List[Option[Vle]])] = {
    val pairs: Streamer[(Option[Obj], Option[Vle])] = input.map { o => o.retainOpt(groupers) -> o.attemptKey(groupee.from) }

    if (!isIteratorBased) pairs.groupByKey
    else                  pairs.pipe(spillingGroupBy1N(pair)) }

  // ---------------------------------------------------------------------------
  private def _groupNN(pair: GroupingPairNN)(groupees: Renz, groupers: Renz)(input: Streamer[Obj]): Streamer[(Option[Obj], List[Option[Obj]])] = {
    val pairs: Streamer[(Option[Obj], Option[Obj])] = input.map { o => o.retainOpt(groupers) -> o.retainOpt(groupees) }

    if (!isIteratorBased) pairs.groupByKey
    else                  pairs.pipe(spillingGroupByNN(pair)) }

}

// ===========================================================================
object ObjsAggregations {

  def recombine[T](grouperAs: Key, groupeeAs: Key)(value: (Option[Vle], List[Option[T]])): Obj =
    value match {
      case (Some(x), v) => obj(grouperAs -> x, groupeeAs -> v.flatten /* may be empty */)
      case (None,    v) => obj(                groupeeAs -> v.flatten /* may not empty (by design) */) } // would fail shortly after at runtime if empty anyway (see a210113121804)

  // ---------------------------------------------------------------------------
  def recombine[T, U](as: Key, f: List[Option[T]] => U)(value: (Option[Obj], List[Option[T]])): Obj =
    value match {
      case (Some(o), v) => o.addKey(as,   f(v))
      case (None,    v) =>      obj(as -> f(v)) }

}

// ===========================================================================
