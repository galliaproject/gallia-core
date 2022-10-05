package gallia
package streamer

import scala.reflect.{ClassTag => CT}
import aptus.Anything_

import aptus.CloseabledIterator
import heads.merging.MergingData._
import atoms.utils.SuperMetaPair
import streamer.{IteratorStreamerUtils => _utils}

// ===========================================================================
object IteratorStreamer {
  def from [T](gen: DataRegenerationClosure[T]): IteratorStreamer[T] = new IteratorStreamer(gen.regenerate)
}

// ===========================================================================
final class IteratorStreamer[A](gen: () => CloseabledIterator[A]) extends Streamer[A] {
  // TODO: t210204105730 - look into https://github.com/com-lihaoyi/geny?
  private lazy val underlying: CloseabledIterator[A] = gen()

  // ---------------------------------------------------------------------------
  override val tipe: StreamerType = StreamerType.IteratorBased

  // ===========================================================================
  def fork: IteratorStreamer[A] = new IteratorStreamer(() => gen())

  // ---------------------------------------------------------------------------
  private def itr: CloseabledIterator[A] = { ensureStillUsable(); underlying }

          def _alter  [B](f: CloseabledIterator[A] => CloseabledIterator[B]): IteratorStreamer[B] = new DataRegenerationClosure[B] { def regenerate = () => f(itr) }.pipe(IteratorStreamer.from)
  private def _consume[B](f: CloseabledIterator[A] => B)                    : B                   = f(itr).tap { _ => consumed = true; underlying.close() }

    // ---------------------------------------------------------------------------
    private var consumed: Boolean = false
    private var exited  : Boolean = false

    // ---------------------------------------------------------------------------
    private def ensureStillUsable() = {
      assert(!consumed, "consumed")
      assert(!exited  , "exited") }

    // ---------------------------------------------------------------------------
    def formatEither: Either[String, List[A]] =
           if (consumed) Left ("consumed")
      else if (exited)   Left ("exited")
      else               Right(toList)

  // ===========================================================================
  private[gallia] def selfClosingIterator:                 Iterator[A] = { exited = true; underlying.toSelfClosing }
  private[gallia] def closeabledIterator : aptus.CloseabledIterator[A] = { exited = true; underlying }

  // ---------------------------------------------------------------------------
//override def toView  : ViewRepr[A] =  { exited = true; itr.to(collection.immutable.LazyList).view }
  override def toList  : List    [A] =  _consume(_.consumeAll)

  // ---------------------------------------------------------------------------
  override def toViewBased    : Streamer[A] = ViewStreamer.from(toList)
  override def toIteratorBased: Streamer[A] = this

  // ---------------------------------------------------------------------------
  override def toMeBased[B >: A : CT](that: Streamer[B]): Streamer[B] = that.toIteratorBased

  // ===========================================================================
  override def     map[B: CT](f: A =>      B ): Streamer[B] = _alter(IteratorParHack.    map(f))
  override def flatMap[B: CT](f: A => Coll[B]): Streamer[B] = _alter(IteratorParHack.flatMap(f))

  // ---------------------------------------------------------------------------
  private[gallia] def     _map[B](f: A =>      B) : IteratorStreamer[B] = _alter(IteratorParHack.    map(f))
  private[gallia] def _flatMap[B](f: A => Coll[B]): IteratorStreamer[B] = _alter(IteratorParHack.flatMap(f))

  // ---------------------------------------------------------------------------
  override def filter(p: A => Boolean): Streamer[A] = _alter  (_.filter(p))
  override def find  (p: A => Boolean): Option  [A] = _consume(_.find  (p))

  // ---------------------------------------------------------------------------
  override def size   : Int     = _consume(_.size)
  override def isEmpty: Boolean = itr.isEmpty

  // ---------------------------------------------------------------------------
  override def take(n: Int): Streamer[A] = _alter(_.take(n))
  override def drop(n: Int): Streamer[A] = _alter(_.drop(n))

  // ---------------------------------------------------------------------------
  override def takeWhile(p: A => Boolean): Streamer[A] = _alter(_.takeWhile(p))
  override def dropWhile(p: A => Boolean): Streamer[A] = _alter(_.dropWhile(p))

  // ===========================================================================
  override def reduce(op: (A, A) => A): A = _consume(_.reduce(op))

  // ---------------------------------------------------------------------------
  override def union[B >: A : CT](that: Streamer[B]): Streamer[B] = {
    ensureStillUsable(); _utils.union(this.asInstanceOf[IteratorStreamer[B]], that) }

  // ---------------------------------------------------------------------------
  override def zip[B >: A : CT](that: Streamer[B], combiner: (B, B) => B): Streamer[B] = {
    ensureStillUsable(); _utils.zip(this.asInstanceOf[IteratorStreamer[B]], that, combiner) }

  // ===========================================================================
  override def sortBy[K](meta: SuperMetaPair[K])(f: A => K): Streamer[A] =
    aptus.illegalState("220629103901 - should be by-passed now (see 220629103917)")

  // ---------------------------------------------------------------------------
  override def distinct: Streamer[A] =
    aptus.illegalState("220629103902 - should be by-passed now (see 220629103917)")

  // ---------------------------------------------------------------------------
  override def groupByKey[K: CT, V: CT](implicit ev: A <:< (K, V)): Streamer[(K, List[V])] =
    aptus.illegalState("220629103903 - should be by-passed now (see 220629103917)")

  // ===========================================================================
  override def coGroup[K: CT, V: CT](joinType: JoinType)(that: Streamer[(K, V)])(implicit ev: A <:< (K, V)): Streamer[(K, (Iterable[V], Iterable[V]))] =
    aptus.illegalState("220629103904 - should be by-passed now (see 220629103917)")

  // ---------------------------------------------------------------------------
  override def join[K: CT, V: CT](joinType: JoinType, combine: (V, V) => V)(that: Streamer[(K, V)])(implicit ev: A <:< (K, V)): Streamer[V] =
    { ensureStillUsable(); _utils.join(joinType, combine)(this.asInstanceOf[Streamer[(K, V)]], that) }

  // ===========================================================================
  def groupByPreSortedKey[K, V](implicit ev: A <:< (K, V)) = _alter(_.groupByPreSortedKey) // TODO: generalize to streamer

  // ---------------------------------------------------------------------------
  def firstOpt(): Option[A] =
    if (isEmpty) _consume(_ => None)
    else         _consume(_.next().in.some)
}

// ===========================================================================
