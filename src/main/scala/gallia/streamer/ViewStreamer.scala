package gallia
package streamer

import scala.collection.compat.immutable.LazyList // need compat version for 2.12
import heads.merging.MergingData._
import atoms.utils.SuperMetaPair
import streamer.{ViewStreamerUtils => _utils}

// ===========================================================================
class ViewStreamer[A](view: ViewRepr[A]) extends Streamer[A] { // TODO: add a ListStreamer purely for tests
  override val tipe = StreamerType.ViewBased

  // ---------------------------------------------------------------------------
  private def _rewrap[B](newView: ViewRepr[B]): Streamer[B] = new ViewStreamer(newView)

  // ===========================================================================
  private[gallia] def selfClosingIterator:                  Iterator[A] =                                          view.iterator  // nothing to close
  private[gallia] def closeabledIterator  : aptus.CloseabledIterator[A] = aptus.CloseabledIterator.fromUncloseable(view.iterator) // nothing to close

  // ---------------------------------------------------------------------------
//override def toView  : ViewRepr[A] = view
  override def toList  : List    [A] = view.toList

  // ---------------------------------------------------------------------------
  override def toViewBased    : Streamer[A] = this
  override def toIteratorBased: Streamer[A] = IteratorStreamer.from(new data.DataRegenerationClosure[A] { def regenerate = () => closeabledIterator })

  override def toMeBased [B >: A : CT](that: Streamer[B]): Streamer[B] = that.toViewBased

  // ===========================================================================
  override def     map[B: CT](f: A =>      B ): Streamer[B] = view.    map(f).toSeq.view.pipe(_rewrap)
  override def flatMap[B: CT](f: A => Coll[B]): Streamer[B] = view.flatMap(f).toSeq.view.pipe(_rewrap)

  override def filter(p: A => Boolean): Streamer[A] = view.filter(p).toSeq.view.pipe(_rewrap)
  override def find  (p: A => Boolean): Option  [A] = view.find  (p)

  override def size: Int = view.size

  override def isEmpty: Boolean = view.isEmpty

  override def take(n: Int): Streamer[A] = view.take(n).pipe(_rewrap)
  override def drop(n: Int): Streamer[A] = view.drop(n).pipe(_rewrap)

  override def takeWhile(p: A => Boolean): Streamer[A] = view.takeWhile(p).iterator.to(LazyList).view.pipe(_rewrap) // TODO: better way?
  override def dropWhile(p: A => Boolean): Streamer[A] = view.dropWhile(p).iterator.to(LazyList).view.pipe(_rewrap) // TODO: better way?

  // ===========================================================================
  override def reduce(op: (A, A) => A): A = view.reduce(op)

  // ===========================================================================
  override def sortBy[K](meta: SuperMetaPair[K])(f: A => K): Streamer[A] = view.sortBy(f)(meta.ord).toSeq.view.pipe(_rewrap)

  // ---------------------------------------------------------------------------
  override def distinct: Streamer[A] = view.distinct.toSeq.view.pipe(_rewrap)

  // ---------------------------------------------------------------------------
  override def groupByKey[K: CT, V: CT](implicit ev: A <:< (K, V)): Streamer[(K, List[V])] =
    view.asInstanceOf[ViewRepr[(K, V)]].pipe(_utils.groupByKey).pipe(_rewrap)

  // ===========================================================================
  override def union[B >: A : CT](that: Streamer[B])                       : Streamer[B] = _utils.union(this.asInstanceOf[ViewStreamer[B]], that)
  override def zip  [B >: A : CT](that: Streamer[B], combiner: (B, B) => B): Streamer[B] = _utils.zip  (this.asInstanceOf[ViewStreamer[B]], that, combiner)

  // ===========================================================================
  override def coGroup[K: CT, V: CT](joinType: JoinType)(that: Streamer[(K, V)])(implicit ev: A <:< (K, V)): Streamer[(K, (Iterable[V], Iterable[V]))] =
    _utils.coGroup(joinType)(this.asInstanceOf[Streamer[(K, V)]], that)

  // ---------------------------------------------------------------------------
  override def join[K: CT, V: CT](joinType: JoinType, combine: (V, V) => V)(that: Streamer[(K, V)])(implicit ev: A <:< (K, V)): Streamer[V] =
    _utils.join(joinType, combine)(this.asInstanceOf[Streamer[(K, V)]], that)
}

// ---------------------------------------------------------------------------
object ViewStreamer {
  def from[T](data: List[T]): ViewStreamer[T] = new ViewStreamer(data.view)
}

// ===========================================================================

