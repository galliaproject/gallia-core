package gallia.data.multiple.streamer

import scala.reflect.{ClassTag => CT}

import aptus.Anything_

import gallia.Aliases.Coll
import gallia.heads.merging.MergingData._
import gallia.data.multiple.streamer.{ViewStreamerUtils => _utils}

// ===========================================================================
class ViewStreamer[A](view: ViewRepr[A]) extends Streamer[A] {
  val tipe = StreamerType.ViewBased

  // ---------------------------------------------------------------------------
  private def _rewrap[B](newView: ViewRepr[B]): Streamer[B] = new ViewStreamer(newView)

  protected def egal(that: Streamer[A]): Boolean = this.toList == that.toList

  // ===========================================================================
  def iteratorAndCloseable: (Iterator[A], java.io.Closeable) = (iterator, new java.io.Closeable { def close() {} })

  def iterator: Iterator[A] = view.iterator
  def toList  : List    [A] = view.force.toList
  def toView  : ViewRepr[A] = view

  def     map[B: CT](f: A =>      B ): Streamer[B] = view.    map(f).toSeq.view.thn(_rewrap)
  def flatMap[B: CT](f: A => Coll[B]): Streamer[B] = view.flatMap(f).toSeq.view.thn(_rewrap)

  def filter(p: A => Boolean): Streamer[A] = view.filter(p).toSeq.view.thn(_rewrap)

  def size: Int = view.force.size

  def isEmpty: Boolean = view.isEmpty

  def take(n: Int): Streamer[A] = view.take(n).thn(_rewrap)
  def drop(n: Int): Streamer[A] = view.drop(n).thn(_rewrap)

  // ===========================================================================
  def reduce(op: (A, A) => A): A = view.reduce(op)

  // ===========================================================================
  def sortBy[K](ignored: CT[K], ord: Ordering[K])(f: A => K): Streamer[A] = view.sortBy(f)(ord).toSeq.view.thn(_rewrap)

  // ---------------------------------------------------------------------------
  def distinct: Streamer[A] = view.distinct.toSeq.view.thn(_rewrap)

  // ---------------------------------------------------------------------------
  def groupByKey[K: CT, V: CT](implicit ev: A <:< (K, V)): Streamer[(K, List[V])] =
    view.asInstanceOf[ViewRepr[(K, V)]].thn(_utils.groupByKey).thn(_rewrap)

  // ===========================================================================
  def union[B >: A : CT](that: Streamer[B]): Streamer[B] = _utils.union(this.asInstanceOf[ViewStreamer[B]], that)

  // ===========================================================================
  def coGroup[K: CT, V: CT](joinType: JoinType)(that: Streamer[(K, V)])(implicit ev: A <:< (K, V)): Streamer[(K, (Iterable[V], Iterable[V]))] =
    _utils.coGroup(joinType)(this.asInstanceOf[Streamer[(K, V)]], that)

  // ---------------------------------------------------------------------------
  def join[K: CT, V: CT](joinType: JoinType, combine: (V, V) => V)(that: Streamer[(K, V)])(implicit ev: A <:< (K, V)): Streamer[V] =
    _utils.join(joinType, combine)(this.asInstanceOf[Streamer[(K, V)]], that)
}

// ===========================================================================

