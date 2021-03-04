package gallia.data.multiple.streamer

import scala.reflect.{ClassTag => CT}

import aptus.Anything_
import gallia.Aliases.Coll
import gallia.heads.merging.MergingData._
import gallia.data.multiple.streamer.{IteratorStreamerUtils => _utils}

// ===========================================================================
class IteratorStreamer[A](itr: Iterator[A]) extends Streamer[A] {
  // TODO: t210115104555 - tricky... depending on what it's used for, need to:
  //   - keep track of closeable
  //   - keep track of consumption (consumed or not, eg via .size)
  //   - keep access to source so can reread
  //   - provide "checkpointing"?
  //   - look into geny?
	// FIXME: detect "forking", will need intermediate file

  val tipe = StreamerType.IteratorBased

  // ---------------------------------------------------------------------------
  private def _rewrap[B](y: Iterator[B]): Streamer[B] = Streamer.fromIterator(y)

  protected def egal(that: Streamer[A]): Boolean = itr.toList == that.toList

  // ===========================================================================
  def iteratorAndCloseable: (Iterator[A], java.io.Closeable) = (iterator, ??? /* see t210115104555 */)

  def iterator: Iterator[A] = itr
  def toView  : ViewRepr[A] = itr.toSeq.view
  def toList  : List    [A] = itr.toList

  def     map[B: CT](f: A =>      B ): Streamer[B] = IteratorParHack.    map(itr)(f).thn(_rewrap)
  def flatMap[B: CT](f: A => Coll[B]): Streamer[B] = IteratorParHack.flatMap(itr)(f).thn(_rewrap)

  def filter(p: A => Boolean): Streamer[A] = itr.filter(p).thn(_rewrap)

  def size: Int = itr.size

  def isEmpty: Boolean = itr.isEmpty

  def take(n: Int): Streamer[A] = itr.take(n).thn(_rewrap)
  def drop(n: Int): Streamer[A] = itr.drop(n).thn(_rewrap)

  // ===========================================================================
  def reduce(op: (A, A) => A): A = itr.reduce(op)

  // ===========================================================================
  def sortBy[K](ignored: CT[K], ord: Ordering[K])(f: A => K): Streamer[A] = gallia.illegal("TODO:210115103131:NotImplemented") // TODO: see spilling hack

  // ---------------------------------------------------------------------------
  def distinct: Streamer[A] = gallia.illegal("TODO:210115103132:NotImplemented") // TODO: see spilling hack

  // ---------------------------------------------------------------------------
  def groupByKey[K: CT, V: CT](implicit ev: A <:< (K, V)): Streamer[(K, List[V])] =
     itr.asInstanceOf[Iterator[(K, V)]].thn(_utils.groupByKey).thn(_rewrap)  

  // ===========================================================================
  def union[B >: A : CT](that: Streamer[B]): Streamer[B] = _rewrap(_utils.union(this.asInstanceOf[IteratorStreamer[B]], that))

  // ===========================================================================
  override def coGroup[K: CT, V: CT](joinType: JoinType)(that: Streamer[(K, V)])(implicit ev: A <:< (K, V)): Streamer[(K, (Iterable[V], Iterable[V]))] =
    gallia.illegal("TODO:210115103134:NotImplemented") // TODO: see spilling hack

  // ---------------------------------------------------------------------------
  override def join[K: CT, V: CT](joinType: JoinType, combine: (V, V) => V)(that: Streamer[(K, V)])(implicit ev: A <:< (K, V)): Streamer[V] =
    _utils.join(joinType, combine)(this.asInstanceOf[Streamer[(K, V)]], that).thn(_rewrap)

}

// ===========================================================================
