package gallia
package streamer

import heads.merging.MergingData._
import atoms.utils.SuperMetaPair

// ===========================================================================
trait Streamer[A] { // note: not necessarily bothering with genericity (in the general sense) at this point, mostly on abstracting relevant scala/spark collections
  // TODO:
  // - bother with variance?
  // - t210115095741 - performance: give easy access to .par where applicable; consider Iterator counterpart (look into https://github.com/tim-group/iterata)

  // ---------------------------------------------------------------------------
  val tipe: StreamerType

  // ===========================================================================
  final override def equals(that: Any): Boolean = egal(that.asInstanceOf[Streamer[A]])

  // ---------------------------------------------------------------------------
  protected final def egal(that: Streamer[A]): Boolean = this.toList == that.toList // only for tests

  // ===========================================================================
  private[gallia] def selfClosingIterator:                 Iterator[A] // must not reuse streamer afterward
  private[gallia] def closeabledIterator : aptus.CloseabledIterator[A] // must not reuse streamer afterward

  // ---------------------------------------------------------------------------
//def toView  : ViewRepr[A] - TODO: t210315114618 - causes odd compilation issue with gallia-spark, to investigate
  def toList  : List    [A]

  // ===========================================================================
  // ClassTag is a requirement of Spark RDD (easier to include it here than work around it); TODO: Coll as well?
  def     map[B: WTT](f: A =>      B ): Streamer[B]
  def flatMap[B: WTT](f: A => Coll[B]): Streamer[B]

  def filter(p: A => Boolean): Streamer[A]
  def find  (p: A => Boolean): Option  [A]

  def size   : Int
  def isEmpty: Boolean

  @Distributivity // TODO: t210117112314 - maybe sample?
  def take(n: Int): Streamer[A]
  def drop(n: Int): Streamer[A]

  def takeWhile(p: A => Boolean): Streamer[A]
  def dropWhile(p: A => Boolean): Streamer[A]

  // ===========================================================================
  def reduce(op: (A, A) => A): A

  // ---------------------------------------------------------------------------
  // ClassTag[T] is a requirement coming from Spark RDDs
  def sort     (meta: SuperMetaPair[A])           : Streamer[A] = sortBy(meta)(identity)
  def sortBy[K](meta: SuperMetaPair[K])(f: A => K): Streamer[A]

  // ---------------------------------------------------------------------------
  def distinct: Streamer[A]

  /** think uniq command */
  def distinctByAdjacency: Streamer[A] = ??? // t210117113705 - implement

  // ---------------------------------------------------------------------------
  def groupByKey[K: WTT, V: WTT](implicit ev: A <:< (K, V)): Streamer[(K, List[V])]

  // ===========================================================================
  def union[B >: A : WTT](that: Streamer[B])                       : Streamer[B] //TODO: use implicit ev
  def zip  [B >: A : WTT](that: Streamer[B], combiner: (B, B) => B): Streamer[B]

  // ---------------------------------------------------------------------------
  def join   [K: WTT, V: WTT](joinType: JoinType, combiner: (V, V) => V)(that: Streamer[(K, V)])(implicit ev: A <:< (K, V)): Streamer[              V]
  def coGroup[K: WTT, V: WTT](joinType: JoinType                       )(that: Streamer[(K, V)])(implicit ev: A <:< (K, V)): Streamer[(K, (Iterable[V], Iterable[V]))]

  // ===========================================================================
  private[streamer] def toMeBased[B >: A : WTT](that: Streamer[B]): Streamer[B]

  // ---------------------------------------------------------------------------
  def toViewBased    : Streamer[A]
  def toIteratorBased: Streamer[A]

  // ---------------------------------------------------------------------------
  def asInstanceOfIteratorStreamer: IteratorStreamer[A] = this.asInstanceOf[IteratorStreamer[A]] // saves repeating A at call site + import

  // ===========================================================================
  // TODO: t210116154010 - ideally would need to curry type args...
  final def mapKeys  [K, V, K2    ](f: K => K2)(implicit ev: A <:< (K, V)): Streamer[(K2, V )] = this.map { x => f(x._1) ->   x._2 }
  final def mapValues[K, V,     V2](f: V => V2)(implicit ev: A <:< (K, V)): Streamer[(K , V2)] = this.map { x =>   x._1  -> f(x._2) }

  // ---------------------------------------------------------------------------
  final def filterNot(p: A => Boolean): Streamer[A] = filter(!p(_))
  final def nonEmpty: Boolean = !isEmpty
}

// ===========================================================================
