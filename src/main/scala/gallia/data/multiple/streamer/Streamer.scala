package gallia
package data.multiple.streamer

import scala.reflect.{ClassTag => CT}
import java.io.Closeable

import aptus.aptutils.IteratorUtils

import heads.merging.MergingData._

// ===========================================================================
trait Streamer[A] { // note: not necessarily bothering with genericity (in the general sense) at this point, mostly on abstracting relevant scala/spark collections
    // TODO:
    // - bother with variance?
    // - t210115095741 - performance: give easy access to .par where applicable; consider Iterator counterpart (look into https://github.com/tim-group/iterata)

    // ---------------------------------------------------------------------------
    val tipe: StreamerType

    // ---------------------------------------------------------------------------
    final override def equals(that: Any): Boolean = egal(that.asInstanceOf[Streamer[A]])

    // ---------------------------------------------------------------------------
    protected def egal(that: Streamer[A]): Boolean // TODO: only for tests?

    def iteratorAndCloseable: (Iterator[A], Closeable) // TODO: t210205121008

    def iterator: Iterator[A]
  //def toView  : ViewRepr[A] - TODO: t210315114618 - causes odd compilation issue with gallia-spark, to investigate
    def toList  : List    [A]

    // CT is a requirement of Spark RDD (easier to include it here than work around it); TODO: Coll as well?
    def     map[B: CT](f: A =>      B ): Streamer[B]
    def flatMap[B: CT](f: A => Coll[B]): Streamer[B]

    def filter(p: A => Boolean): Streamer[A]
  //def find  (p: A => Boolean): Option  [A] // TODO: t210204105730 - offer streamer find?

    def size: Int

    def  isEmpty: Boolean

    @Distributivity // TODO: t210117112314 - maybe sample?
    def take(n: Int): Streamer[A]
    def drop(n: Int): Streamer[A]

    // ===========================================================================
    def reduce(op: (A, A) => A): A

    // ---------------------------------------------------------------------------
    def sort     (ctag: CT[A], ord: Ordering[A])           : Streamer[A] = sortBy(ctag, ord)(identity)
    def sortBy[K](ctag: CT[K], ord: Ordering[K])(f: A => K): Streamer[A]

    // ---------------------------------------------------------------------------
    def distinct: Streamer[A]

    /** think uniq command */
    def distinctByAdjacency: Streamer[A] = ??? // t210117113705 - implement

    // ---------------------------------------------------------------------------
    def groupByKey[K: CT, V: CT](implicit ev: A <:< (K, V)): Streamer[(K, List[V])]

    // ===========================================================================
    def union[B >: A : CT](that: Streamer[B]): Streamer[B] //TODO: use implicit ev

    // ---------------------------------------------------------------------------
    def join   [K: CT, V: CT](joinType: JoinType, combiner: (V, V) => V)(that: Streamer[(K, V)])(implicit ev: A <:< (K, V)): Streamer[              V]
    def coGroup[K: CT, V: CT](joinType: JoinType                       )(that: Streamer[(K, V)])(implicit ev: A <:< (K, V)): Streamer[(K, (Iterable[V], Iterable[V]))]

    // ===========================================================================
    @aptus.nonfinl private[streamer] def asMeBased[B >: A : CT](that: Streamer[B]): Streamer[B] =
      that.asListBased // overriden for RDD

    final def asListBased: Streamer[A] = toList.pipe(Streamer.fromList)

    // ===========================================================================
    // TODO: t210116154010 - ideally would need to curry type args...
    final def mapKeys  [K, V, K2    ](f: K => K2)(implicit ev: A <:< (K, V)): Streamer[(K2, V )] = this.map { x => f(x._1) ->   x._2 }
    final def mapValues[K, V,     V2](f: V => V2)(implicit ev: A <:< (K, V)): Streamer[(K , V2)] = this.map { x =>   x._1  -> f(x._2) }

    // ---------------------------------------------------------------------------
    final def filterNot(p: A => Boolean): Streamer[A] = filter(!p(_))
    final def nonEmpty: Boolean = !isEmpty
  }

  // ===========================================================================
  object Streamer {
    val Empty = fromList(Nil)

    // ---------------------------------------------------------------------------
    def fromView    [A](data: ViewRepr[A]): Streamer[A] = new ViewStreamer(data)
    def fromList    [A](data: List[A])    : Streamer[A] = new ViewStreamer(data.view)
    def fromIterator[A](data: Iterator[A]): Streamer[A] = new IteratorStreamer(data) // must close separately (eg to read first N lines)

    def fromIterator[A](iter: aptus.Closeabled[Iterator[A]]): Streamer[A] = fromIterator(iter.underlying, iter.cls)
    def fromIterator[A](iter: aptus.CloseabledIterator [A]) : Streamer[A] = fromIterator(iter.underlying, iter.cls)

    def fromIterator[A](pair: (Iterator[A], Closeable)): Streamer[A] = { // for now... (TODO see t210116154537 as part of t210115104555)
      gallia.closeables += pair._2
      fromIterator(IteratorUtils.selfClosing(pair._1, pair._2))
    }
  }

// ===========================================================================

