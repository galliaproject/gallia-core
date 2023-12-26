package gallia
package heads

import aptus.Seq_
import actions.ActionsOthers.{MapV2V, CombineVV, DressUp}

// ===========================================================================
class HeadV[T: WTT /* will be Vle (Any) for data phase */] private[gallia] ( // TODO: t220916113454 - separate HeadV[T] from HeadV[Seq[U]]
      override val nodeId : NodeId,
      override val handler: Handler)
    extends Head[HeadV[T]] 
    with    HeadVOut[T] {
  private[gallia] type Self = HeadV[T]
  private         val  self = this

  // ---------------------------------------------------------------------------
  private[heads] def vo(action: ActionVO): HeadV[T] = handler.chainvo(this)(action)
  private[heads] def vu(action: ActionVU): HeadU    = handler.chainvu(this)(action)
  private[heads] def vz(action: ActionVZ): HeadZ    = handler.chainvz(this)(action)

  // ---------------------------------------------------------------------------
  def headU(newNodeId: NodeId): HeadU = new HeadU(newNodeId, handler)
  def headZ(newNodeId: NodeId): HeadZ = new HeadZ(newNodeId, handler)

  // ---------------------------------------------------------------------------
  override def toString: String = nodeId // TODO

  // ---------------------------------------------------------------------------
  @deprecated("bypasses the 210205063004 mechanism")
  def forceValue: T = end().runv().forceData1b // TODO: or as forceData, forceResult?

  // ---------------------------------------------------------------------------
  private[gallia] def rewrap[T2: WTT](newNodeId: NodeId): HeadV[T2] = new HeadV[T2](newNodeId, handler)

  // ---------------------------------------------------------------------------
  def dressUp(key: KeyW): HeadO = DressUp[T](key.value).pipe(vu) // "dress up" because naked value otherwise

  // ===========================================================================
  def mapV [         T2: WTT](f: T  => T2)                            : HeadV[    T2 ] = typeNode[T2].pipe { tn => handler.chainvv(this)(MapV2V(tn, (x: Any) => f(x.asInstanceOf[T]) )) }
  def mapVs[T1: WTT, T2: WTT](f: T1 => T2)(implicit ev: T <:< Seq[T1]): HeadV[Seq[T2]] = mapV[Seq[T2]](_.map(f)) // worth keeping? - TODO: subclass rather?

  // ---------------------------------------------------------------------------
  def toInt   (implicit ev: T =:= Double): HeadV[Int]    = mapV(_.toInt)
  def toDouble(implicit ev: T =:= Int)   : HeadV[Double] = mapV(_.toDouble)

  // ---------------------------------------------------------------------------
  // TODO: t220916113454 - separate HeadV[T] from HeadV[Seq[U]]

    @gallia.IntSize def count(implicit ev1: T <:< Iterable[_]): HeadV[Int] = mapV(_.size)

    def distinct [U : WTT](implicit ev1: T <:< Iterable[       U ]): HeadV[Seq[U]] = mapV(_.toSeq.distinct)
    def flattened[U : WTT](implicit ev1: T <:< Iterable[Option[U]]): HeadV[Seq[U]] = mapV(_.toSeq.flatten)

    def min  [N : WTT](implicit ev1: T <:< Iterable[N], ev2: Numeric[N]): HeadV[N]   = mapV(_.min)
    def max  [N : WTT](implicit ev1: T <:< Iterable[N], ev2: Numeric[N]): HeadV[N]   = mapV(_.max)
    def range[N : WTT](implicit ev1: T <:< Iterable[N], ev2: Numeric[N]): HeadV[N]   = mapV(x => ev2.minus(x.max, x.min))

    def sum   [N : WTT](implicit ev1: T <:< Iterable[N], ev2: Numeric[N]): HeadV[N]      = mapV(_.sum)
    def mean  [N : WTT](implicit ev1: T <:< Iterable[N], ev2: Numeric[N]): HeadV[Double] = mapV(_.toSeq.mean)
    def median[N : WTT](implicit ev1: T <:< Iterable[N], ev2: Numeric[N]): HeadV[Double] = mapV(_.toSeq.median)
    def stdev [N : WTT](implicit ev1: T <:< Iterable[N], ev2: Numeric[N]): HeadV[Double] = mapV(_.toSeq.stdev)

    //TODO: IQR? stats? see ReducingType

  // ---------------------------------------------------------------------------
  def combine[T2: WTT](that: HeadV[T2]) = new _Combine(that)

    class _Combine[T2: WTT] private[HeadV] (that: HeadV[T2]) {
      def using[T3: WTT](f: (T, T2) => T3): HeadV[T3] = handler.joinVv2v[T, T2, T3](self, that)( // TODO: as "reduce"?
        CombineVV(typeNode[T], typeNode[T2], result = typeNode[T3],
          (x: Any, y: Any) => f(x.asInstanceOf[T], y.asInstanceOf[T2]))) }

    // ---------------------------------------------------------------------------
    // combining String
    def concatenate(that: HeadV[_])(implicit ev1: T <:< String): HeadV[String] = combine(that).using(_ + _.toString)
    def concatenate(that: Any)     (implicit ev1: T <:< String): HeadV[String] = concatenate(headV[String](that.toString))

    def concatenateStrings             (implicit ev1: T <:< Iterable[String]): HeadV[String] = mapV(_.reduceLeft(_ + _))
    def concatenateStrings(sep: String)(implicit ev1: T <:< Iterable[String]): HeadV[String] = mapV(_.reduceLeft(_ + sep + _))

    // ---------------------------------------------------------------------------
    // need to provide common combinations, unless there's a way to reconcile two different Numeric[_] somehow? (t220920111256)

    // FIXME: t220920155343 - see https://users.scala-lang.org/t/ambiguous-reference-to-overloaded-definition-with/8817
    def plus(that: HeadV[Int])   (implicit ev: T =:= Int)                           : HeadV[Int]    = combine(that).using(_ + _)
  //def plus(that: HeadV[Int])   (implicit ev: T =:= Double, d1: DI)                : HeadV[Double] = combine(that).using(_ + _)
  //def plus(that: HeadV[Double])(implicit ev: T =:= Int,    d1: DI, d2: DI)        : HeadV[Double] = combine(that).using(_ + _)
    def plus(that: HeadV[Double])(implicit ev: T =:= Double, d1: DI, d2: DI, d3: DI): HeadV[Double] = combine(that).using(_ + _)

    def minus(that: HeadV[Int])   (implicit ev: T =:= Int)                           : HeadV[Int]    = combine(that).using(_ - _)
  //def minus(that: HeadV[Int])   (implicit ev: T =:= Double, d1: DI)                : HeadV[Double] = combine(that).using(_ - _)
  //def minus(that: HeadV[Double])(implicit ev: T =:= Int,    d1: DI, d2: DI)        : HeadV[Double] = combine(that).using(_ - _)
    def minus(that: HeadV[Double])(implicit ev: T =:= Double, d1: DI, d2: DI, d3: DI): HeadV[Double] = combine(that).using(_ - _)

    def times(that: HeadV[Int])   (implicit ev: T =:= Int)                           : HeadV[Int]    = combine(that).using(_ * _)
  //def times(that: HeadV[Int])   (implicit ev: T =:= Double, d1: DI)                : HeadV[Double] = combine(that).using(_ * _)
  //def times(that: HeadV[Double])(implicit ev: T =:= Int,    d1: DI, d2: DI)        : HeadV[Double] = combine(that).using(_ * _)
    def times(that: HeadV[Double])(implicit ev: T =:= Double, d1: DI, d2: DI, d3: DI): HeadV[Double] = combine(that).using(_ * _)

    def dividedBy(that: HeadV[Int])   (implicit ev: T =:= Int)                           : HeadV[Int]    = combine(that).using(_ / _)
  //def dividedBy(that: HeadV[Int])   (implicit ev: T =:= Double, d1: DI)                : HeadV[Double] = combine(that).using(_ / _)
  //def dividedBy(that: HeadV[Double])(implicit ev: T =:= Int,    d1: DI, d2: DI)        : HeadV[Double] = combine(that).using(_ / _)
    def dividedBy(that: HeadV[Double])(implicit ev: T =:= Double, d1: DI, d2: DI, d3: DI): HeadV[Double] = combine(that).using(_ / _)

    // ---------------------------------------------------------------------------
    def plus(that: Int)   (implicit ev: T =:= Int)                           : HeadV[Int]    = combine(headV(that)).using(_ + _)
  //def plus(that: Int)   (implicit ev: T =:= Double, d1: DI)                : HeadV[Double] = combine(headV(that)).using(_ + _)
  //def plus(that: Double)(implicit ev: T =:= Int,    d1: DI, d2: DI)        : HeadV[Double] = combine(headV(that)).using(_ + _)
    def plus(that: Double)(implicit ev: T =:= Double, d1: DI, d2: DI, d3: DI): HeadV[Double] = combine(headV(that)).using(_ + _)

    def minus(that: Int)   (implicit ev: T =:= Int)                           : HeadV[Int]    = combine(headV(that)).using(_ - _)
  //def minus(that: Int)   (implicit ev: T =:= Double, d1: DI)                : HeadV[Double] = combine(headV(that)).using(_ - _)
  //def minus(that: Double)(implicit ev: T =:= Int,    d1: DI, d2: DI)        : HeadV[Double] = combine(headV(that)).using(_ - _)
    def minus(that: Double)(implicit ev: T =:= Double, d1: DI, d2: DI, d3: DI): HeadV[Double] = combine(headV(that)).using(_ - _)

    def times(that: Int)   (implicit ev: T =:= Int)                           : HeadV[Int]    = combine(headV(that)).using(_ * _)
  //def times(that: Int)   (implicit ev: T =:= Double, d1: DI)                : HeadV[Double] = combine(headV(that)).using(_ * _)
  //def times(that: Double)(implicit ev: T =:= Int,    d1: DI, d2: DI)        : HeadV[Double] = combine(headV(that)).using(_ * _)
    def times(that: Double)(implicit ev: T =:= Double, d1: DI, d2: DI, d3: DI): HeadV[Double] = combine(headV(that)).using(_ * _)

    def dividedBy(that: Int)   (implicit ev: T =:= Int)                           : HeadV[Int]    = combine(headV(that)).using(_ / _)
  //def dividedBy(that: Int)   (implicit ev: T =:= Double, d1: DI)                : HeadV[Double] = combine(headV(that)).using(_ / _)
  //def dividedBy(that: Double)(implicit ev: T =:= Int,    d1: DI, d2: DI)        : HeadV[Double] = combine(headV(that)).using(_ / _)
    def dividedBy(that: Double)(implicit ev: T =:= Double, d1: DI, d2: DI, d3: DI): HeadV[Double] = combine(headV(that)).using(_ / _)
}

// ===========================================================================
