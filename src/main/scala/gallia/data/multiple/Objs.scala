package gallia
package data
package multiple

import aptus.Seq_
import streamer.{IteratorStreamer, StreamerType, ViewStreamer}

// ===========================================================================
case class Objs private (  // TODO: two versions, see t210104164036
    private[gallia] val values: Streamer[Obj])
       extends ObjsOperations
       with    ObjsSorting
       with    ObjsAggregations
       with    ObjsMerging
       with    ObjsOut {

    override def toString: String = formatDefault

    // ---------------------------------------------------------------------------
    private def _rewrap(values: Streamer[Obj]): Objs = Objs.build(values)

    // ---------------------------------------------------------------------------
    private[gallia] def isViewBased    : Boolean = values.tipe.isViewBased
    private[gallia] def isIteratorBased: Boolean = values.tipe.isIteratorBased
    private[gallia] def isRDDBased     : Boolean = values.tipe.isRDDBased

    // ===========================================================================
    private[gallia] def _modifyUnderlyingStreamer(f: Streamer[Obj] => Streamer[Obj]): Objs = _rewrap(f(values)) // eg to modify spark RDD

    // ---------------------------------------------------------------------------
    def _toViewBased    : Objs = values.toViewBased    .pipe(_rewrap)
    def _toIteratorBased: Objs = values.toIteratorBased.pipe(_rewrap)

    // ---------------------------------------------------------------------------
    def _toRddBased: Objs =
      values.tipe match {
        case StreamerType.ViewBased     => values.asInstanceOf[ViewStreamer[Obj]].pipe(Objs.toRddHack).getOrElse(aptus.illegalState("220721100024 - no spark context registered"))
        case StreamerType.IteratorBased => aptus.illegalState(data.multiple.CantMixIteratorAndRddProcessing)
        case StreamerType.RDDBased      => this }

    // ===========================================================================
    def     mapToStreamer[A: CWTT](f: Obj =>      A ): Streamer[A] = values.    map(f)
    def flatMapToStreamer[A: CWTT](f: Obj => Coll[A]): Streamer[A] = values.flatMap(f)

    // ---------------------------------------------------------------------------
    def consumeSelfClosing:                 Iterator[Obj] = values.selfClosingIterator
    def closeabledIterator: aptus.CloseabledIterator[Obj] = values. closeabledIterator

    // ---------------------------------------------------------------------------
    /** in this case wrapper not to be reused */
    def toListAndTrash: List[Obj] = values.toList // no mercy.

    // ---------------------------------------------------------------------------
    //def reduce(op: (Obj, Obj) => Obj): Obj = values.reduce(op) - not used?

    // ===========================================================================
    /*@Narrow */
    def     map(f: Obj =>      Obj ): Objs = values.    map(f).pipe(_rewrap)
    def flatMap(f: Obj => Coll[Obj]): Objs = values.flatMap(f).pipe(_rewrap)

    def filter(p: Obj => Boolean):        Objs = values.filter(p).pipe(_rewrap)
    def find  (p: Obj => Boolean): Option[Obj] = values.find  (p)

    // ===========================================================================
    def force = new _Force()
      final class _Force private[Objs] { def one: Obj = toListAndTrash.force.one }

    // ---------------------------------------------------------------------------
    def  isEmpty: Boolean = values. isEmpty
    def nonEmpty: Boolean = values.nonEmpty

    def size: Int = values.size

    def take(n: Option[Int]): Objs = n.map(values.take).getOrElse(values).pipe(_rewrap)
    def drop(n: Option[Int]): Objs = n.map(values.drop).getOrElse(values).pipe(_rewrap)

    final def take(n: Int): Objs = take(Some(n))
    final def drop(n: Int): Objs = drop(Some(n))

    final def takeWhile(p: Obj => Boolean): Objs = values.takeWhile(p).pipe(_rewrap)
    final def dropWhile(p: Obj => Boolean): Objs = values.dropWhile(p).pipe(_rewrap)
  }

  // ===========================================================================
  object Objs {
    private[gallia] def build(values: Streamer[Obj]) = new Objs(values)

    // ---------------------------------------------------------------------------
    def splat(value1: Obj, more: Obj*)       : Objs = from(value1 +: more.toList)

    // ---------------------------------------------------------------------------
    lazy val empty: Objs = from(Nil)

    // ---------------------------------------------------------------------------
    def from(values: List[Obj])                 : Objs = Objs.build(    ViewStreamer.from(values))
    def from(data: DataRegenerationClosure[Obj]): Objs = Objs.build(IteratorStreamer.from(data))

    // ---------------------------------------------------------------------------
    def streamFromFile(path: String): Objs = ObjsIn.streamFromFile(path)

    // ===========================================================================
    private[gallia] def toRddHack(streamer: ViewStreamer[Obj]): Option[Objs] =
      gallia.Hacks.sparkRddHack.getValueOpt() // see 220721104754 in gallia-spark
        .map {
          _ .toRddStreamer(streamer)
            .pipe(Objs.build) }
  }

// ===========================================================================
