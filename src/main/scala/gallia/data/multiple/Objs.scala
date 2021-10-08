package gallia.data.multiple

import aptus.Seq_
import scala.reflect.ClassTag

// ===========================================================================
case class Objs private (  // TODO: two versions, see t210104164036
    private[gallia] val values: Streamer[Obj])
       extends ObjsOperations
       with    ObjsAggregations
       with    ObjsMerging
       with    ObjsOut {

    override def toString: String = formatDefault

    // ---------------------------------------------------------------------------
    private def _rewrap(values: Streamer[Obj]): Objs = Objs.build(values)

    // ===========================================================================
    private[gallia] def _modifyUnderlyingStreamer(f: Streamer[Obj] => Streamer[Obj]): Objs = _rewrap(f(values)) // eg to modify spark RDD

    private[gallia] def _asListBased: Objs = values.asListBased.pipe(_rewrap)

    // ===========================================================================
    def     mapToStreamer[A: ClassTag](f: Obj =>      A ): Streamer[A] = values.    map(f)
    def flatMapToStreamer[A: ClassTag](f: Obj => Coll[A]): Streamer[A] = values.flatMap(f)

    def consume: Iterator[Obj] = values.iterator // more for intent; FIXME: check usage ok througout... - relates to t210115104555

    /** in this case wrapper not to be reused */
    def toListAndTrash: List[Obj] = values.toList // no mercy.

    // ---------------------------------------------------------------------------
    def reduce(op: (Obj, Obj) => Obj): Obj = values.reduce(op)

    // ===========================================================================
    /*@Narrow */
    def map      (f: Obj =>      Obj ): Objs = values.    map(f).pipe(_rewrap)
    def flatMap  (f: Obj => Coll[Obj]): Objs = values.flatMap(f).pipe(_rewrap)

    def filter(p: Obj => Boolean):        Objs = values.filter(p).pipe(_rewrap)
  //def find  (p: Obj => Boolean): Option[Obj] = values.find  (p) // TODO: t210204105730 - offer streamer find?

    // ===========================================================================
    def force = new { def one: Obj = toListAndTrash.force.one }

    def  isEmpty: Boolean = values. isEmpty
    def nonEmpty: Boolean = values.nonEmpty

    def size: Int = values.size

    def take(n: Option[Int]): Objs = n.map(values.take).getOrElse(values).pipe(_rewrap)

    final def take(n: Int): Objs = take(Some(n))
  }

  // ===========================================================================
  object Objs {
    private[gallia] def build(values: Streamer[Obj]) = new Objs(values)

    // ---------------------------------------------------------------------------
    def splat(value1: Obj, more: Obj*): Objs = from(value1 +: more.toList)
    def from(values: Seq [Obj])       : Objs = from(values.toList)
    def from(values: List[Obj])       : Objs = Streamer.fromList(values).pipe(Objs.build)
  }

// ===========================================================================
