package gallia
package heads

import heads.grouping.HasAs
import data.multiple.Streamer
import actions._
import actions.ActionsOthers._
import actions.ActionsZZ.{Take, Drop, AddIndex}
import actions.ActionsThns._
import actions.ActionsCustoms.{CustomZZ, CustomZU}
import actions.ActionsZZMerging.MergeObjsVle
import actions.ActionsZZFiltering.{TakeWhile, DropWhile}

// ===========================================================================
class HeadZ private[gallia] ( // TODO: t210121105809 - rename to HeadS and generally change occurrences of "z" into "s"
      protected[gallia] override val nodeId : NodeId,
      protected[gallia] override val handler: Handler)
    extends heads.common.HeadCommon[HeadZ]

      with    HeadZOut

      with    HeadZFiltering
      with    HeadZSquashing
      with    HeadZFors
      with    HeadZAccessors
      with    HeadZOperations
      with    reducing.HeadZReducing
      with    sorting .HeadZSorting
      with    pivoting.HeadZPivoting
      with    grouping.HeadZGrouping
      with    grouping.HeadZAggregations
      with    merging .HeadZMerging {

  private[gallia] type Self = HeadZ
  private         val  self = this

  // ---------------------------------------------------------------------------
  override def toString: String = nodeId // TODO

  // ---------------------------------------------------------------------------
  private[gallia] def _modifyUnderlyingStreamer(f: Streamer[Obj] => Streamer[Obj]): HeadZ = self ::+ ModifyUnderlyingStreamer(f)

  // ===========================================================================
  @deprecated("bypasses the 210205063004 mechanism")
  def  forceAObjs : AObjs = end().runz().forceData2(_.aobjs)
  def _forceResult: AObjs = end().runz().forceData2(_.aobjs) // meant for tests only

  // ---------------------------------------------------------------------------
  protected[gallia] def rewrap      (newHeadId: NodeId): Self            = new HeadZ(newHeadId, handler)
  protected[gallia] def rewrapWithAs(newHeadId: NodeId): Self with HasAs = new HeadZ(newHeadId, handler) with HasAs

    private[heads] def updateAs1(key: Key) = { handler.updateAs(nodeId, key); this } // TODO: generalize

  // ---------------------------------------------------------------------------
  def headU        (newNodeId: NodeId): HeadU    = new HeadU   (newNodeId, handler)
  def headV[T: WTT](newNodeId: NodeId): HeadV[T] = new HeadV[T](newNodeId, handler)

  // ---------------------------------------------------------------------------
  final          protected[heads] def ::+ (action: ActionZZ): Self = handler.chainzz(self)(action)
  final override protected[heads] def  :+ (action: ActionUU): Self = this ::+ UWrapper(action)

    // ===========================================================================
    private case class UWrapper(u: ActionUU) extends ActionZZ {
      def vldt   (in: Cls): Errs = u. vldt(in)
      def _meta  (in: Cls): Cls  = u._meta(in)
      def atomzzs(ctx: NodeMetaContext): AtomZZs =
        u .atomuus(ctx)
          .map(gallia.atoms._UWrapper.apply) }

  // ===========================================================================
  private[heads] def zz      (action: ActionZZ): HeadZ            = handler.chainzz      (self)(action)
  private[heads] def zzWithAs(action: ActionZZ): HeadZ with HasAs = handler.chainzzWithAs(self)(action) // used by aggregations
  private[heads] def allowAs (target: Key)     : HeadZ with HasAs = handler.chainzzWithAs(self)(AsRename(Ren.from(target)))

  // ---------------------------------------------------------------------------
  private[heads] def zzToz         (that: HeadZ)   (action: ActionZzToZ): HeadZ = handler.joinZz2z(self, that)(action)
  private[heads] def zvToz[T : WTT](that: HeadV[T])(action: ActionZvToZ): HeadZ = handler.joinZv2z(self, that)(action)

  // ---------------------------------------------------------------------------
  private[heads] def zu        (action: ActionZU): HeadU    = handler.chainzu   (self)(action)
  private[heads] def zv[V: WTT](action: ActionZV): HeadV[V] = handler.chainzv[V](self)(action)
  private[heads] def zo        (action: ActionZO): HeadZ    = handler.chainzo   (self)(action)

  // ===========================================================================
  private[heads] def mergeObjsVle[V: WTT](that: HeadV[V])(f: (Objs, V) => Objs): HeadZ = zvToz(that)(MergeObjsVle((z: Objs, v: Vle) => f(z, v.asInstanceOf[V]) ))

  // ===========================================================================
  def     map[V: WTT](f: HeadU => HeadV[V])(implicit di: DI): HeadV[List[V]] = zv(ActionsZZ.    MapU2V(typeNode[V], f))
  def     map        (f: HeadU => HeadU)                    : Self           = zz(ActionsZZ.    MapU2U(f)) // TODO: misnomer... endo-map?

  def flatMap0       (f: HeadU => HeadZ)                    : Self           = zz(ActionsZZ.FlatMap   (f)) // TODO: still makes sense (or use map+flattenBy)?

  // ===========================================================================
  def collectValues[V: WTT](f: HeadU => HeadV[V]): List[V] = map(f).forceValue

  // TODO: t201016114103 - or as "toList"?
  def collectObjects: List[Obj] = end().runz().forceData2(_.data.toListAndTrash)
  def collectObjectz:      Objs = collectObjects.pipe(Objs.from)

  // ===========================================================================
  def      populateDataClasses[DC: WTT] = ??? // TODO: t210117105638 - see 210117105638@v
  def forcePopulateDataClasses[DC: WTT] = ???

  // ===========================================================================
  // for consistency only, shouldn't really use outside of tests (use .pipe instead)
  def thn        (f: Self => Self    ): Self     = zz(ThnZZ(f))
  def thn        (f: Self => HeadU   ): HeadU    = zu(ThnZU(f))
  def thn[V: WTT](f: Self => HeadV[V]): HeadV[V] = zv(ThnZV(f))

  // ===========================================================================
  // custom: consider using 220412171654 instead (data classes
      // TODO: these will be very affected by t210104164036

    /** "Computer, I know better than you" */ def customS2SDataOnly(                  data: List[Obj] => List[Obj]): Self = self ::+ CustomZZ.from(meta = c => c, data)
    /** "Computer, I know better than you" */ def customZ2Z        (meta: Cls => Cls, data:      Objs =>      Objs): Self = self ::+ new CustomZZ(meta, data)
    /** "Computer, I know better than you" */ def customS2S        (meta: Cls => Cls, data: List[Obj] => List[Obj]): Self = self ::+ CustomZZ.from(meta, data)
    /** "Computer, I know better than you" */ def custom           (meta: Cls => Cls, data: List[Obj] => List[Obj]): Self = self ::+ CustomZZ.from(meta, data)

    /** "Computer, I know better than you" */ def custom(x: ObjsToObj) : HeadU =       zu(CustomZU.from(x.meta,           x.data))
    /** "Computer, I know better than you" */ def custom(x: ObjToObjs) : HeadZ = self ::+ CustomZZ.from(x.meta, _.flatMap(x.data))
    /** "Computer, I know better than you" */ def custom(x: ObjsToObjs): HeadZ = self ::+ CustomZZ.from(x.meta,           x.data )

  // ===========================================================================
  def assertDataUnsafeZ(pred: Objs => Boolean): Self = self ::+
    actions.ActionsAsserts.AssertDataUnsafeZ(pred) // beware costly operation (distribution), eg size

  // ===========================================================================
  def logProgress                                         : Self = logProgress(n = 1000, _ => "")
  def logProgress(n:           Int                       ): Self = logProgress(n, _ => "")
  def logProgress(n:           Int , msg: String         ): Self = logProgress(n, _ => msg)
  def logProgress(n:           Int , debug: Obj => String): Self = logProgress(Some(n), debug)

  def logProgress(nOpt: Option[Int]                      ): Self = logProgress(nOpt, _ => "")
  def logProgress(nOpt: Option[Int], msg: String         ): Self = logProgress(nOpt, _ => msg)
  def logProgress(nOpt: Option[Int], debug: Obj => String): Self = zz(ActionsZZ.LogProgress(nOpt, debug))

  // ===========================================================================
  def checkpoint(base: String)                        : Self2 = checkpoint(s"${base}.checkpoint.schema.json", s"${base}.checkpoint.data.jsonl.gz")  
  def checkpoint(schemaPath: String, dataPath: String): Self2 = zz(CheckpointZ(schemaPath, dataPath))  

  // ===========================================================================
  // TODO: add first/last/sample of N

  def inspect()           : Self = self ::+ InspectZ(None     , abort = false)
  def inspect(msg: String): Self = self ::+ InspectZ(Some(msg), abort = false)

  @aptus.fordevonly("pretty ugly...") def inspectAndAbort: Self = self ::+ InspectZ(None,      abort = true)

  // ===========================================================================
  def toViewBased    : HeadZ = self ::+ ToViewBased
  def toIteratorBased: HeadZ = self ::+ ToIteratorBased

  def _toBased(inMemory: Boolean): HeadZ = if (inMemory) toViewBased else toIteratorBased // mostly for tests

  // ===========================================================================
  // TODO: add more common ones
  // TODO: t210117110015 - move to common (need to abstract ForX...)

  def retainFirst            : Self2 = forKey(_.firstKey).thn(_ retain _)
  def renameSoleKey(to: KeyW): Self2 = forKey(_.soleKey) .thn(_.rename(_).to(to))
  
  def removeRecursivelyIfValue(value: String): Self2 = forLeafPaths { _.removeIfValueFor(_).is(value) } 

  @PartialTypeMatching
    def convertToIntRecursively    : Self2 = forLeafPaths(_.convert(_).toInt)
    def convertToDoubleRecursively : Self2 = forLeafPaths(_.convert(_).toDouble)
    def convertToBooleanRecursively: Self2 = forLeafPaths(_.convert(_).toBoolean)

  // ===========================================================================  
  def size: HeadV[Int] = zv(Size)

    // TODO: t210127164715 - if not top-level? (check missing will return size = 0)
    def isEmpty : HeadV[Boolean] = size.mapV(_ == 0)
    def nonEmpty: HeadV[Boolean] = size.mapV(_ >  0)

    def hasSize(value: Int) = size.mapV(_ == value)
    
    // ---------------------------------------------------------------------------
    def forceSize     = size    .forceValue
    def forceIsEmpty  =  isEmpty.forceValue
    def forceNonEmpty = nonEmpty.forceValue
    
  // ===========================================================================
  // TODO: t210117112314
  def head: HeadU = take(1).force.one
  //def last: HeadU = takeRight(1).force.one

    // TODO: t210117112314
    @Distributivity def take    (n: Int): HeadZ = zz(Take(n))
    @Distributivity def takeLeft(n: Int): HeadZ = zz(Take(n))

    @Distributivity def drop    (n: Int): HeadZ = zz(Drop(n))
    @Distributivity def dropLeft(n: Int): HeadZ = zz(Drop(n))

      @Distributivity def take    (n: Option[Int]): HeadZ = n.map(take).getOrElse(self)
      @Distributivity def takeLeft(n: Option[Int]): HeadZ = n.map(take).getOrElse(self)

      @Distributivity def drop    (n: Option[Int]): HeadZ = n.map(drop).getOrElse(self)
      @Distributivity def dropLeft(n: Option[Int]): HeadZ = n.map(drop).getOrElse(self)

  @Distributivity def takeWhile(pred: HeadU => HeadV[Boolean]): HeadZ = zz(TakeWhile(pred))
  @Distributivity def dropWhile(pred: HeadU => HeadV[Boolean]): HeadZ = zz(DropWhile(pred))

      //def takeRight(n: Int): HeadZ = ???
      //def dropRight(n: Int): HeadZ = ???

      //def reverse          : HeadZ = ???
      //def randomize        : HeadZ = ???

  @Distributivity def addIndex          : HeadS = zz(AddIndex(key = _index,   oneBased = false))
  @Distributivity def addIndex(as: KeyW): HeadS = zz(AddIndex(key = as.value, oneBased = false))
  @Distributivity def addRank           : HeadS = zz(AddIndex(key = _rank,    oneBased = true ))
  @Distributivity def addRank(as: KeyW) : HeadS = zz(AddIndex(key = as.value, oneBased = true ))

  // ===========================================================================
  // TODO: t210205122151
//def toRddBased     : Self = ???
//def toInMemoryBased: Self = ??? // rename...
}

// ===========================================================================
object HeadZ extends HeadZIn {
  lazy val Dummy: HeadZ = """[{"foo":1, "bar": "baz1"}, {"foo":2, "bar": "baz2"}]""".stream()

  // ---------------------------------------------------------------------------
  lazy val Empty             : HeadZ =  empty(Cls.Line)
       def empty(schema: Cls): HeadZ =  AObjs.empty(schema)

  // ---------------------------------------------------------------------------
  val DefaultOutputFile = "/tmp/out.jsonl.gz"
}

// ===========================================================================
