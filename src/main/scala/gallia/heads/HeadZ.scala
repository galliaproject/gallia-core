package gallia
package heads

import heads.grouping.HasAs
import data.multiple.Streamer
import actions._
import actions.ActionsOthers._
import actions.ActionsZZ._
import actions.ActionsThns._
import actions.ActionsCustoms.CustomZZ

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
      with    pivoting.HeadZUnarraying
      with    grouping.HeadZGrouping
      with    grouping.HeadZAggregations
      with    merging .HeadZMerging {

  private[gallia] type Self = HeadZ
  private    val  self = this

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
  private[heads] def zz      (action: ActionZZ): HeadZ            = handler.chainzz      (self)(action)
  private[heads] def zzWithAs(action: ActionZZ): HeadZ with HasAs = handler.chainzzWithAs(self)(action) // used by aggregations
  private[heads] def allowAs (target: Key)     : HeadZ with HasAs = handler.chainzzWithAs(self)(AsRename(Ren.from(target)))

  // ---------------------------------------------------------------------------
  private[heads] def zzToz(that: HeadZ)(action: ActionZzToZ): HeadZ = handler.joinZz2z(self, that)(action)

  // ---------------------------------------------------------------------------
  private[heads] def zu        (action: ActionZU): HeadU    = handler.chainzu(self)(action)
  private[heads] def zv[V: WTT](action: ActionZV): HeadV[V] = handler.chainzv(self)(action)
  private[heads] def zo        (action: ActionZO): HeadZ    = handler.chainzo(self)(action)

  // ===========================================================================
  def     map[V: WTT](f: HeadU => HeadV[V])(implicit di: DI): HeadV[List[V]] = zv(ActionsZZ.    MapU2V(typeNode[V], f))
  def     map        (f: HeadU => HeadU)                    : Self           = zz(ActionsZZ.    MapU2U(f)) // TODO: misnomer... endo-map?

  def flatMap0       (f: HeadU => HeadZ)                    : Self           = zz(ActionsZZ.FlatMap   (f)) // TODO: still makes sense (or use map+flattenBy)?

  // ===========================================================================
  def collectValues[V: WTT](f: HeadU => HeadV[V]): List[V] = map(f).forceValue

  // TODO: t201016114103 - or as "toList"?
  def collectObjects: List[Obj] = end.runz().forceData2(_.data.toListAndTrash)
  def collectObjectz:      Objs = collectObjects.pipe(Objs.from)

  // ===========================================================================
  def      populateDataClasses[DC: WTT] = ??? // TODO: t210117105638 - see 210117105638@v
  def forcePopulateDataClasses[DC: WTT] = ???

  // ===========================================================================
  // for consistency only, shouldn't really use outside of tests (use .pipe instead)
  def thn        (f: Self => Self    ): Self     = zz(ThnZZ(f))
  def thn        (f: Self => HeadU   ): HeadU    = zu(ThnZU(f))
  def thn[V: WTT](f: Self => HeadV[V]): HeadV[V] = zv(ThnZV(f))

  // ---------------------------------------------------------------------------
  // TODO: these will be very affected by t210104164036
  def customZ2Z(meta: Cls => Cls, data: Objs     => Objs    ): Self = self ::+ new CustomZZ(meta, data)
  def customS2S(meta: Cls => Cls, data: Seq[Obj] => Seq[Obj]): Self = self ::+ CustomZZ.from(meta, data)

  // ---------------------------------------------------------------------------
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

  @deprecated
  def asListBased: HeadZ = self ::+ AsListBased
  def asViewBased: HeadZ = self ::+ AsListBased

  // ===========================================================================
  // TODO: add more common ones
  // TODO: t210117110015 - move to common (need to abstract ForX...)

  def retainFirst               : Self2 = forKey(_.firstKey).thn(_ retain _)
  def renameSoleKey(value: KeyW): Self2 = forKey(_.soleKey) .thn(_.rename(_).to(value))
  
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
    @Distributivity def take(n: Int): HeadZ = zz(Take(n))

      def take(n: Option[Int]): HeadZ = n.map(take).getOrElse(self)
      //def drop(n: Int): HeadZ = ???
      //def takeLeft (n: Int): HeadZ = ??? // = take
      //def dropLeft (n: Int): HeadZ = ???
      //def takeRight(n: Int): HeadZ = ???
      //def dropRight(n: Int): HeadZ = ???
      //def reverse          : HeadZ = ???
      //def randomize        : HeadZ = ???
      //def addIndex  : Self = ??? // adds _index
      //def addRank   : Self = ??? // adds _rank (1-based)

@Distributivity def addIndex: HeadS = zz(AddIndex(key = _index, oneBased = false))
@Distributivity def addRank : HeadS = zz(AddIndex(key = _rank,  oneBased = true ))

  // ===========================================================================
  // TODO: t210205122151
//def toRddBased     : Self = ???
//def toInMemoryBased: Self = ??? // rename...
}

// ===========================================================================
object HeadZ extends HeadZIn {
  lazy val Dummy: HeadZ = """[{"foo":1, "bar": "baz1"}, {"foo":2, "bar": "baz2"}]""".stream()

  // ---------------------------------------------------------------------------
  val DefaultOutputFile = "/tmp/out.jsonl.gz"
}

// ===========================================================================
