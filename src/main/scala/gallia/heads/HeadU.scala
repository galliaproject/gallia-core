package gallia.heads

import gallia._
import gallia.actions.ActionsOthers._
import gallia.actions.ActionsZens._

// ===========================================================================
class HeadU private[gallia]( // TODO: t210121105809 - rename to HeadO and generally change occurrences of "u" into "o"; "u" was historical
      protected[gallia] override val nodeId : NodeId,
      protected[gallia] override val handler: Handler)
    extends common.HeadCommon[HeadU]

      with    HeadUOut

      with    HeadUSquashing
      with    HeadUFors
      with    HeadUAccessors {
  private[gallia] type Self = HeadU

  val self = this

  override def toString: String = nodeId // TODO

  @deprecated("bypasses the 210205063004 mechanism (but ok for trivial cases)") def forceAObj: AObj = end().runu().forceData2(_.aobj) // TODO: or forceData? forceResult?

  // ---------------------------------------------------------------------------
  protected[gallia] def rewrap(id: NodeId): HeadU = new HeadU(id, handler)

  private[gallia] def headZ        (newNodeId: NodeId): HeadZ    = new HeadZ   (newNodeId, handler)
  private[gallia] def headV[T: WTT](newNodeId: NodeId): HeadV[T] = new HeadV[T](newNodeId, handler)

  final          protected[heads] def ::+ (action: ActionUU): Self = handler.chainuu(this)(action)
  final override protected[heads] def  :+ (action: ActionUU): Self = handler.chainuu(this)(action)

    private[heads] def uu        (action: ActionUU): HeadU    = handler.chainuu(this)(action)
    private[heads] def uz        (action: ActionUZ): HeadZ    = handler.chainuz(this)(action)
    private[heads] def uv[T: WTT](action: ActionUV): HeadV[T] = handler.chainuv(this)(action)
    private[heads] def uo        (action: ActionUO): HeadU    = handler.chainuo(this)(action)

  // ===========================================================================
  // for consistency only, shouldn't really use outside of tests (use .thn instead)
  def zen        (f: Self => Self    ): Self     = uu(ZenUU(f))
  def zen        (f: Self => HeadZ   ): HeadZ    = uz(ZenUZ(f))
  def zen[V: WTT](f: Self => HeadV[V]): HeadV[V] = uv(ZenUV(f))

  // ---------------------------------------------------------------------------
  def      populateDataClass[DC: WTT]: Either[Any, DC] = ??? // TODO: t210117105638 - see 210117105638@v
  def forcePopulateDataClass[DC: WTT]: DC              = ???

  // ---------------------------------------------------------------------------
  // uz

  def flattenBy(target: RPathW): HeadZ = uz(FlattenByU(target.value))
  def convertToZ              : HeadZ = uz(ConvertUtoZ)

  // ===========================================================================
  def merge(that: HeadU): Self = handler.joinuu2u(this, that)

  // ===========================================================================
  def inspect             : Self = self ::+ InspectU(None     , abort = false)
  def inspect(msg: String): Self = self ::+ InspectU(Some(msg), abort = false)

  @aptus.fordevonly // pretty ugly
  def inspectAndAbort     : Self = self ::+ InspectU(None,      abort = true)

  // ===========================================================================
  // TODO: add more common ones
  // TODO: t210117110015 - move to common (need to abstract ForX...)
  def retainFirst               : Self2 = forKey(_.firstKey).zen(_ retain _)
  def renameSoleKey(value: KeyW): Self2 = forKey(_.soleKey) .zen(_.rename(_).to(value))
  
  def removeRecursivelyIfValue(value: String): Self2 = forLeafPaths { _.removeIfValueFor(_).is(value) } 

  def convertToIntRecursively                : Self2 = forLeafPaths(_.convert(_).toInt)
  def convertToDoubleRecursively             : Self2 = forLeafPaths(_.convert(_).toDouble)
  def convertToBooleanRecursively            : Self2 = forLeafPaths(_.convert(_).toBoolean)
}

// ===========================================================================
object HeadU extends HeadUIn {
  lazy val Dummy: HeadU = """{"_dummy": "dummy"}""".read()

  // ---------------------------------------------------------------------------
  val DefaultOutputFile = "/tmp/out.json.gz"
}

// ===========================================================================
