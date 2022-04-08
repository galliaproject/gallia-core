import gallia.io.in._
import gallia.domain._

// ===========================================================================
package object gallia
    extends Reserved
    with    Aliases
    with    Annotations {

  private[gallia] implicit class GalliaAnything_[A](u: A) { // so as to not import chaining._ everywhere
    def pipe[B](f: A => B)   : B =   f(u)
    def tap [B](f: A => Unit): A = { f(u); u }    
  }

  // ===========================================================================
  // TODO: t210121105809 - rename to HeadU->HeadO and HeadZ->HeadS (historical names)
  type HeadO = heads.HeadU
  val  HeadO = heads.HeadU
  
  type HeadS = heads.HeadZ
  val  HeadS = heads.HeadZ

  type HeadU = gallia.heads.HeadU
  val  HeadU = gallia.heads.HeadU

  type HeadZ = gallia.heads.HeadZ
  val  HeadZ = gallia.heads.HeadZ

  type HeadV[T] = heads.HeadV[T]

  // ---------------------------------------------------------------------------
  type AObj  = gallia.domain.AObj
  val  AObj  = gallia.domain.AObj
  type BObj  = gallia.domain.BObj
  val  BObj  = gallia.domain.BObj

  type AObjs = gallia.domain.AObjs
  val  AObjs = gallia.domain.AObjs
  type BObjs = gallia.domain.BObjs
  val  BObjs = gallia.domain.BObjs

  // ---------------------------------------------------------------------------
  type Obj  = gallia.data.single  .Obj
  val  Obj  = gallia.data.single  .Obj

  type Objs = gallia.data.multiple.Objs
  val  Objs = gallia.data.multiple.Objs
  
  // ---------------------------------------------------------------------------
  type     Cls = meta.Cls
  lazy val Cls = meta.Cls

  type     Fld = meta.Fld
  lazy val Fld = meta.Fld

  // ---------------------------------------------------------------------------
  type BigDec = scala.BigDecimal
  val  BigDec = scala.BigDecimal

  // ---------------------------------------------------------------------------
  def byteBuffer(byte1: Byte, more: Byte*): ByteBuffer = java.nio.ByteBuffer.wrap((byte1 +: more).toArray)
  def byteBuffer(value: String)           : ByteBuffer = java.nio.ByteBuffer.wrap(value.getBytes)
  def byteBuffer(bytes: Array[Byte])      : ByteBuffer = java.nio.ByteBuffer.wrap(bytes)
  
  // ---------------------------------------------------------------------------
  implicit class ByteBuffer__(bb: ByteBuffer) {    
    def mapBytes(f: Array[Byte] => Array[Byte]): ByteBuffer = bb.array.pipe(f).pipe(byteBuffer)  }

  // ===========================================================================
  private[gallia] type AnyValue = Any

  // ---------------------------------------------------------------------------
  private[gallia] type Temporal = java.time.temporal.Temporal
    
    private[gallia] type LocalTime      = java.time. LocalTime
    private[gallia] type LocalDate      = java.time. LocalDate
    
    private[gallia] type LocalDateTime  = java.time. LocalDateTime
    private[gallia] type OffsetDateTime = java.time.OffsetDateTime
    private[gallia] type ZonedDateTime  = java.time. ZonedDateTime
    
    private[gallia] type Instant        = java.time.Instant

  // ---------------------------------------------------------------------------
  private[gallia] type ByteBuffer    = java.nio.ByteBuffer // note: use ByteBuffer.wrap(_: Array[Byte])

  // ---------------------------------------------------------------------------  
  private[gallia] type EnumEntry            = enumeratum.EnumEntry
  private[gallia] type Enum[T <: EnumEntry] = enumeratum.Enum[T]
  
  // ---------------------------------------------------------------------------
  private[gallia] type Whatever = whatever.Whatever
  private[gallia] val  Whatever = whatever.Whatever

  // ---------------------------------------------------------------------------
  private[gallia] type Err = vldt.Err
  private[gallia] val  Err = vldt.Err

  private[gallia] type Errs  = Seq[Err]
  private[gallia] type Err_  = Option[Err]

  // ---------------------------------------------------------------------------
  private[gallia] type ActionVMN = ActionVN with ActionMN

  // ===========================================================================
  private[gallia] def dataError(anys: Any*): Nothing = throw new RuntimeError(anys.mkString(","))

  // ---------------------------------------------------------------------------
  private[gallia] def errIf_(test: Boolean)(a: Any): Err_ = if (test) Some(Err(a)) else None
  private[gallia] def err_  (msg: String)          : Err_ = Some(Err(msg))
  private[gallia] def err   (any: Any)             : Err  = Err(any)
  private[gallia] def errs  (any: Any)             : Errs = Seq(Err(any))

  // ---------------------------------------------------------------------------
  private[gallia] def typeNode[T: WTT] = gallia.reflect.TypeNode.parse[T]

  // ===========================================================================
  /** until/unless sure what we'll use - only to be used in non-object arrays/matrices/tensors */
  val none = null // TODO: t210115144940 - vs object vs None vs null  vs NaN ...?

  // ===========================================================================
  implicit class InputString__     (val inputString: InputString)         extends ReadObjFromString with StreamObjsFromString with ReadHeadFromString with StreamHeadFromString
  implicit class InputIterable__[T](val values     : Iterable[T])         extends StreamObjsFromIterable[T]  
  implicit class InputConnection__ (val connection : java.sql.Connection) extends StreamConnection

  // ---------------------------------------------------------------------------
  implicit class  Key_(val u:  Key) extends Key__ { val _key = u }
  implicit class SKey_(val u: SKey) extends Key__ { val _key = Symbol(u          ) }
  implicit class UKey_(val u: UKey) extends Key__ { val _key = Symbol(u.entryName) }
  implicit class EKey_(val u: EKey) extends Key__ { val _key = Symbol(u.toString ) }

  implicit class KPath_(val u: KPath) extends gallia.domain.Sorter.Sorter__ { val _path = u}

  // ---------------------------------------------------------------------------
  implicit def _toPairKk(x: ( Key,  Key)): (KeyW, KeyW) = (x._1, x._2)
  implicit def _toPairKS(x: ( Key, SKey)): (KeyW, KeyW) = (x._1, x._2)
  implicit def _toPairSK(x: (SKey,  Key)): (KeyW, KeyW) = (x._1, x._2)
  implicit def _toPairSS(x: (SKey, SKey)): (KeyW, KeyW) = (x._1, x._2)

  // ---------------------------------------------------------------------------
  import heads.merging.HeadZPair
  implicit def _toPair1(pair: (HeadZ, HeadZ)): HeadZPair = new HeadZPair(pair._1, pair._2) // TODO: st201123125604 - why not picked up?
  implicit def _toPair2(pair: (BObjs, BObjs)): HeadZPair = new HeadZPair(pair._1, pair._2)

  implicit def _toWrappedSelection(x: selection.untyped.processors.RPathzSelection): selection.typed.TsWrapper[Whatever] = new selection.typed.TsWrapper[Whatever](x)

  // ---------------------------------------------------------------------------
  private[gallia] implicit def Info_(info: meta.Info): Seq[meta.Info] = Seq(info) // see t210125111338 (union types)
  
  // ===========================================================================
  def cls[T: WTT]                 : Cls = reflect.TypeNode.parse[T].leaf.forceDataClass
  def cls(schemaFilePath: String) : Cls = Cls.fromFile(schemaFilePath) // TODO: or also detect file vs direct object?
  def cls(field1: Fld, more: Fld*): Cls = cls((field1 +: more).toList)
  def cls(fields: Seq[Fld])       : Cls = meta.Cls(fields.toList)

  // ---------------------------------------------------------------------------
  def obj(entry1: DataEntry, more: DataEntry*)            : Obj = Obj.fromIterable((entry1 +: more).toList.map(_.pair))
  def obj(entries: Seq[( Key, AnyValue)])                 : Obj = Obj.fromIterable(entries)
  def obj(entries: Seq[(SKey, AnyValue)])(implicit di: DI): Obj = Obj.fromIterable(entries.map { case (k, v) => Symbol(k) -> v })

  def objFromDataClass[T  <: Product : WTT](value: T): Obj = data.single.ObjIn.fromDataClassInstance(value)
  
  // ---------------------------------------------------------------------------
  def objs(values: Obj*): Objs = Objs.from(values.toList)

  // ===========================================================================
  def bobj(entry1: KVE, more: KVE*): BObj = BObj(KVEs((entry1 +: more).toList))

    def aobj(c: Cls)                 (u: Obj): AObj = AObj(c, u)
    def aobj(field1: Fld, more: Fld*)(u: Obj): AObj = AObj(cls(field1, more:_*), u) // can't have both meta and data be varargs...
    
    def aobjFromDataClass[T <: Product : WTT](value: T): AObj = AObj(cls[T], objFromDataClass(value))

  // ---------------------------------------------------------------------------
  def bobjs(value1: BObj, more: BObj*): BObjs = (value1 +: more).toList.pipe(BObjs.apply)

    def aobjs(values: AObj*)                        : AObjs = values.toList.pipe(AObjs.from)
    def aobjs(field1: Fld, more: Fld*)(values: Obj*): AObjs = aobjs(cls(field1, more:_*))(values:_*)
    def aobjs(c: Cls)                 (values: Obj*): AObjs = AObjs(c, values.toList.pipe(Objs.from))
    def aobjs(c: Cls, z: Objs)                      : AObjs = AObjs(c, z)

    def aobjsFromDataClasses[T <: Product : WTT](values: Seq[T]): AObjs = AObjs(cls[T], values.toList.map(objFromDataClass[T]).pipe(Objs.from))

  // ===========================================================================
  def indexToKey(i: Int): Key = rankToKey(i + 1)
  def  rankToKey(i: Int): Key = Symbol(s"_${i}")

  // ===========================================================================
  private[gallia] val closeables = cross.MutList[java.io.Closeable]()

  // ---------------------------------------------------------------------------
  sys.addShutdownHook {
    closeables.foreach { _.close() /* idempotent */ } }
}

// ===========================================================================
package gallia {
  trait HasToObj { def toObj: Obj }

  // ---------------------------------------------------------------------------
  case class MetaError(errors: gallia.run.ResultSchema.Errors) extends RuntimeException(errors.formatExceptionMessage)

  /** those can't be called until we have the actual data's (eg ensure uniqueness, pivot for new keys, ...) */
  case class RuntimeError   (details: Any) extends RuntimeException(details.toString)
  
  case class ToBeImplemented(details: Any) extends RuntimeException(details.toString)

  // ===========================================================================
  trait Key__
        extends domain.Sorter.Sorter__
        with    meta.FldCreator
        with    heads.reducing.ReducingPair1.Implicit_
        with    heads.reducing.ReducingPairN.Implicit_ {
      protected override def _path: KPathW = _key
      protected          val _key : Key

      // ---------------------------------------------------------------------------
      def |> (that: KeyW): KPath = KPath.from(_key).appendLevel(that.value)
      def |> (that: Ren) : RPath = KPath.from(_key).appendLevel(that.from).qpath(that.to)

      // TODO:?
      //def |> (that: KeyWz   ): SPathz = ???
      //def |> (x1: KeyW, x2: KeyW, more: KeyW*): SPathz = ???

      def ~> (to:   KeyW): Ren   = Ren.from(_key, to)

      def <~> (key: KeyW): gallia.heads.merging.MergingData.JoinKey = gallia.heads.merging.MergingData.JoinKey(_key, key.value)
    }
}

// ===========================================================================
