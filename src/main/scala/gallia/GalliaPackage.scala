import gallia.io.in._
import gallia.domain._

// ===========================================================================
package object gallia
    extends Reserved
    with    Aliases
    with    Annotations
    with    CustomTraits
    with    GenericEntryImplicits
    with    reflect.ReflectExtensions
    with    reflect.lowlevel.ReflectionTypesAbstraction /* notably WTT, differs for scala 2 vs scala 3 */
    with    reflect.lowlevel.WttImplicits /* only useful to scala 2 */ {

  // ---------------------------------------------------------------------------
  private[gallia] implicit class GalliaAnything_[A](value: A) { // so as to not import chaining._ everywhere
    private[gallia] def pipe[B](f: A => B)   : B =   f(value)
    private[gallia] def tap    (f: A => Unit): A = { f(value); value }

    // ---------------------------------------------------------------------------
    // to save some common imports
    private[gallia] def _assert(p: A => Boolean):              A = new aptus.Anything_(value).assert(p)
    private[gallia] def _assert(p: A => Boolean, f: A => Any): A = new aptus.Anything_(value).assert(p, f)

    private[gallia] def p___           : A = new aptus.Anything_(value).p__
    private[gallia] def p_             : A = new aptus.Anything_(value).p
    private[gallia] def i_(f: A => Any): A = new aptus.Anything_(value).i(f) }

  // ---------------------------------------------------------------------------
  private[gallia] implicit class GalliaSeq[T](val seq: Seq[T]) extends GalliaUtils.GalliaSeq[T] // TODO: to aptus if generalizes well?

  // ---------------------------------------------------------------------------
  private[gallia] implicit class Map__[K, V](map: Map[K, V]) { def mapValues0[V2](f: V => V2): Map[K, V2] = map.map { case (k, v) => k -> f(v) } } // because of scala 2.12

  // ===========================================================================
  // TODO: t210121105809 - rename to HeadU->HeadO and HeadZ->HeadS (historical names)

  type HeadV[T] = heads.HeadV[T]

  type HeadO = heads.HeadU // "O" for "one"
  val  HeadO = heads.HeadU // "O" for "one"

  type HeadM = heads.HeadZ // "M" for "multiple"
  val  HeadM = heads.HeadZ // "M" for "multiple"

    type HeadU = gallia.heads.HeadU // will deprecate (legacy name)
    val  HeadU = gallia.heads.HeadU // will deprecate (legacy name)

    type HeadZ = gallia.heads.HeadZ // will deprecate (legacy name)
    val  HeadZ = gallia.heads.HeadZ // will deprecate (legacy name)

    type HeadS = heads.HeadZ // will deprecate (legacy name)
    val  HeadS = heads.HeadZ // will deprecate (legacy name)

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
  type     TypeNode = reflect.TypeNode
  lazy val TypeNode = reflect.TypeNode

  // ---------------------------------------------------------------------------
  type EnumValue = reflect.EnumValue
  val  EnumValue = reflect.EnumValue

  // ---------------------------------------------------------------------------
  type BigDec = scala.BigDecimal
  val  BigDec = scala.BigDecimal

  // ---------------------------------------------------------------------------
  private[gallia] type KeyPair = GenericEntry[Key]
  private         type VEntry  = GenericEntry[HeadV[_]]

  // ===========================================================================
  def headV[T: WTT](value1: T, value2: T, more: T*): HeadV[Seq[T]] = heads.Head.inputV(Seq(value1, value2) ++ more)
  def headV[T: WTT](values: Seq[T])                : HeadV[Seq[T]] = heads.Head.inputV(values)
  def headV[T: WTT](value :     T )                : HeadV[    T ] = heads.Head.inputV(value)

  // ---------------------------------------------------------------------------
                  def headO(first: VEntry, second: VEntry, more: VEntry*): HeadU = headO(Seq(first, second) ++ more)
  private[gallia] def headO(pairs: Seq[VEntry])                          : HeadU = pairs.map { x => x.value._2.dressUp(x.value._1) } .reduceLeft(_ merge _)

  // ===========================================================================
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

  private[gallia] type _Enm    =        gallia.meta.basic.BasicType._Enm
  private[gallia] type _EnmOpt = Option[gallia.meta.basic.BasicType._Enm]

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

  // ---------------------------------------------------------------------------
  private[gallia] type DataRegenerationClosure[T] = gallia.data.DataRegenerationClosure[T]

  // ---------------------------------------------------------------------------
  private[gallia] type DataClass = Product with Equals // TODO: no way to make it more specific for case class?

  // ---------------------------------------------------------------------------
  private[gallia] type ClassTag[T] = scala.reflect.ClassTag[T] // mostly because of spark

  // ===========================================================================
  private[gallia] def dataError(anys: Any*): Nothing = throw new RuntimeError(anys.mkString(","))

  // ---------------------------------------------------------------------------
  private[gallia] def errIf_(test: Boolean)(a: Any): Err_ = if (test) Some(Err(a)) else None
  private[gallia] def err_  (msg: String)          : Err_ = Some(Err(msg))
  private[gallia] def err   (any: Any)             : Err  = Err(any)
  private[gallia] def errs  (any: Any)             : Errs = Seq(Err(any))

  // ===========================================================================
                  def _typeNode[T: WTT]: gallia.reflect.TypeNode    = implicitly[WTT[T]].typeNode // for tests
  private[gallia] def  typeNode[T: WTT]: gallia.reflect.TypeNode    = implicitly[WTT[T]].typeNode
  private[gallia] def  ctag    [T: WTT]: scala .reflect.ClassTag[T] = implicitly[WTT[T]].ctag

  // ===========================================================================
  /** until/unless sure what we'll use - only to be used in non-object arrays/matrices/tensors */
  val none = null // TODO: t210115144940 - vs object vs None vs null  vs NaN ...?

  // ===========================================================================
  implicit class InputString__     (val inputString: InputString)         extends ReadObjFromString with StreamObjsFromString with ReadHeadFromString with StreamHeadFromString
  implicit class InputIterable__[T](val data       : Iterable[T])         extends StreamObjsFromIterable[T] /* TODO: t230620092637 - rename as _data (else causes conflicts) */
  implicit class InputConnection__ (val connection : java.sql.Connection) extends StreamConnection
  implicit class InputIterator__[T](val values     : DataRegenerationClosure[Obj]) { def stream(schema: Cls): HeadS = actions.in.GenericInputZ(schema, values).pipe(heads.Head.inputZ) }

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
  private[gallia] implicit def SubInfo_(subInfo: meta.SubInfo): Seq[meta.SubInfo] = Seq(subInfo) // see t210125111338 (union types)

  // ===========================================================================
  def cls[T: WTT]                         : Cls = typeNode[T].leaf.forceDataClass
  def cls(schemaFilePath: String)         : Cls = Cls.fromFile(schemaFilePath) // TODO: or also detect file vs direct object?
  def cls(field1: Fld, more: Fld*)        : Cls = cls((field1 +: more).toList)
  def cls               (fields: Seq[Fld]): Cls = meta.Cls(fields.toList)
  def cls(name: ClsName, fields: Seq[Fld]): Cls = meta.Cls(fields.toList).setName(name)

  // ---------------------------------------------------------------------------
  def obj(entry1: DataEntry, more: DataEntry*)                    : Obj = Obj.fromIterable((entry1 +: more).toList.map(_.pair))
  def obj(entries: Seq[( Key, AnyValue)])                         : Obj = Obj.fromIterable(entries)
  def obj(entries: Seq[(SKey, AnyValue)])(implicit d : DI)        : Obj = Obj.fromIterable(entries.map { case (k, v) => Symbol(k) -> v })
  def obj(entries: Seq[(UKey, AnyValue)])(implicit d1: DI, d2: DI): Obj = Obj.fromIterable(entries.map { case (k, v) => Symbol(k.entryName) -> v })

  def objFromDataClass[T  <: Product : WTT](value: T): Obj = data.single.ObjIn.fromDataClassInstance(value)

  // ---------------------------------------------------------------------------
  def objs(values: Obj*)                     : Objs = Objs.from(values.toList)
  def objs(values: Seq[Obj])(implicit di: DI): Objs = Objs.from(values.toList)

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

    def aobjsFromDataClasses[T <: Product : WTT](value1: T, more: T*): AObjs = aobjsFromDataClasses(value1 +: more)
    def aobjsFromDataClasses[T <: Product : WTT](values: Seq[T])     : AObjs = AObjs(cls[T], values.toList.map(objFromDataClass[T]).pipe(Objs.from))

  // ===========================================================================
  def indexToKey(i: Int): Key = rankToKey(i + 1)
  def  rankToKey(i: Int): Key = Symbol(s"_${i}")
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
      def |> (that: Ren) : RPath = KPath.from(_key).appendLevel(that.from).rpath(that.to)

      // TODO:?
      //def |> (that: KeyWz   ): SPathz = ???
      //def |> (x1: KeyW, x2: KeyW, more: KeyW*): SPathz = ???

      def ~> (to:   KeyW): Ren   = Ren.from(_key, to)

      def <~> (key: KeyW): gallia.heads.merging.MergingData.JoinKey = gallia.heads.merging.MergingData.JoinKey(_key, key.value)
    }
}

// ===========================================================================
