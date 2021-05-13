import java.lang.IllegalStateException

import gallia.io.in._
import gallia.domain._
import gallia.data.single.ObjIn

// ===========================================================================
package object gallia
    extends Reserved
    with    Aliases
    with    Annotations
    with    HeadAliases
    with    DataAliases
    with   ADataAliases {

  // ===========================================================================
  // TODO: t210121105809 - rename to HeadU->HeadO and HeadZ->HeadS (historical names)
  type HeadO = heads.HeadU
  type HeadS = heads.HeadZ

  // ===========================================================================
  private[gallia] type     Cls = meta.Cls
  private[gallia] lazy val Cls = meta.Cls

  private[gallia] type     Fld = meta.Fld
  private[gallia] lazy val Fld = meta.Fld

  // ---------------------------------------------------------------------------
  private[gallia] type Err = vldt.Err
  private[gallia] val  Err = vldt.Err

  private[gallia] type Errs  = Seq[Err]
  private[gallia] type Err_  = Option[Err]

  // ---------------------------------------------------------------------------
  private[gallia] type ActionVMN = ActionVN with ActionMN

  // ===========================================================================
  @deprecated
  private[gallia] def illegal        (anys: Any*) = throw new IllegalArgumentException(anys.mkString(","))
  private[gallia] def illegalArgument(anys: Any*) = throw new IllegalArgumentException(anys.mkString(","))
  private[gallia] def illegalState   (anys: Any*) = throw new IllegalStateException   (anys.mkString(","))
  
  @deprecated
  private[gallia] def runtimeError   (anys: Any*) = throw new RuntimeError(anys.mkString(","))  
  private[gallia] def dataError      (anys: Any*) = throw new RuntimeError(anys.mkString(","))

  private[gallia] def toBeImplemented(anys: Any*) = throw new ToBeImplemented(anys.mkString(","))

  // ---------------------------------------------------------------------------
  private[gallia] def errIf_(test: Boolean)(a: Any): Err_ = if (test) Some(Err(a)) else None
  private[gallia] def err_  (msg: String)          : Err_ = Some(Err(msg))
  private[gallia] def err   (any: Any)             : Err  = Err(any)
  private[gallia] def errs  (any: Any)             : Errs = Seq(Err(any))

  // ---------------------------------------------------------------------------
  private[gallia] def node[T: WTT] = gallia.reflect.TypeNode.parse[T]

  // ===========================================================================
  /** until/unless sure what we'll use - only to be used in non-object arrays/matrices/tensors */
  val none = null // TODO: t210115144940 - vs object vs None vs null  vs NaN ...?

  // ===========================================================================
  implicit class Input(val inputString: InputString) extends ReadObj with StreamObjs with ReadHead with StreamHead

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

  // ===========================================================================
  def cls[T: WTT]                 : Cls = reflect.TypeNode.parse[T].leaf.forceDataClass
  def cls(schemaFilePath: String) : Cls = meta.MetaObj.clsFromFile(schemaFilePath) // TODO: or also detect file vs direct object?
  def cls(field1: Fld, more: Fld*): Cls = meta.Cls(field1 +: more)

  // ---------------------------------------------------------------------------
  def obj(entry1: DataEntry, more: DataEntry*): Obj = ObjIn.from((entry1 +: more).map(_.pair))
  def obj(entries: Seq[(Key, AnyValue)])      : Obj = ObjIn.from(entries)

  // ---------------------------------------------------------------------------
  def objs(values: Obj*): Objs = Objs.from(values.toList)

  // ===========================================================================
  def bobj(entry1: KVE, more: KVE*): BObj = BObj(KVEs(entry1 +: more))

    def aobj(c: Cls)                 (u: Obj): AObj = AObj(c, u)
    def aobj(field1: Fld, more: Fld*)(u: Obj): AObj = AObj(cls(field1, more:_*), u)

  // ---------------------------------------------------------------------------
  def bobjs(value1: BObj, more: BObj*): BObjs = BObjs(value1 +: more)

    def aobjs(values: AObj*)                        : AObjs = AObjs.from(values)
    def aobjs(field1: Fld, more: Fld*)(values: Obj*): AObjs = aobjs(cls(field1, more:_*))(values:_*)
    def aobjs(c: Cls)                 (values: Obj*): AObjs = AObjs(c, Objs.from(values.toList))
    def aobjs(c: Cls, z: Objs)                      : AObjs = AObjs(c, z)

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
  case class RuntimeError(details: Any) extends RuntimeException(details.toString)

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
      def |> (that: Ren  ): RPath = KPath.from(_key).appendLevel(that.from).qpath(that.to)

      // TODO:?
      //def |> (that: KeyWz   ): SPathz = ???
      //def |> (x1: KeyW, x2: KeyW, more: KeyW*): SPathz = ???

      def ~> (to:   KeyW): Ren   = Ren.from(_key, to)

      def <~> (key: KeyW): gallia.heads.merging.MergingData.JoinKey = gallia.heads.merging.MergingData.JoinKey(_key, key.value)
    }
}

// ===========================================================================
