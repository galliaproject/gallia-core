package gallia.meta

import aptus.{Anything_, String_, Seq_}

import gallia._
import gallia.target._

// ===========================================================================
case class Cls(fields: Seq[Fld])
      extends Containee
      with    ClsLike
      with    ClsHelper
      with    ClsNesting
      with    ClsBasics
      with    ClsAvanced
      with    ClsNestingRelated
      with    ClsAggregating
      with    ClsMerging {
  
    fields
      .require(_.nonEmpty)
      .requireDistinctBy(_.key)

    // ---------------------------------------------------------------------------
    // mostly for macros
    private var nameOpt: Option[String] = None
    def setName(name: String) = { nameOpt = Some(name); this }
    def forceName: String = nameOpt.get

    // ---------------------------------------------------------------------------
    protected override val _fields: Seq[Fld]  = fields // see ClsLike

    // ---------------------------------------------------------------------------
    override val lookup: Map[Key, Fld] = fields.force.mapLeft(_.key)

    override val keySet    : Set   [Key] = keys.toSet
    override val keyVector : Vector[Key] = keys.toVector

    override val requiredKeys  : Seq[Key] = _fields.filter(_.isRequired).map(_.key)
    override val requiredKeySet: Set[Key] = _fields.filter(_.isRequired).map(_.key).toSet

    // ---------------------------------------------------------------------------
    override def toString = formatDefault

      def formatDefault =
        fields
          .map(_.formatDefault)
          .section(nameOpt.map(_.quote.colon).getOrElse("cls:"))

    // ---------------------------------------------------------------------------
    def hasNesting                   : Boolean = fields.exists(_.isNesting)
    def areAllNonRequired(keyz: Keyz): Boolean = keyz.map(field(_)).forall(_.isNotRequired)

    def merge(that: Cls): Cls = Cls(this.fields ++ that.fields)

    // ===========================================================================
    def _filterByKey (pred:  Key => Boolean): Seq[Fld] = fields.filterBy(pred)(_. key)
    def _filterBySKey(pred: SKey => Boolean): Seq[Fld] = fields.filterBy(pred)(_.skey)

    // ===========================================================================
    def field (path: RPathW): Fld         = field (path.value)
    def field_(path: RPathW): Option[Fld] = field_(path.value)

    def soleField = fields.force.one

    // ---------------------------------------------------------------------------
    def forceNestedClass(key: Key  ): Cls = field(key).info.forceNestedClass
    def forceNestedClass(key: KPath): Cls = field(key).info.forceNestedClass

    def forceBasicType(key: Key  ): BasicType = field(key).info.forceBasicType
    def forceBasicType(key: KPath): BasicType = field(key).info.forceBasicType

    // ---------------------------------------------------------------------------
    @deprecated def numericalType(ren: Ren): NumericalType = field(ren.from).forceNumericalType

    // ---------------------------------------------------------------------------
    def filterFields(p: Fld => Boolean): Cls = Cls(fields.filter(p))

    def     mapFields(f: Fld =>     Fld ): Cls = Cls(fields.    map(f))
    def flatMapFields(f: Fld => Seq[Fld]): Cls = Cls(fields.flatMap(f))

    // ===========================================================================
    // both for val and meta

    private[meta /*cls*/] def rewrap(fields: Seq[Fld]) = Cls(fields)

    // ===========================================================================
    private[meta /*cls*/] def requireNewKey   (target : Key ) { requireNewKeys(Keyz.from(target)) }
    private[meta /*cls*/] def requireNewKeys  (targets: Keyz) { targets.values.intersect(this.keys).as.someIf(!_.isEmpty).foreach(x => illegal(s"201124171037:${x.#@@}(${keys.#@@})") /*TODO*/) }

    private[meta /*cls*/] def requireKnownKey (target : Key ) { requireKnownKeys(Keyz.from(target)) }
    private[meta /*cls*/] def requireKnownKeys(targets: Keyz) { targets.values.diff(this.keys).as.someIf(!_.isEmpty).foreach(x => illegal(s"201124171038:${x.#@@}(${keys.#@@})") /*TODO*/) }

    // ---------------------------------------------------------------------------
    private[meta /*cls*/] def requireRenamingKey (target : RenW ) { requireRenamingKeys(Renz.from(target)) }
    private[meta /*cls*/] def requireRenamingKeys(targets: RenWz) { requireKnownKeys(targets.fromz); requireNewKeys  (targets.toz) }

    // ===========================================================================
    def toObj2 = MetaObj.cls(this)

    def formatJson        = toObj2.formatCompactJson
    def formatCompactJson = toObj2.formatCompactJson
    def formatPrettyJson  = toObj2.formatPrettyJson

    def formatShort0: String = MetaObj.formatClassDebug(this)
    def formatShort : String = MetaObj.formatClassDebug(this).sectionAllOff("<root>")
    def formatFull  : String = ???//MetaObj.cls (this)

    def printShort() { formatShort.p; () }
    def printFull () { formatFull .p; () }

    def writeShort(out: String) { formatShort.writeFileContent(out); () }
    def writeFull (out: String) { formatFull .writeFileContent(out); () }

    // ===========================================================================
    def valueFromObj  (instantiator: Instantiator)(value: Any): Any = value.asInstanceOf[              Obj  ]      .thn(instantiator.instantiateRecursively(this, _))
    def valueFromObjs (instantiator: Instantiator)(value: Any): Any = value.asInstanceOf[       Seq   [Obj] ]      .map(instantiator.instantiateRecursively(this, _))
    def valueFromObj_ (instantiator: Instantiator)(value: Any): Any = value.asInstanceOf[       Option[Obj] ]      .map(instantiator.instantiateRecursively(this, _))
    def valueFromObjs_(instantiator: Instantiator)(value: Any): Any = value.asInstanceOf[Option[Seq   [Obj]]].map(_.map(instantiator.instantiateRecursively(this, _)))

    // ---------------------------------------------------------------------------
    def valueToObj  (value: Any): Any = value.asInstanceOf[              Product  ]            .productIterator.thn(ClsUtils.valuesToObj(this))
    def valueToObjs (value: Any): Any = value.asInstanceOf[       Seq   [Product] ]      .map(_.productIterator.thn(ClsUtils.valuesToObj(this)))
    def valueToObj_ (value: Any): Any = value.asInstanceOf[       Option[Product] ]      .map(_.productIterator.thn(ClsUtils.valuesToObj(this)))
    def valueToObjs_(value: Any): Any = value.asInstanceOf[Option[Seq   [Product]]].map(_.map(_.productIterator.thn(ClsUtils.valuesToObj(this))))

    // ===========================================================================
    //TODO: ensure not nested type?
    def   updateType(target: Key   , node: TypeNode): Cls =                transformField(target)(_.updateInfo(node.forceNonBObjInfo))
      def updateType(target: Ren   , node: TypeNode): Cls = rename(target).updateType(target.to, node)

      def updateType(target: KPath , node: TypeNode): Cls = transformx(target)(_.updateType(_, node), _.updateType(_, node))
      def updateType(target: RPath , node: TypeNode): Cls = transformx(target)(_.updateType(_, node), _.updateType(_, node))

      def updateType(target: KPathz, node: TypeNode): Cls = target.foldLeft(this)(_.updateType(_, node))
      def updateType(target: RPathz, node: TypeNode): Cls = target.foldLeft(this)(_.updateType(_, node))

    // ---------------------------------------------------------------------------
    def   updateContainee(target: Key   , node: TypeNode): Cls =                transformField(target)(_.updateContainee(node.forceNonBObjInfo.containee))
      def updateContainee(target: Ren   , node: TypeNode): Cls = rename(target).updateContainee(target.to, node)

      def updateContainee(target: KPath , node: TypeNode): Cls = transformx(target)(_.updateContainee(_, node), _.updateContainee(_, node))
      def updateContainee(target: RPath , node: TypeNode): Cls = transformx(target)(_.updateContainee(_, node), _.updateContainee(_, node))

      def updateContainee(target: KPathz, node: TypeNode): Cls = target.foldLeft(this)(_.updateContainee(_, node))
      def updateContainee(target: RPathz, node: TypeNode): Cls = target.foldLeft(this)(_.updateContainee(_, node))

    // ---------------------------------------------------------------------------
    def   updateContainer(target: Key   , node: TypeNode): Cls =                transformField(target)(_.updateContainer(node.forceNonBObjInfo.container))
      def updateContainer(target: Ren   , node: TypeNode): Cls = rename(target).updateContainer(target.to, node)

      def updateContainer(target: KPath , node: TypeNode): Cls = transformx(target)(_.updateContainer(_, node), _.updateContainer(_, node))
      def updateContainer(target: RPath , node: TypeNode): Cls = transformx(target)(_.updateContainer(_, node), _.updateContainer(_, node))

      def updateContainer(target: KPathz, node: TypeNode): Cls = target.foldLeft(this)(_.updateContainer(_, node))
      def updateContainer(target: RPathz, node: TypeNode): Cls = target.foldLeft(this)(_.updateContainer(_, node))

    // ===========================================================================
    def transformField(target: Key   )(f: Fld => Fld): Cls =               _transformField(target)(f)
    def transformField(target: Ren   )(f: Fld => Fld): Cls = rename(target).transformField(target.to)(f)

    def transformField(target: KPath )(f: Fld => Fld): Cls = transformx(target)(_.transformField(_)(f), _.transformField(_)(f))
    def transformField(target: RPath )(f: Fld => Fld): Cls = transformx(target)(_.transformField(_)(f), _.transformField(_)(f))

    def transformField(target: KPathz)(f: Fld => Fld): Cls = target.foldLeft(this)(_.transformField(_)(f))
    def transformField(target: RPathz)(f: Fld => Fld): Cls = target.foldLeft(this)(_.transformField(_)(f))

      // ---------------------------------------------------------------------------
      // commonly needed

      def transformInfo       (target: RPathWz)(f: Info => Info): Cls = target.values.foldLeft(this)(_.transformInfo(_)(f))
      def transformInfo       (target: RPathW )(f: Info => Info): Cls = transformField(target.value)(_.transformInfo(f))
      def transformNestedClass(target: RPathW )(f: Cls  => Cls) : Cls = transformInfo(target.value)(_.transformNestedClass(f))

    // ===========================================================================
    def    toRequired(key: RPathW): Cls = transformInfo(key.value)(_.   toRequired)
    def toNonRequired(key: RPathW): Cls = transformInfo(key.value)(_.toNonRequired)

    def    toMultiple(key: RPathW): Cls = transformInfo(key.value)(_.   toMultiple)
    def toNonMultiple(key: RPathW): Cls = transformInfo(key.value)(_.toNonMultiple)

    // ---------------------------------------------------------------------------
    def    toRequired(path: RPath): Cls = transformx(path)(_    toRequired _, _    toRequired _)
    def toNonRequired(path: RPath): Cls = transformx(path)(_ toNonRequired _, _ toNonRequired _)

    def    toMultiple(path: RPath): Cls = transformx(path)(_    toMultiple _, _    toMultiple _)
    def toNonMultiple(path: RPath): Cls = transformx(path)(_ toNonMultiple _, _ toNonMultiple _)

    // ===========================================================================
    def toOneStr     (target: RPathW) = updateType(target.value, BasicType._String .node)
    def toOneInt     (target: RPathW) = updateType(target.value, BasicType._Int    .node)
    def toOneDouble  (target: RPathW) = updateType(target.value, BasicType._Double .node)
    def toOneBoolean (target: RPathW) = updateType(target.value, BasicType._Boolean.node)

      // ---------------------------------------------------------------------------
      def toStr     (target: RPathW) = updateContainee(target.value, BasicType._String .node)
      def toInt     (target: RPathW) = updateContainee(target.value, BasicType._Int    .node)
      def toDouble  (target: RPathW) = updateContainee(target.value, BasicType._Double .node)
      def toBoolean (target: RPathW) = updateContainee(target.value, BasicType._Boolean.node)

        // ---------------------------------------------------------------------------
        def toOptionalBoolean(target: RPathW) = toOneBoolean(target).toNonRequired(target)
  }

// ===========================================================================
object Cls {
  val Dummy = Cls(Seq(Fld.Dummy))

  val Content = cls(_content.string)
  val Line    = cls(_line   .string) // TODO:opt?

  def array(nesting: Cls) = cls(_array.clss(nesting))

  // ---------------------------------------------------------------------------
  val FullDescriptiveStats = ClsConstants.FullDescriptiveStats
  val FullPercentiles      = ClsConstants.FullPercentiles

  // ===========================================================================
  private[gallia] def vleInt: Cls = Cls(Seq(Fld.oneInt(_vle)))

  private[gallia] def vle (node: TypeNode): Cls = Cls(Seq(Fld(_vle, node.forceNonBObjInfo)))
  private[gallia] def vles(node: TypeNode): Cls = Cls(Seq(Fld(_vle, node.forceNonBObjInfo).toMultiple))

  // ---------------------------------------------------------------------------
  def from(keys: Seq[ Key]): Cls = from(keys.map(_.name))
  def from(keys: Seq[SKey])(implicit di: DI): Cls =
    keys
      .map(skey => Fld(skey.symbol, Info.string))
      .thn(Cls.apply)

}

// ===========================================================================
