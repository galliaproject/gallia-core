package gallia
package meta

import aptus.{Anything_, String_, Seq_}

import target._

// ===========================================================================
case class Cls(fields: Seq[Fld]) // TODO: as List?
      extends meta.ValueType
      with    ClsLike

      with    ClsHelper
      with    ClsNesting
      with    ClsBasics
      with    ClsAdvanced
      with    ClsNestingRelated
      with    ClsAggregating
      with    ClsMerging {

    fields
      .require(_.nonEmpty)
      .requireDistinctBy(_.key)

    // ---------------------------------------------------------------------------
    // mostly for macros; also useful to union types (see t210125111338) -> may promote - consider impact on equality
    private var _nameOpt: ClsNameOpt = None
      def   setName(name: ClsName) = { _nameOpt = Some(name); this }
      def forceName: ClsName    = _nameOpt.get
      def nameOpt  : ClsNameOpt = _nameOpt

    // ---------------------------------------------------------------------------
    override val lookup: Map[Key, Fld] = fields.map(_.associateLeft(_.key)).force.map

    override val  keySet    : Set   [ Key] =  keys.toSet
    override val skeySet    : Set   [SKey] = skeys.toSet
    override val  keyVector : Vector[ Key] =  keys.toVector

    override val requiredKeys  : Seq[Key] = fields.filter(_.isRequired).map(_.key)
    override val requiredKeySet: Set[Key] = fields.filter(_.isRequired).map(_.key).toSet

    // ---------------------------------------------------------------------------
    override def toString = formatDefault

      def formatDefault: String =
        fields
          .map(_.formatDefault)
          .section(nameOpt.map(_.quote.colon).getOrElse(""))

    // ---------------------------------------------------------------------------
    def unknownKeys(o: Obj): Set[Key] = {
      val set1 = o   .keySet
      val set2 = this.keySet

      set1.diff(set2)
    }

    // ---------------------------------------------------------------------------
    def merge(that: Cls): Cls = Cls(this.fields ++ that.fields)

    // ---------------------------------------------------------------------------
    override def valuePredicate: AnyValue => Boolean = _.isInstanceOf[Obj]

    // ===========================================================================
    def _filterByKey (pred:  Key => Boolean): Seq[Fld] = fields.filterBy(pred)(_. key)
    def _filterBySKey(pred: SKey => Boolean): Seq[Fld] = fields.filterBy(pred)(_.skey)

    // ===========================================================================
    def filterFields(p: Fld => Boolean): Cls = Cls(fields.filter(p))

    def     mapFields(f: Fld =>     Fld ): Cls = Cls(fields.    map(f))
    def flatMapFields(f: Fld => Seq[Fld]): Cls = Cls(fields.flatMap(f))

    // ===========================================================================
    // both for val and meta

    private[meta /*cls*/] def rewrap(fields: Seq[Fld]) = Cls(fields.toList)

    // ===========================================================================
    private[meta /*cls*/] def requireNewKey   (target : Key ) = { requireNewKeys(Keyz.from(target)) }
    private[meta /*cls*/] def requireNewKeys  (targets: Keyz) = { targets.values.intersect(this.keys).in.someIf(!_.isEmpty).foreach(x => aptus.illegalState(s"201124171037:${x.#@@}(${keys.#@@})") /*TODO*/) }

    private[meta /*cls*/] def requireKnownKey (target : Key ) = { requireKnownKeys(Keyz.from(target)) }
    private[meta /*cls*/] def requireKnownKeys(targets: Keyz) = { targets.values.diff(this.keys).in.someIf(!_.isEmpty).foreach(x => aptus.illegalState(s"201124171038:${x.#@@}(${keys.#@@})") /*TODO*/) }

    // ---------------------------------------------------------------------------
    private[meta /*cls*/] def requireRenamingKey (target : RenW ) = { requireRenamingKeys(Renz.from(target)) }
    private[meta /*cls*/] def requireRenamingKeys(targets: RenWz) = { requireKnownKeys(targets.fromz); requireNewKeys  (targets.toz) }

    // ===========================================================================
    def aobj (o: Obj)             : AObj  = AObj (this, o)
    def aobjs(o1: Obj, more: Obj*): AObjs = AObjs(this, Objs.from((o1 +: more).toList))

    // ---------------------------------------------------------------------------
    def toObj : Obj = ClsToMetaObj.clsToObj(this)

    // ===========================================================================
    def formatJson        = toObj.formatCompactJson
    def formatCompactJson = toObj.formatCompactJson
    def formatPrettyJson  = toObj.formatPrettyJson

    def formatShort0: String = ClsToDebugString(this)
    def formatShort : String = ClsToDebugString(this).sectionAllOff("<root>")
    def formatFull  : String = ???//MetaObj.cls (this)

    def printShort() = { formatShort.p; () }
    def printFull () = { formatFull .p; () }

    def writeShort(out: String) = { formatShort.writeFileContent(out); () }
    def writeFull (out: String) = { formatFull .writeFileContent(out); () }

    // ===========================================================================
    def valueFromObj  (instantiator: Instantiator)(value: Any): Any = value.asInstanceOf[              Obj  ]      .pipe(instantiator.instantiateRecursively(this, _))
    def valueFromObjs (instantiator: Instantiator)(value: Any): Any = value.asInstanceOf[       Seq   [Obj] ]      .map (instantiator.instantiateRecursively(this, _))
    def valueFromObj_ (instantiator: Instantiator)(value: Any): Any = value.asInstanceOf[       Option[Obj] ]      .map (instantiator.instantiateRecursively(this, _))
    def valueFromObjs_(instantiator: Instantiator)(value: Any): Any = value.asInstanceOf[Option[Seq   [Obj]]].map(_.map (instantiator.instantiateRecursively(this, _)))

    // ---------------------------------------------------------------------------
    def valueToObj  (value: Any): Any = value.asInstanceOf[              Product  ]            .productIterator.pipe(ClsUtils.valuesToObj(this))
    def valueToObjs (value: Any): Any = value.asInstanceOf[       Seq   [Product] ]      .map(_.productIterator.pipe(ClsUtils.valuesToObj(this)))
    def valueToObj_ (value: Any): Any = value.asInstanceOf[       Option[Product] ]      .map(_.productIterator.pipe(ClsUtils.valuesToObj(this)))
    def valueToObjs_(value: Any): Any = value.asInstanceOf[Option[Seq   [Product]]].map(_.map(_.productIterator.pipe(ClsUtils.valuesToObj(this))))

    // ===========================================================================
    //TODO: ensure not nested type?
    def updateType(target: Key   , fromNode: TypeNode, toNode: TypeNode): Cls = transformField(target)(_.updateOptionality(toNode.isOption).updateSpecificSubInfo(
       fromNode.forceNonBObjSubInfo(enmOpt(target)),
         toNode.forceNonBObjSubInfo(enmOpt(target))))
    def updateType(target: Ren   , fromNode: TypeNode, toNode: TypeNode): Cls = rename(target).updateType(target.to, fromNode, toNode)

    def updateType(target: KPath , fromNode: TypeNode, toNode: TypeNode): Cls = transformx(target)(_.updateType(_, fromNode, toNode), _.updateType(_, fromNode, toNode))
    def updateType(target: RPath , fromNode: TypeNode, toNode: TypeNode): Cls = transformx(target)(_.updateType(_, fromNode, toNode), _.updateType(_, fromNode, toNode))

    def updateType(target: KPathz, fromNode: TypeNode, toNode: TypeNode): Cls = target.foldLeft(this)(_.updateType(_, fromNode, toNode))
    def updateType(target: RPathz, fromNode: TypeNode, toNode: TypeNode): Cls = target.foldLeft(this)(_.updateType(_, fromNode, toNode))

    // ---------------------------------------------------------------------------
    def updateTypex(target: Key, fromNode: TypeNode, toNode: TypeNode): Cls = transformField(target) { field =>
      field.updateSpecificValueType(
        fromNode.forceNonBObjSubInfo(enmOpt(field.key)).valueType,
          toNode.forceNonBObjSubInfo(enmOpt(target))   .valueType) }
    def updateTypex(target: Ren   , fromNode: TypeNode, toNode: TypeNode): Cls = rename(target).updateTypex(target.to, fromNode, toNode)

    def updateTypex(target: KPath , fromNode: TypeNode, toNode: TypeNode): Cls = transformx(target)(_.updateTypex(_, fromNode, toNode), _.updateTypex(_, fromNode, toNode))
    def updateTypex(target: RPath , fromNode: TypeNode, toNode: TypeNode): Cls = transformx(target)(_.updateTypex(_, fromNode, toNode), _.updateTypex(_, fromNode, toNode))

    def updateTypex(target: KPathz, fromNode: TypeNode, toNode: TypeNode): Cls = target.foldLeft(this)(_.updateTypex(_, fromNode, toNode))
    def updateTypex(target: RPathz, fromNode: TypeNode, toNode: TypeNode): Cls = target.foldLeft(this)(_.updateTypex(_, fromNode, toNode))

    // ---------------------------------------------------------------------------
    def updateSpecificInfo1(target: Key   , from: Info1, to: Info1): Cls = transformField(target)(_.updateSpecificInfo1(from, to))
    def updateSpecificInfo1(target: Ren   , from: Info1, to: Info1): Cls = rename(target).updateSpecificInfo1(target.to, from, to)

    def updateSpecificInfo1(target: KPath , from: Info1, to: Info1): Cls = transformx(target)(_.updateSpecificInfo1(_, from, to), _.updateSpecificInfo1(_, from, to))
    def updateSpecificInfo1(target: RPath , from: Info1, to: Info1): Cls = transformx(target)(_.updateSpecificInfo1(_, from, to), _.updateSpecificInfo1(_, from, to))

    def updateSpecificInfo1(target: KPathz, from: Info1, to: Info1): Cls = target.foldLeft(this)(_.updateSpecificInfo1(_, from, to))
    def updateSpecificInfo1(target: RPathz, from: Info1, to: Info1): Cls = target.foldLeft(this)(_.updateSpecificInfo1(_, from, to))

    // ---------------------------------------------------------------------------
    def updateInfo(target: Key   , info: Info): Cls = transformField(target)(_.transformInfo(_ => info))
    def updateInfo(target: Ren   , info: Info): Cls = rename(target).updateInfo(target.to, info)

    def updateInfo(target: KPath , info: Info): Cls = transformx(target)(_.updateInfo(_, info), _.updateInfo(_, info))
    def updateInfo(target: RPath , info: Info): Cls = transformx(target)(_.updateInfo(_, info), _.updateInfo(_, info))

    def updateInfo(target: KPathz, info: Info): Cls = target.foldLeft(this)(_.updateInfo(_, info))
    def updateInfo(target: RPathz, info: Info): Cls = target.foldLeft(this)(_.updateInfo(_, info))

    // ---------------------------------------------------------------------------
    def transformInfo(target: Key   , f: Info => Info): Cls = transformField(target)(_.transformInfo(f))
    def transformInfo(target: Ren   , f: Info => Info): Cls = rename(target).transformInfo(target.to, f)

    def transformInfo(target: KPath , f: Info => Info): Cls = transformx(target)(_.transformInfo(_, f), _.transformInfo(_, f))
    def transformInfo(target: RPath , f: Info => Info): Cls = transformx(target)(_.transformInfo(_, f), _.transformInfo(_, f))

    def transformInfo(target: KPathz, f: Info => Info): Cls = target.foldLeft(this)(_.transformInfo(_, f))
    def transformInfo(target: RPathz, f: Info => Info): Cls = target.foldLeft(this)(_.transformInfo(_, f))

    // ===========================================================================
    def transformField(target: Key   )(f: Fld => Fld): Cls =               _transformField(target)(f)
    def transformField(target: Ren   )(f: Fld => Fld): Cls = rename(target).transformField(target.to)(f)

    def transformField(target: KPath )(f: Fld => Fld): Cls = transformx(target)(_.transformField(_)(f), _.transformField(_)(f))
    def transformField(target: RPath )(f: Fld => Fld): Cls = transformx(target)(_.transformField(_)(f), _.transformField(_)(f))

    def transformField(target: KPathz)(f: Fld => Fld): Cls = target.foldLeft(this)(_.transformField(_)(f))
    def transformField(target: RPathz)(f: Fld => Fld): Cls = target.foldLeft(this)(_.transformField(_)(f))

      // ---------------------------------------------------------------------------
      // commonly needed

      def transformAllSubInfos     (target: RPathWz)(f: SubInfo => SubInfo): Cls = target.values.foldLeft(this)(_.transformAllSubInfos(_)(f))
      def transformAllSubInfos     (target: RPathW )(f: SubInfo => SubInfo): Cls = transformField(target.value)(_.transformAllSubInfos(f))

      def transformSoleSubInfo     (target: RPathWz)(f: SubInfo => SubInfo): Cls = target.values.foldLeft(this)(_.transformSoleSubInfo(_)(f))
      def transformSoleSubInfo     (target: RPathW )(f: SubInfo => SubInfo): Cls = transformField(target.value)(_.transformSoleSubInfo(f))

        def transformSoleValueType(target: RPathWz)(f: ValueType => ValueType): Cls = target.values.foldLeft(this)(_.transformSoleValueType(_)(f))
        def transformSoleValueType(target: RPathW )(f: ValueType => ValueType): Cls = transformField(target.value)(_.transformSoleValueType(f))

      // ---------------------------------------------------------------------------
      def transformNestedClasses  (target: RPathW)(f: Cls  => Cls): Cls = transformField(target.value)(_.transformNestedClasses       (f))
      def transformSoleNestedClass(target: RPathW)(f: Cls  => Cls): Cls = transformField(target.value)(_.transformSoleNestedClass     (f))

      def transformNestedClass(disambiguatorOpt: UnionObjectDisambiguatorOpt)(target: RPathW)(f: Cls  => Cls): Cls =
        transformField(target.value)(_.transformNestedClass(disambiguatorOpt)(f))

    // ===========================================================================
    def toRequired(key: RPathW): Cls = transformInfo(key.value, _.toRequired)
    def toOptional(key: RPathW): Cls = transformInfo(key.value, _.toOptional)

    def toMultiple(key: RPathW): Cls = transformAllSubInfos(key.value)(_.toMultiple)
    def toSingle  (key: RPathW): Cls = transformAllSubInfos(key.value)(_.toSingle)

    // ---------------------------------------------------------------------------
    def toRequired(path: RPath): Cls = transformx(path)(_ toRequired _, _ toRequired _)
    def toOptional(path: RPath): Cls = transformx(path)(_ toOptional _, _ toOptional _)

    def toMultiple(path: RPath): Cls = transformx(path)(_ toMultiple _, _ toMultiple _)
    def toSingle  (path: RPath): Cls = transformx(path)(_ toSingle   _, _ toSingle   _)

    // ===========================================================================
    def metaSchema: Cls  = MetaSchema.withDepth(maxDepth)
    def metaAObj  : AObj = AObj(metaSchema, toObj)
  }

// ===========================================================================
object Cls {
  lazy val Dummy   = Cls(List(Fld.Dummy))

  lazy val Content = cls(_content.string)
  lazy val Line    = cls(_line   .string) // TODO:opt?

  // ---------------------------------------------------------------------------
  lazy val FullDescriptiveStats = ClsConstants.FullDescriptiveStats
  lazy val FullPercentiles      = ClsConstants.FullPercentiles

  // ---------------------------------------------------------------------------
  def array(nesting: Cls) = cls(_array.clss(nesting))

  // ===========================================================================
  private[gallia] def vleInt: Cls = Cls(List(Fld.oneInt(_vle)))

  private[gallia] def vle (node: TypeNode): Cls = Cls(List(Fld(_vle, node.forceNonBObjInfo)))
  private[gallia] def vles(node: TypeNode): Cls = Cls(List(Fld(_vle, node.forceNonBObjInfo).toMultiple))

  // ---------------------------------------------------------------------------
  @PartialTypeMatching
    def oneBoolean(key: KeyW): Cls = Cls(List(key.value.boolean))
    def oneString (key: KeyW): Cls = Cls(List(key.value.string))
    def oneInt    (key: KeyW): Cls = Cls(List(key.value.int))
    def oneDouble (key: KeyW): Cls = Cls(List(key.value.double))

  // ---------------------------------------------------------------------------
  // TODO: or also detect file vs direct object?
  def fromFile  (schemaFilePath: String): Cls = meta.MetaObjToCls.clsFromFile  (schemaFilePath)
  def fromString(value: String)         : Cls = meta.MetaObjToCls.clsFromString(value)
  def fromObj   (value: Obj)            : Cls = meta.MetaObjToCls.clsFromObj   (value)

  // ---------------------------------------------------------------------------
  def from(keys: Seq[ Key]): Cls = from(keys.map(_.name))
  def from(keys: Seq[SKey])(implicit di: DI): Cls =
    keys
      .toList
      .map(skey => Fld(skey.symbol, Info.oneString))
      .pipe(Cls.apply)

}

// ===========================================================================
