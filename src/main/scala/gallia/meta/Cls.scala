package gallia
package meta

import aptus.{Anything_, String_, Seq_}

import target._

// ===========================================================================
case class Cls(fields: Seq[Fld]) // TODO: as List?
      extends meta.Containee
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

      def formatDefault =
        fields
          .map(_.formatDefault)
          .section(nameOpt.map(_.quote.colon).getOrElse("cls:"))

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
    def aobjs(o1: Obj, more: Obj*): AObjs = AObjs(this, Objs.from(o1 +: more))

    def toObj : Obj = MetaObj.cls(this)
    @deprecated
    def toObj2: Obj = MetaObj.cls(this)

    // ===========================================================================
    def formatJson        = toObj.formatCompactJson
    def formatCompactJson = toObj.formatCompactJson
    def formatPrettyJson  = toObj.formatPrettyJson

    def formatShort0: String = MetaObj.formatClassDebug(this)
    def formatShort : String = MetaObj.formatClassDebug(this).sectionAllOff("<root>")
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
    def updateType(target: Key   , fromNode: TypeNode, toNode: TypeNode): Cls = transformField(target)(_.updateOptionality(toNode.isOption).updateSpecificInfo(fromNode.forceNonBObjInfo(enmOpt(target)), toNode.forceNonBObjInfo(enmOpt(target))))
    def updateType(target: Ren   , fromNode: TypeNode, toNode: TypeNode): Cls = rename(target).updateType(target.to, fromNode, toNode)

    def updateType(target: KPath , fromNode: TypeNode, toNode: TypeNode): Cls = transformx(target)(_.updateType(_, fromNode, toNode), _.updateType(_, fromNode, toNode))
    def updateType(target: RPath , fromNode: TypeNode, toNode: TypeNode): Cls = transformx(target)(_.updateType(_, fromNode, toNode), _.updateType(_, fromNode, toNode))

    def updateType(target: KPathz, fromNode: TypeNode, toNode: TypeNode): Cls = target.foldLeft(this)(_.updateType(_, fromNode, toNode))
    def updateType(target: RPathz, fromNode: TypeNode, toNode: TypeNode): Cls = target.foldLeft(this)(_.updateType(_, fromNode, toNode))

    // ---------------------------------------------------------------------------
    def updateTypex(target: Key, fromNode: TypeNode, toNode: TypeNode): Cls = transformField(target) { field =>
      field.updateSpecificContainee(
        fromNode.forceNonBObjInfo(enmOpt(field.key)).containee,
          toNode.forceNonBObjInfo(enmOpt(target))   .containee) }
    def updateTypex(target: Ren   , fromNode: TypeNode, toNode: TypeNode): Cls = rename(target).updateTypex(target.to, fromNode, toNode)

    def updateTypex(target: KPath , fromNode: TypeNode, toNode: TypeNode): Cls = transformx(target)(_.updateTypex(_, fromNode, toNode), _.updateTypex(_, fromNode, toNode))
    def updateTypex(target: RPath , fromNode: TypeNode, toNode: TypeNode): Cls = transformx(target)(_.updateTypex(_, fromNode, toNode), _.updateTypex(_, fromNode, toNode))

    def updateTypex(target: KPathz, fromNode: TypeNode, toNode: TypeNode): Cls = target.foldLeft(this)(_.updateTypex(_, fromNode, toNode))
    def updateTypex(target: RPathz, fromNode: TypeNode, toNode: TypeNode): Cls = target.foldLeft(this)(_.updateTypex(_, fromNode, toNode))

    // ---------------------------------------------------------------------------
    def updateSpecificOfnu(target: Key   , from: Ofnu, to: Ofnu): Cls = transformField(target)(_.updateSpecificOfnu(from, to))
    def updateSpecificOfnu(target: Ren   , from: Ofnu, to: Ofnu): Cls = rename(target).updateSpecificOfnu(target.to, from, to)

    def updateSpecificOfnu(target: KPath , from: Ofnu, to: Ofnu): Cls = transformx(target)(_.updateSpecificOfnu(_, from, to), _.updateSpecificOfnu(_, from, to))
    def updateSpecificOfnu(target: RPath , from: Ofnu, to: Ofnu): Cls = transformx(target)(_.updateSpecificOfnu(_, from, to), _.updateSpecificOfnu(_, from, to))

    def updateSpecificOfnu(target: KPathz, from: Ofnu, to: Ofnu): Cls = target.foldLeft(this)(_.updateSpecificOfnu(_, from, to))
    def updateSpecificOfnu(target: RPathz, from: Ofnu, to: Ofnu): Cls = target.foldLeft(this)(_.updateSpecificOfnu(_, from, to))

    // ---------------------------------------------------------------------------
    def updateOfni(target: Key   , ofni: Ofni): Cls = transformField(target)(_.transformOfni(_ => ofni))
    def updateOfni(target: Ren   , ofni: Ofni): Cls = rename(target).updateOfni(target.to, ofni)

    def updateOfni(target: KPath , ofni: Ofni): Cls = transformx(target)(_.updateOfni(_, ofni), _.updateOfni(_, ofni))
    def updateOfni(target: RPath , ofni: Ofni): Cls = transformx(target)(_.updateOfni(_, ofni), _.updateOfni(_, ofni))

    def updateOfni(target: KPathz, ofni: Ofni): Cls = target.foldLeft(this)(_.updateOfni(_, ofni))
    def updateOfni(target: RPathz, ofni: Ofni): Cls = target.foldLeft(this)(_.updateOfni(_, ofni))

    // ---------------------------------------------------------------------------
    def transformOfni(target: Key   , f: Ofni => Ofni): Cls = transformField(target)(_.transformOfni(f))
    def transformOfni(target: Ren   , f: Ofni => Ofni): Cls = rename(target).transformOfni(target.to, f)

    def transformOfni(target: KPath , f: Ofni => Ofni): Cls = transformx(target)(_.transformOfni(_, f), _.transformOfni(_, f))
    def transformOfni(target: RPath , f: Ofni => Ofni): Cls = transformx(target)(_.transformOfni(_, f), _.transformOfni(_, f))

    def transformOfni(target: KPathz, f: Ofni => Ofni): Cls = target.foldLeft(this)(_.transformOfni(_, f))
    def transformOfni(target: RPathz, f: Ofni => Ofni): Cls = target.foldLeft(this)(_.transformOfni(_, f))

    // ===========================================================================
    def transformField(target: Key   )(f: Fld => Fld): Cls =               _transformField(target)(f)
    def transformField(target: Ren   )(f: Fld => Fld): Cls = rename(target).transformField(target.to)(f)

    def transformField(target: KPath )(f: Fld => Fld): Cls = transformx(target)(_.transformField(_)(f), _.transformField(_)(f))
    def transformField(target: RPath )(f: Fld => Fld): Cls = transformx(target)(_.transformField(_)(f), _.transformField(_)(f))

    def transformField(target: KPathz)(f: Fld => Fld): Cls = target.foldLeft(this)(_.transformField(_)(f))
    def transformField(target: RPathz)(f: Fld => Fld): Cls = target.foldLeft(this)(_.transformField(_)(f))

      // ---------------------------------------------------------------------------
      // commonly needed

      def transformAllInfos     (target: RPathWz)(f: Info => Info): Cls = target.values.foldLeft(this)(_.transformAllInfos(_)(f))
      def transformAllInfos     (target: RPathW )(f: Info => Info): Cls = transformField(target.value)(_.transformAllInfos(f))

      def transformSoleInfo     (target: RPathWz)(f: Info => Info): Cls = target.values.foldLeft(this)(_.transformSoleInfo(_)(f))
      def transformSoleInfo     (target: RPathW )(f: Info => Info): Cls = transformField(target.value)(_.transformSoleInfo(f))

        def transformSoleContainee(target: RPathWz)(f: Containee => Containee): Cls = target.values.foldLeft(this)(_.transformSoleContainee(_)(f))
        def transformSoleContainee(target: RPathW )(f: Containee => Containee): Cls = transformField(target.value)(_.transformSoleContainee(f))

      // ---------------------------------------------------------------------------
      def transformNestedClasses  (target: RPathW)(f: Cls  => Cls): Cls = transformField(target.value)(_.transformNestedClasses       (f))
      def transformSoleNestedClass(target: RPathW)(f: Cls  => Cls): Cls = transformField(target.value)(_.transformSoleNestedClass     (f))

      def transformNestedClass(disambiguatorOpt: UnionObjectDisambiguatorOpt)(target: RPathW)(f: Cls  => Cls): Cls =
        transformField(target.value)(_.transformNestedClass(disambiguatorOpt)(f))

    // ===========================================================================
    def    toRequired(key: RPathW): Cls = transformOfni(key.value, _.toRequired)
    def toNonRequired(key: RPathW): Cls = transformOfni(key.value, _.toOptional)

    def    toMultiple(key: RPathW): Cls = transformAllInfos(key.value)(_.toMultiple)
    def toNonMultiple(key: RPathW): Cls = transformAllInfos(key.value)(_.toSingle)

    // ---------------------------------------------------------------------------
    def    toRequired(path: RPath): Cls = transformx(path)(_    toRequired _, _    toRequired _)
    def toNonRequired(path: RPath): Cls = transformx(path)(_ toNonRequired _, _ toNonRequired _)

    def    toMultiple(path: RPath): Cls = transformx(path)(_    toMultiple _, _    toMultiple _)
    def toNonMultiple(path: RPath): Cls = transformx(path)(_ toNonMultiple _, _ toNonMultiple _)
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

  private[gallia] def vle (node: TypeNode): Cls = Cls(List(Fld(_vle, node.forceNonBObjOfni)))
  private[gallia] def vles(node: TypeNode): Cls = Cls(List(Fld(_vle, node.forceNonBObjOfni).toMultiple))

  // ---------------------------------------------------------------------------
  @PartialTypeMatching
    def oneBoolean(key: KeyW): Cls = Cls(List(key.value.boolean))
    def oneString (key: KeyW): Cls = Cls(List(key.value.string))
    def oneInt    (key: KeyW): Cls = Cls(List(key.value.int))
    def oneDouble (key: KeyW): Cls = Cls(List(key.value.double))

  // ---------------------------------------------------------------------------
  def fromFile  (schemaFilePath: String): Cls = meta.MetaObj.clsFromFile  (schemaFilePath) // TODO: or also detect file vs direct object?
  def fromString(value: String)         : Cls = meta.MetaObj.clsFromString(value)          // TODO: or also detect file vs direct object?
  
  // ---------------------------------------------------------------------------
  def from(keys: Seq[ Key]): Cls = from(keys.map(_.name))
  def from(keys: Seq[SKey])(implicit di: DI): Cls =
    keys
      .toList
      .map(skey => Fld(skey.symbol, Ofni.oneString))
      .pipe(Cls.apply)

}

// ===========================================================================
