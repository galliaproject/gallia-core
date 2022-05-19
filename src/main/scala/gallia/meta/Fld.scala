package gallia
package meta

import aptus.String_

// ===========================================================================
case class Fld(key: Key, info: Info) extends FldLike {
    require(key.name.nonEmpty)// TODO: validation

    final override protected val _info: Info         = info
    final override           def union: Seq[SubInfo] = _info.union

    // ---------------------------------------------------------------------------
    // mostly for macros (see t210330102827)
    private var _enumNameOpt: EnmNameOpt = None
      def setEnumName(enumName: EnmName) = { _enumNameOpt = Some(enumName); this }
      def forceEnumName: EnmName    = _enumNameOpt.get
      def enumNameOpt  : EnmNameOpt = _enumNameOpt

    // ---------------------------------------------------------------------------
    def toPNF(parent: Seq[Key]) = PNF(KPath(parent, key), info.optional, subInfo1.multiple, subInfo1.valueType)

    // ---------------------------------------------------------------------------
    override def toString = formatDefault
      def formatDefault = s"${key.name.padRight(16, ' ')}\t${info.formatDefault}"

    // ===========================================================================
    def transformKey (f: Key  => Key) : Fld = copy(key  = f(key))
    def transformInfo(f: Info => Info): Fld = copy(info = f(info))

    // ---------------------------------------------------------------------------
    def transformSoleSubInfo                               (f: SubInfo   => SubInfo)  : Fld = transformInfo(_.transformSoleSubInfo            (f))
    def transformAllSubInfos                               (f: SubInfo   => SubInfo)  : Fld = transformInfo(_.transformAllSubInfos            (f))
    def transformSpecificSubInfo  (p: SubInfo   => Boolean)(f: SubInfo   => SubInfo)  : Fld = transformInfo(_.transformSpecificSubInfo     (p)(f))
    def transformSpecificValueType(p: ValueType => Boolean)(f: ValueType => ValueType): Fld = transformInfo(_.transformSpecificValueType(p)(f))
    def transformSoleValueType                             (f: ValueType => ValueType): Fld = transformSoleSubInfo(_.transformValueType(f))

    // ---------------------------------------------------------------------------
    def transformNestedClasses  (f: Cls  => Cls): Fld = transformInfo(_.transformNestedClasses  (f))
    def transformSoleNestedClass(f: Cls  => Cls): Fld = transformInfo(_.transformSoleNestedClass(f))

    def transformNestedClass(target   : UnionObjectDisambiguator)   (f: Cls  => Cls): Fld = transformInfo(_.transformNestedClass(target)(f))
    def transformNestedClass(targetOpt: UnionObjectDisambiguatorOpt)(f: Cls  => Cls)(implicit di: DI): Fld =
      targetOpt
        .map { name => transformNestedClass(name)(f) }
        .getOrElse {   transformSoleNestedClass  (f) }

    // ---------------------------------------------------------------------------
    def updateKey        (value: Key)     : Fld = copy(key  = value)
    def updateInfo       (value: Info)    : Fld = copy(info = value)
    def updateOptionality(value: Optional): Fld = updateInfo(info.updateOptionality(value))

    // ---------------------------------------------------------------------------
    def updateSoleSubInfo                           (newValue: SubInfo)  : Fld = transformSoleSubInfo                             (_ => newValue)
    def updateSpecificSubInfo  (oldValue: SubInfo,   newValue: SubInfo)  : Fld = transformSpecificSubInfo  (_ == oldValue)        (_ => newValue)
    def updateSpecificValueType(oldValue: ValueType, newValue: ValueType): Fld = transformSpecificValueType(_ == oldValue)        (_ => newValue)
    def updateSpecificInfo1    (oldValue: Info1,     newValue: Info1)    : Fld = transformSpecificSubInfo  (_ == oldValue.subInfo)(_ => newValue.subInfo).updateOptionality(newValue.optional)

    // ===========================================================================
    // commonly used

      def toRequired: Fld = transformInfo(_.toRequired)
      def toOptional: Fld = transformInfo(_.toOptional)

      def toMultiple: Fld = transformSoleSubInfo(_.toMultiple)
      def toSingle  : Fld = transformSoleSubInfo(_.toSingle)

      // ---------------------------------------------------------------------------
      def toDouble: Fld = transformSoleSubInfo(_.toDouble) // see t210802091450
  }

  // ===========================================================================
  object Fld {
    val Dummy = Fld(Symbol("_dummy"), Info.oneString)

    // ---------------------------------------------------------------------------
    def one(key: Key, valueType: ValueType) = Fld(key, Info.one(valueType))
    def opt(key: Key, valueType: ValueType) = Fld(key, Info.opt(valueType))
    def nes(key: Key, valueType: ValueType) = Fld(key, Info.nes(valueType))
    def pes(key: Key, valueType: ValueType) = Fld(key, Info.pes(valueType))

    // ---------------------------------------------------------------------------
    def required(key: Key, subInfo: SubInfo) = Fld(key, Info.required(subInfo))
    def optional(key: Key, subInfo: SubInfo) = Fld(key, Info.optional(subInfo))

    // ---------------------------------------------------------------------------
    def oneCls(key: Key, c: Cls) = Fld(key, Info.one(c))
    def nesCls(key: Key, c: Cls) = Fld(key, Info.nes(c))

    // ---------------------------------------------------------------------------
    @PartialTypeMatching
      def oneString (key: Key) = Fld(key, Info.oneString)
      def oneInt    (key: Key) = Fld(key, Info.oneInt)
      def oneDouble (key: Key) = Fld(key, Info.oneDouble)
      def oneBoolean(key: Key) = Fld(key, Info.oneBoolean)

    // ===========================================================================
    @NumberAbstraction
    def isIntAndDouble(f1: Fld, f2: Fld): Boolean = // TODO: t210802091450 generalize
      (f1.isInt    && f2.isDouble) ||
      (f1.isDouble && f2.isInt   )
  }

// ===========================================================================
