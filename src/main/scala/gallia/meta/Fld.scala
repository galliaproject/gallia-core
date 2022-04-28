package gallia
package meta

import aptus.String_

// ===========================================================================
case class Fld(key: Key, ofni: Ofni) extends FldLike {
    require(key.name.nonEmpty)// TODO: validation

    final override protected val _ofni: Ofni      = ofni
    final override           def infos: Seq[Info] = _ofni.infos

    // ---------------------------------------------------------------------------
    // mostly for macros
    private var _enumNameOpt: EnmNameOpt = None
      def setEnumName(enumName: EnmName) = { _enumNameOpt = Some(enumName); this }
      def forceEnumName: EnmName    = _enumNameOpt.get
      def enumNameOpt  : EnmNameOpt = _enumNameOpt

    // ---------------------------------------------------------------------------
    def toPNF(parent: Seq[Key]) = PNF(KPath(parent, key), ofni.optional, info1.multiple, info1.containee)

    // ---------------------------------------------------------------------------
    override def toString = formatDefault
      def formatDefault = s"${key.name.padRight(16, ' ')}\t${ofni.formatDefault}"

    // ===========================================================================
    def transformKey (f: Key  => Key) : Fld = copy(key  = f(key))
    def transformOfni(f: Ofni => Ofni): Fld = copy(ofni = f(ofni))

    // ---------------------------------------------------------------------------
    def transformSoleInfo                        (f: Info => Info): Fld = transformOfni(_.transformSoleInfo       (f))
    def transformAllInfos                        (f: Info => Info): Fld = transformOfni(_.transformAllInfos       (f))
    def transformSpecificInfo(p: Info => Boolean)(f: Info => Info): Fld = transformOfni(_.transformSpecificInfo(p)(f)) // see t210125111338 (union types)

    // ---------------------------------------------------------------------------
    def transformNestedClasses                   (f: Cls  => Cls): Fld = transformOfni(_.transformNestedClasses    (f))
    def transformSoleNestedClass                 (f: Cls  => Cls): Fld = transformOfni(_.transformSoleNestedClass  (f))
    def transformNestedClass(name   : ClsName)   (f: Cls  => Cls): Fld = transformOfni(_.transformNestedClass(name)(f))
    def transformNestedClass(nameOpt: ClsNameOpt)(f: Cls  => Cls): Fld =
      nameOpt
        .map { name => transformNestedClass(name)(f) }
        .getOrElse {   transformSoleNestedClass  (f) }

    // ---------------------------------------------------------------------------
    def updateKey        (value: Key)     : Fld = copy(key  = value)
    def updateOfni       (value: Ofni)    : Fld = copy(ofni = value)
    def updateOptionality(value: Optional): Fld = updateOfni(ofni.updateOptionality(value))

    // ---------------------------------------------------------------------------
    def updateSoleInfo                    (newValue: Info): Fld = transformSoleInfo(_ => newValue)
    def updateSpecificInfo(oldValue: Info, newValue: Info): Fld = transformSpecificInfo(_ == oldValue)(_ => newValue) // see t210125111338 (union types)

    // ===========================================================================
    // commonly used

      def    toRequired: Fld = transformOfni(_.toRequired)
      def toNonRequired: Fld = transformOfni(_.toOptional)

      def    toMultiple: Fld = transformSoleInfo(_.toMultiple)
      def toNonMultiple: Fld = transformSoleInfo(_.toSingle)

      // ---------------------------------------------------------------------------
      def toDouble: Fld = transformSoleInfo(_.toDouble) // see t210802091450
  }

  // ===========================================================================
  object Fld {
    val Dummy = Fld(Symbol("_dummy"), Ofni.oneString)

    // ---------------------------------------------------------------------------
    def one(key: Key, containee: Containee) = Fld(key, Ofni.one(containee))
    def opt(key: Key, containee: Containee) = Fld(key, Ofni.opt(containee))
    def nes(key: Key, containee: Containee) = Fld(key, Ofni.nes(containee))
    def pes(key: Key, containee: Containee) = Fld(key, Ofni.pes(containee))

    // ---------------------------------------------------------------------------
    def oneCls(key: Key, c: Cls) = Fld(key, Ofni.one(c))
    def nesCls(key: Key, c: Cls) = Fld(key, Ofni.nes(c))

    // ---------------------------------------------------------------------------
    @PartialTypeMatching
      def oneString (key: Key) = Fld(key, Ofni.oneString)
      def oneInt    (key: Key) = Fld(key, Ofni.oneInt)
      def oneDouble (key: Key) = Fld(key, Ofni.oneDouble)
      def oneBoolean(key: Key) = Fld(key, Ofni.oneBoolean)

    // ===========================================================================
    @NumberAbstraction
    def isIntAndDouble(f1: Fld, f2: Fld): Boolean = // TODO: t210802091450 generalize
      (f1.isInt    && f2.isDouble) ||
      (f1.isDouble && f2.isInt   )
  }

// ===========================================================================
