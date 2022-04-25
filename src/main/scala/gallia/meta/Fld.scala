package gallia
package meta

import aptus.{Anything_, Seq_, String_}
import GalliaUtils.Seq__

// ===========================================================================
case class Fld(key: Key, infos: Seq[Info]) extends FldLike {
    require(key.name.nonEmpty)// TODO: validation
    infos
      .requireNonEmpty
      .requireDistinct
      .require( // a220411090125
        _.map(_.isRequired).distinct.size == 1,
        _.map(_.isRequired).@@)

    // ---------------------------------------------------------------------------
    def isUnionType: Boolean = infos.size > 1 // see t210125111338 (union types)
    def info1      : Info    = infos match {  // see t210125111338 (union types)
        case Seq(sole) => sole
        case seq       => aptus.unsupportedOperation("limited support for union types (see t210125111338)") }

      // ---------------------------------------------------------------------------
      def containee1: Containee = info1.containee
      def container1: Container = info1.container   

    // ---------------------------------------------------------------------------
    // mostly for macros
    private var _enumNameOpt: EnmNameOpt = None
      def setEnumName(enumName: EnmName) = { _enumNameOpt = Some(enumName); this }
      def forceEnumName: EnmName    = _enumNameOpt.get
      def enumNameOpt  : EnmNameOpt = _enumNameOpt

    // ---------------------------------------------------------------------------
    // see t210125111338 (union types)
    protected override val _containees: Seq[Containee] = infos.map(_.containee)
    protected override val _containers: Seq[Container] = infos.map(_.container)

    def toPNF(parent: Seq[Key]) = PNF(KPath(parent, key), info1)

    // ---------------------------------------------------------------------------
    override def toString = formatDefault
      def formatDefault = s"${key.name.padRight(16, ' ')}\t${infos.map(_.formatDefault).join("|")}" // see t210125111338 (union types)

    // ===========================================================================
    def transformKey(f: Key => Key): Fld = copy(key = f(key))

    def transformSoleInfo                        (f: Info => Info): Fld = copy(infos = f(info1))
    def transformAllInfos                        (f: Info => Info): Fld = copy(infos = infos.map(f))
    def transformSpecificInfo(p: Info => Boolean)(f: Info => Info): Fld = copy(infos = infos.mapAffectExactlyOne(p)(f)) // see t210125111338 (union types)

    // ---------------------------------------------------------------------------
    def transformNestedClasses                   (f: Cls  => Cls): Fld = copy(infos = infos.mapIf              (_.isNesting)              (_.transformNestedClass(f)))
    def transformSoleNestedClass                 (f: Cls  => Cls): Fld = copy(infos = infos.mapAffectExactlyOne(_.isNesting)              (_.transformNestedClass(f)))
    def transformNestedClass(name   : ClsName)   (f: Cls  => Cls): Fld = copy(infos = infos.mapIf              (_.isNestingWithName(name))(_.transformNestedClass(f)))
    def transformNestedClass(nameOpt: ClsNameOpt)(f: Cls  => Cls): Fld =
      nameOpt
        .map { name => transformNestedClass(name)(f) }
        .getOrElse {   transformSoleNestedClass  (f) }

    // ---------------------------------------------------------------------------
    def updateKey                         (newValue: Key ): Fld = copy(key = newValue)
    def updateSoleInfo                    (newValue: Info): Fld = transformSoleInfo(_ => newValue)
    def updateSpecificInfo(oldValue: Info, newValue: Info): Fld = transformSpecificInfo(_ == oldValue)(_ => newValue) // see t210125111338 (union types)

    // ===========================================================================
    // commonly used

      def    toRequired: Fld = transformAllInfos(_.toRequired)
      def toNonRequired: Fld = transformAllInfos(_.toNonRequired)

      def    toMultiple: Fld = transformSoleInfo(_.toMultiple)
      def toNonMultiple: Fld = transformSoleInfo(_.toNonMultiple)

      // ---------------------------------------------------------------------------
      def toDouble: Fld = transformSoleInfo(_.toDouble) // see t210802091450
  }

  // ===========================================================================
  object Fld {
    val Dummy = Fld(Symbol("_dummy"), Info.oneString)

    // ---------------------------------------------------------------------------
    def one(key: Key, containee: Containee) = Fld(key, Info(Container._One, containee))
    def opt(key: Key, containee: Containee) = Fld(key, Info(Container._Opt, containee))
    def nes(key: Key, containee: Containee) = Fld(key, Info(Container._Nes, containee))
    def pes(key: Key, containee: Containee) = Fld(key, Info(Container._Pes, containee))

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
