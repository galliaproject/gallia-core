package gallia
package meta

import aptus.{Seq_, String_}

// ===========================================================================
case class FldPair(field1: Fld, field2: Fld) {
  override def toString: String = formatDefault
    def formatDefault: String = Seq(field1.formatDefault, field2.formatDefault).section
}

// ===========================================================================
/** PNF = Potentially Nested Field */
case class PNF(path: KPath, info: Info) extends FldLike {
            override val key = path.key

  protected override val _container: Container = info.container
  protected override val _containee: Containee = info.containee
}

// ===========================================================================
case class Fld(key: Key, info: Info) extends FldLike {
    require(key.name.nonEmpty)// TODO: validation

    // ---------------------------------------------------------------------------
    // mostly for macros
    private var enumNameOpt: Option[String] = None
    def setEnumName(enumName: String) = { enumNameOpt = Some(enumName); this }
    def forceEnumName: String = enumNameOpt.get

    // ---------------------------------------------------------------------------
    protected override val _container: Container = info.container
    protected override val _containee: Containee = info.containee

    def toPNF(parent: Seq[Key]) = PNF(KPath(parent, key), info)

    // ---------------------------------------------------------------------------
    override def toString = formatDefault
      def formatDefault = s"${key.name.padRight(16, ' ')}\t${info.formatDefault}"

    // ===========================================================================
    def transformKey  (f: Key  => Key ): Fld = Fld(f(key),   info )
    def transformInfo (f: Info => Info): Fld = Fld(  key , f(info))

    // ===========================================================================
    def updateKey  (newValue: Key  ): Fld = Fld(newValue, info)
    def updateInfo (newValue: Info ): Fld = Fld(key, newValue)

    // ---------------------------------------------------------------------------
    def updateContainee(newValue: Containee): Fld = transformInfo(_.updateContainee(newValue))
    def updateContainer(newValue: Container): Fld = transformInfo(_.updateContainer(newValue))

    // ===========================================================================
    // commonly used

      def    toRequired: Fld = updateInfo(info.   toRequired)
      def toNonRequired: Fld = updateInfo(info.toNonRequired)

      def    toMultiple: Fld = updateInfo(info.   toMultiple)
      def toNonMultiple: Fld = updateInfo(info.toNonMultiple)

      def    toDouble: Fld = updateInfo(info.   toDouble)

      // ---------------------------------------------------------------------------
      def nestedClassOpt: Option[Cls] = info.nestingTypeOpt
  }

  // ===========================================================================
  object Fld {
    val Dummy = Fld(Symbol("_dummy"), Info.oneString)

    // ---------------------------------------------------------------------------
    def one(key: Key, containee: Containee) = Fld(key, Info(Container._One, containee))

    def oneCls(key: Key, c: Cls) = Fld(key, Info.one(c))
    def nesCls(key: Key, c: Cls) = Fld(key, Info.nes(c))

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
