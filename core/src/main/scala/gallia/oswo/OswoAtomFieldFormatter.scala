package gallia
package oswo

import aptus._

// ===========================================================================
private object OswoAtomFieldFormatter { import source.SourceFluentBuilders._
  private val DeserializeFunction = fullName(classOf[OswoSerDes.type]) + ".deserializeFunction[Any => Any]"

  // ===========================================================================
  private implicit class OswoKey_(key: Key) {
    def formatKeySourceIndented: SourceString = key.name.padRight(16, ' ')
    def formatKeySource        : SourceString = key.name }

  // ===========================================================================
  implicit class OswoFld_(f: Fld) {
    def formatKeySourceIndented: SourceString = f.key.formatKeySourceIndented
    def formatKeySource        : SourceString = f.key.formatKeySource

    // ===========================================================================
    private def basicType: BasicType = f.forceInfo1.basicTypeOpt.get /* TODO: nesting */

    // ===========================================================================
    def fieldInitialization(o: Obj): source.SourceCode = // o.basicValue(${f.skey}).asInstanceOf[${basicType.formatScala}]
      ().argAssignment(f.formatKeySourceIndented)
          .eq(basicType.formatBasicType(f.key)(o))

    // ---------------------------------------------------------------------------
    def fieldDefault: SourceString = s"${f.formatKeySourceIndented} = x.${f.formatKeySource}"

    // ===========================================================================
    def fieldRename(target: ActualRen): SourceString = {
        if (f.key != target.from) fieldDefault else
          s"${target.to.formatKeySourceIndented} = x.${target.from.formatKeySource}" }

    // ---------------------------------------------------------------------------
    def fieldAdd(target: Key, value: AnyValue): SourceString =
      if (f.key != target) fieldDefault else
        s"${f.formatKeySourceIndented} = ${basicType.formatBasicType(value)}"

    // ===========================================================================
    def fieldFunctionIf(id: String, target: Key): SourceString = {
      // eg: f = f.toUpperCase, - or rather: f = deserializeFunction("id003")(x.f)
      if (f.key != target) fieldDefault else
        s"${f.formatKeySourceIndented} = ${DeserializeFunction}(gallia.serials(${id.quote}))(x.${f.formatKeySource}).asInstanceOf[${f.forceBasicType.formatScala /* TODO: containers */}]" }

    // ---------------------------------------------------------------------------
    def fieldConvertToDouble(target: Key): SourceString = {
      if (f.key != target) fieldDefault else
        s"${f.formatKeySourceIndented} = x.${f.formatKeySource}.toDouble" /* TODO: doubleValue unless String (t240130143719) - see 210106152701 */ } }

}

// ===========================================================================
