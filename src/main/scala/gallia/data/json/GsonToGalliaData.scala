package gallia
package data
package json

import aptus._
import aptus.aptjson.GsonParser

// ===========================================================================
object GsonToGalliaData {

  def parseRecursively(c: Cls, jsonString: String): Obj =
    jsonString
      .pipe(GsonParsing.parseObject)
      .pipe(GsonToGalliaData.convertRecursively(c))

  // ---------------------------------------------------------------------------
  def convertRecursively(c: Cls)(o: Obj): Obj =
      c .fields
        .flatMap { field =>
          c.unknownKeys(o).assert(_.isEmpty) // necessary for union types (see 220615165554)

          o .attemptKey(field.key)
            .map { value =>
              field.key ->
                processField(field.info)(value) } }
      .pipe(gallia.obj)

  // ===========================================================================
  def parseGsonJsonElement(info: meta.InfoLike)(value: String): AnyValue =
      parseJsonString(info.isNesting, info.subInfo1.multiple)(value)
        .pipe(processField(info))

  // ===========================================================================
  private def processField(info: meta.InfoLike)(value: AnyValue): AnyValue =
    info.valueExtractionWithFailures {

      // ---------------------------------------------------------------------------
      nestedGalliaClass => multiple =>
        if (!multiple) value.asInstanceOf[    Obj  ].pipe(convertRecursively(nestedGalliaClass))
        else           value.asInstanceOf[Seq[Obj ]].map (convertRecursively(nestedGalliaClass)) } {

      // ---------------------------------------------------------------------------
      bsc => multiple =>
        if (!multiple) value                            .pipe(_basicValue(bsc))
        else           value.asInstanceOf[Seq[_]].toList.map (_basicValue(bsc)) }

  // ===========================================================================
  private def _basicValue(basicType: BasicType)(value: AnyValue): AnyValue =
    basicType match {
      case BasicType._String  => value.asInstanceOf[String]
      case BasicType._Boolean => value.asInstanceOf[Boolean]
      case BasicType._Double  => value.asInstanceOf[Double]
      case BasicType._Int     => BasicType._Int.parseAnyToDouble(value)

      // ---------------------------------------------------------------------------
      case _: BasicType._Enm  => BasicType._Enm.parseStringAsAny(value)

      // ---------------------------------------------------------------------------
      case BasicType._Long    => BasicType._Long .parseAnyToDouble(value)
      case BasicType._Float   => BasicType._Float.parseAnyToDouble(value)

      case BasicType._Byte    => BasicType._Byte .parseAnyToDouble(value)
      case BasicType._Short   => BasicType._Short.parseAnyToDouble(value)

      // ---------------------------------------------------------------------------
      case BasicType._BigInt  => stringOrLong  (BigInt    .apply, BigInt    .apply)(value)
      case BasicType._BigDec  => stringOrDouble(BigDecimal.apply, BigDecimal.apply)(value)

      // ---------------------------------------------------------------------------
      case BasicType._LocalDate     => stringOrLong(BasicType._LocalDate    .pair)(value)
      case BasicType._LocalDateTime => stringOrLong(BasicType._LocalDateTime.pair)(value)
      case BasicType._Instant       => stringOrLong(BasicType._Instant      .pair)(value)

      // ---------------------------------------------------------------------------
      case BasicType.     _LocalTime => BasicType._LocalTime     .parseStringAsAny(value)
      case BasicType._OffsetDateTime => BasicType._OffsetDateTime.parseStringAsAny(value)
      case BasicType. _ZonedDateTime => BasicType._ZonedDateTime .parseStringAsAny(value)

      // ---------------------------------------------------------------------------
      case BasicType._Binary => BasicType._Binary.parseStringAsAny(value)
    }

  // ===========================================================================
  private[json] def stringOrLong[T](pair:    (String => T,         Long => T)): Any => T = stringOrLong(pair._1, pair._2)
  private[json] def stringOrLong[T](ifString: String => T, ifLong: Long => T) : Any => T =
    _ match {
      case s: String => ifString(s)
      case n: Number => ifLong  (numberToLong(n)) }

  // ---------------------------------------------------------------------------
  private[json] def stringOrDouble[T](ifString: String => T, ifDouble: Double => T): Any => T =
    _ match {
      case s: String => ifString(s)
      case n: Number => ifDouble(n.doubleValue) }

  // ===========================================================================
  private[json] def numberToLong(n: Number): Long =
    n .doubleValue
      .assert(meta.basic.BasicTypeUtils.doubleFitsLong)
      .pipe(d => d.toLong.ensuring(_.toDouble == d))

  // ===========================================================================
  private def parseJsonString(nesting: Boolean, multiple: Boolean)(value: JsonString): AnyValue =
    (nesting, multiple) match { // spilling for instance does not support union types
      case (false, false) => GsonParser.stringToPrimitiveValueAny (value) /*     Vle  */
      case (false, true ) => GsonParser.stringToPrimitiveValueAnys(value) /* Seq[Vle] */
      case (true , false) => GsonToObj .fromObjectString          (value) /*     Obj  */
      case (true , true ) => GsonToObj .fromArrayString           (value) /* Seq[Obj] */ }

}

// ===========================================================================
