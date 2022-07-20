package gallia
package data
package json

import com.google.gson._
import meta.{Info1, InfoLike}

// ===========================================================================
object GalliaToGsonData {
  private lazy val GsonInstance = new GsonBuilder().create()

  // ---------------------------------------------------------------------------
  def formatRecursively(c: Cls, o: Obj): aptus.JsonCompact = convertRecursively(c)(o).pipe(aptus.aptjson.GsonFormatter.compact)

  // ---------------------------------------------------------------------------
  def convertRecursively(c: Cls)(o: Obj): JsonObject =
    new JsonObject()
      .tap { rec =>
        c .fields
          .foreach { field =>
            o .attemptKey(field.key)
              .foreach { value =>
                rec.add(
                  field.skey,
                  processField(debug = field.skey)(field)(value)
                    .asInstanceOf[JsonElement /* note: JsonObject <: JsonElement */]) } } }

  // ===========================================================================
  def formatGsonJsonElement(fld: Fld)(value: AnyValue): String = formatGsonJsonElement(debug = fld.skey)(fld.info)(value)

  // ---------------------------------------------------------------------------
  def formatGsonJsonElement(debug: String)(info: InfoLike)(value: AnyValue): String =
    processField(debug)(info)(value)
      .asInstanceOf[JsonElement /* note: JsonArray and JsonObject <: JsonElement */]
      .toString /* stable */

  // ---------------------------------------------------------------------------
  private def processField(debug: String)(info: InfoLike)(value: AnyValue): AnyValue =
    info.valueExtractionWithMatching(debug)(value) {

      // ---------------------------------------------------------------------------
      nc => multiple =>
        if (!multiple) value.asInstanceOf[    Obj ].pipe { convertRecursively(nc) }
        else           value.asInstanceOf[Seq[Obj]].map  { convertRecursively(nc) }.pipe(array) } {

      // ---------------------------------------------------------------------------
      bsc => multiple =>
        if (!multiple)  value                     .pipe(_basicValue(bsc))
        else            value.asInstanceOf[Seq[_]].map (_basicValue(bsc)).pipe(array) }

  // ===========================================================================
  private def _basicValue(basicType: BasicType)(value: AnyValue): JsonElement =
    GsonInstance.toJsonTree(
      basicType match {
        case BasicType._Boolean => value
        case BasicType._Int     => value
        case BasicType._Double  => value
        case BasicType._String  => value

        // ---------------------------------------------------------------------------
        case _: BasicType._Enm => value.asInstanceOf[EnumValue].stringValue

        // ---------------------------------------------------------------------------
        case BasicType._Byte   => value
        case BasicType._Short  => value
        case BasicType._Long   => value
        case BasicType._Float  => value

        // ---------------------------------------------------------------------------
        case BasicType._BigInt => value.asInstanceOf[BigInt].bigInteger // gson only knows Java types
        case BasicType._BigDec => value.asInstanceOf[BigDec].bigDecimal // gson only knows Java types

        // ---------------------------------------------------------------------------
        case BasicType. _LocalDate     => value.asInstanceOf[ LocalDate]    .pipe(DataFormatting.formatLocalDate)
        case BasicType. _LocalTime     => value.asInstanceOf[ LocalTime]    .pipe(DataFormatting.formatLocalTime)
        case BasicType. _LocalDateTime => value.asInstanceOf[ LocalDateTime].pipe(DataFormatting.formatLocalDateTime)
        case BasicType._OffsetDateTime => value.asInstanceOf[OffsetDateTime].pipe(DataFormatting.formatOffsetDateTime)
        case BasicType. _ZonedDateTime => value.asInstanceOf[ ZonedDateTime].pipe(DataFormatting.formatZonedDateTime)
        case BasicType._Instant        => value.asInstanceOf[Instant]       .pipe(DataFormatting.formatInstant)

        // ---------------------------------------------------------------------------
        case BasicType._Binary => value.asInstanceOf[ByteBuffer].pipe(DataFormatting.formatBinary) })

  // ===========================================================================
  private def array(elements: Seq[JsonElement]): JsonArray =
    new JsonArray(/*elements.size; FIXME: t210315113950 - causes issues with EMR: look into more*/)
      .tap { array => elements.foreach(array.add) }

}

// ===========================================================================
