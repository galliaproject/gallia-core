package gallia
package data
package json

import aptus._
import data.DataFormatting

// ===========================================================================
object ObjToGson {
  import com.google.gson._

  private lazy val Gson = new GsonBuilder().create()

  // ===========================================================================
  def apply(o: Obj): JsonObject = // TODO: t201230140315 - hopefully there's a more efficient way (no access to "members"?)...
      new JsonObject()
        .tap { mut =>
          o.data.foreach { case (k, v) =>
            mut.add(k.name, // note: underlying map "uses insertion order for iteration order"
              v match {
                case seq: Seq[_] => array(seq)
                case sgl         => element(sgl) }) } }

    // ===========================================================================
    private def array(seq: Seq[_]): JsonArray = {
      val elements: Seq[JsonElement] = seq.map(element)

      new JsonArray(/*elements.size; FIXME: t210315113950 - causes issues with EMR: look into more*/)
        .tap { array => elements.foreach(array.add) }
    }
    
    // ---------------------------------------------------------------------------
    @TypeMatching
    private def element(value: Any): JsonElement =
      Gson.toJsonTree(
        value match { // see BasicType

          case x: String     => x
          case x: Int        => x
          case x: Double     => x
          case x: Boolean    => x

          // ---------------------------------------------------------------------------
          case x: Obj        => apply(x)

          // ---------------------------------------------------------------------------
          case x: EnumValue   => x.stringValue

          // ---------------------------------------------------------------------------
          case x: Byte       => x
          case x: Short      => x
          case x: Long       => x
          case x: Float      => x

          // ---------------------------------------------------------------------------
          case x: BigInt     => DataFormatting.formatBigInt(x) // can't trust JSON with bignums
          case x: BigDec     => DataFormatting.formatBigDec(x) // can't trust JSON with bignums

          // ---------------------------------------------------------------------------
          case x: LocalDate      => DataFormatting.formatLocalDate(x)
          case x: LocalTime      => DataFormatting.formatLocalTime(x)
          case x: LocalDateTime  => DataFormatting.formatLocalDateTime (x)
          case x: OffsetDateTime => DataFormatting.formatOffsetDateTime(x)
          case x: ZonedDateTime  => DataFormatting.formatZonedDateTime (x)
          case x: Instant        => DataFormatting.formatInstant(x)

          // ---------------------------------------------------------------------------
          case x: ByteBuffer => DataFormatting.formatBinary(x)

          // ---------------------------------------------------------------------------
          case x => illegalState(x.getClass, x) })

}

// ===========================================================================
