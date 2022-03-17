package gallia
package data.json

import aptus._

// ===========================================================================
object ObjToGson {
  import com.google.gson._

  private lazy val Gson = new GsonBuilder().create()

  // ===========================================================================
  // TODO: t210115095838 - optimization: use schema (no need to pattern match seq/nesting, can use addElement, ...)!
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
      value match { // see BasicType

        case x: String  => Gson.toJsonTree(x)
        case x: Int     => Gson.toJsonTree(x)
        case x: Double  => Gson.toJsonTree(x)
        case x: Boolean => Gson.toJsonTree(x)
        
        case x: Obj     => apply(x)

        // ---------------------------------------------------------------------------
        case x: Byte    => Gson.toJsonTree(x)
        case x: Short   => Gson.toJsonTree(x)
        case x: Long    => Gson.toJsonTree(x)
        case x: Float   => Gson.toJsonTree(x)

        // ---------------------------------------------------------------------------
        case x: EnumEntry  => x.entryName.pipe(Gson.toJsonTree)

        // ---------------------------------------------------------------------------
        case x: BigInt     => x.toString /* stable */.pipe(Gson.toJsonTree)
        case x: BigDecimal => x.toString /* stable */.pipe(Gson.toJsonTree)
        
        // ---------------------------------------------------------------------------
        case x: LocalDateTime  => x.formatIso.pipe(Gson.toJsonTree)
        case x: LocalDate      => x.formatIso.pipe(Gson.toJsonTree)
        case x: LocalTime      => x.formatIso.pipe(Gson.toJsonTree)
        case x: OffsetDateTime => x.formatIso.pipe(Gson.toJsonTree)
        case x:  ZonedDateTime => x.formatIso.pipe(Gson.toJsonTree)
        case x: Instant        => x.formatIso.pipe(Gson.toJsonTree)
        
        // ---------------------------------------------------------------------------
        case x: ByteBuffer => x.array.toBase64.prepend("base64:").pipe(Gson.toJsonTree)

        // ---------------------------------------------------------------------------
        case x => illegalState(x.getClass, x)
      }

}

// ===========================================================================
