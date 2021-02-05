package gallia.data.json

import com.google.gson._
import aptus.Anything_
import gallia._

// ===========================================================================
object ObjToGson {

  private lazy val Gson = new GsonBuilder().create()
  private lazy val SystemDefaultZoneId = java.time.ZoneId.systemDefault

  // ===========================================================================
  // TODO: t210115095838 - optimization: use schema (no need to pattern match seq/nesting, can use addElement, ...)!
  def apply(o: Obj): JsonObject = // TODO: t201230140315 - hopefully there's a more efficient way (no access to "members"?)...
      new JsonObject()
        .sideEffect { mut =>
          o.data.foreach { case (k, v) =>
            mut.add(k.name, // note: underlying map "uses insertion order for iteration order"
              v match {
                case seq: Seq[_] => array(seq)
                case sgl         => element(sgl) }) } }

    // ===========================================================================
    @gallia.TypeMatching
    private def element(value: Any): JsonElement =
      value match {
        case o: Obj                  => apply(o)

        case e: enumeratum.EnumEntry => e.entryName.thn(Gson.toJsonTree)
        //TODO: t210110095228 - scala enum

        // can't seem to get JsonSerializer to work
        // TODO: t210116162405 - don't use timestamp?
        case x: LocalDate     => x                            .toEpochDay   .thn(Gson.toJsonTree) // or rely on gson's default serialization?
        case x: LocalDateTime => x.atZone(SystemDefaultZoneId).toEpochSecond.thn(Gson.toJsonTree)

        case x: BigInt     => x.toString /* TODO: ok? */.thn(Gson.toJsonTree) // or rely on gson's default serialization?
        case x: BigDecimal => x.toString /* TODO: ok? */.thn(Gson.toJsonTree)

        case b => Gson.toJsonTree(b) }

    // ---------------------------------------------------------------------------
    private def array(seq: Seq[_]): JsonArray = {
      val elements: Seq[JsonElement] = seq.map(element)

      new JsonArray(elements.size)
        .sideEffect { array =>
          elements.foreach(array.add) }
    }

}

// ===========================================================================
