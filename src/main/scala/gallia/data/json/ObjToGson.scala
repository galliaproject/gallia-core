package gallia
package data.json

import aptus._
import data.DataFormatting

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
      Gson.toJsonTree(
        value match { // see BasicType
  
          case x: String     => x
          case x: Int        => x
          case x: Double     => x
          case x: Boolean    => x
          
          case x: Obj        => apply(x)
  
          // ---------------------------------------------------------------------------
          case x: EnumEntry  => x.entryName
  
          // ---------------------------------------------------------------------------
          case x: BigInt     => DataFormatting.formatBigInt(x)
          case x: BigDec     => DataFormatting.formatBigDec(x)
  
          // ---------------------------------------------------------------------------
          case x: Byte       => x
          case x: Short      => x
          case x: Long       => x
          case x: Float      => x
          
          // ---------------------------------------------------------------------------
          case x: Temporal   => DataFormatting.formatTemporal(x)
          
          // ---------------------------------------------------------------------------
          case x: ByteBuffer => DataFormatting.formatBinary(x)
  
        // ---------------------------------------------------------------------------
          case x => illegalState(x.getClass, x) })

}

// ===========================================================================
