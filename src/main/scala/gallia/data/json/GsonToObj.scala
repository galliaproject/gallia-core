package gallia.data.json

import aptus._
import aptus.aptmisc.Java
import aptus.aptjson.GsonParser

import scala.collection.JavaConverters._
import com.google.gson._

import gallia._

// ===========================================================================
/* important note: 201117103600 - JsonObject maintains its entries in a com.google.gson.internal.LinkedTreeMap
 * which despite its name maintains element order, though it requires a trick (see 201117103735) */
object GsonToObj {
  protected type AnyPrimitiveValue = Any

  // ---------------------------------------------------------------------------
  def fromGsonObject  (value: JsonObject)      : Obj = jsonObjectToObj(value)
  def fromObjectString(value: JsonObjectString): Obj = jsonObjectToObj(GsonParser.stringToJsonObject(value))
  def fromArrayString (value: JsonArrayString) : Objs = GsonParser.stringToJsonObjects(value).map(jsonObjectToObj).toList.pipe(Objs.from) // TODO: from iterator

  // ===========================================================================
  private[json] def jsonObjectToObj(value: JsonObject): Obj =
        value
          .entrySet()
          .iterator().asScala.toList // trick 201117103735; only iterator has been overriden to maintain order... (see 201117103600); likewise for KeySet
          .flatMap { entry =>
            jsonRootToAnyValue(entry.getValue)
              .map(entry.getKey.symbol -> _) }
          .pipe(Obj.fromIterable) //TODO: catch duplicates and so on

      // ---------------------------------------------------------------------------
      private def jsonRootToAnyValue(value: JsonElement): Option[AnyValue] =
          value match {
            case x: JsonPrimitive              => Some(primitiveValue(x))
            case x: JsonObject if (x.size > 0) => Some(jsonObjectToObj(x)) //FIXME: t210116150154 - must remove nulls prior to size
            case x: JsonArray                  => x.iterator().asScala.toList.flatMap(jsonRootToAnyValue).in.noneIf(_.isEmpty)
            case _: JsonObject                 => None
            case _: JsonNull                   => None }

        // ---------------------------------------------------------------------------
        private def primitiveValue(x: JsonPrimitive): AnyPrimitiveValue =
          // unfortunately no access to underlying Object value; TODO: t201103154749 - look into cost of reflection setAccessible(true)
               if (x.isString ) x.getAsString.asInstanceOf[String]
          else if (x.isBoolean) x.getAsBoolean
          else
            /*
             * TODO: can it actually be any of:
             * - AtomicInteger, AtomicLong,
             * - DoubleAccumulator, DoubleAdder,
             * - LongAccumulator, LongAdder */
            toScalaGson(x.getAsNumber) // TODO: in which case will "value: Object" actually already be a Number (aot a LazilyParsedNumber)?
  // ===========================================================================
  private def toScalaGson: PartialFunction[Number, Any] =
      Java.toScalaNumber
        .orElse(
      toScalaOther)

    // ---------------------------------------------------------------------------
    private def toScalaOther: PartialFunction[Number, Any] = {
      // stored as a String (but only accessible as int, long, float and double, ...)
      case z: com.google.gson.internal.LazilyParsedNumber => z.doubleValue()
      case z                                              => z.doubleValue()
    }

}

// ===========================================================================
