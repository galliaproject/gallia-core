package aptus.json

import aptus.JsonString
import aptus.JsonObjectString
import aptus.JsonArrayString

import scala.collection.JavaConverters._
import com.google.gson._

// ===========================================================================
object GsonParser {

  /** we're not interested in literals */
  def parseString(value: JsonString): Option[Either[JsonObject, JsonArray]] =
    value
      .dropWhile(c =>
        c != '{' &&
        c != '[')
      .headOption
      .map { first =>
        if (first == '{') Left (stringToJsonObject(value))
        else              Right(stringToJsonArray (value)) }

  // ---------------------------------------------------------------------------
  def stringToJsonObject(value: JsonObjectString): JsonObject =
    JsonParser
      .parseString(value)
      .getAsJsonObject

  // ---------------------------------------------------------------------------
  def stringToJsonArray(value: JsonArrayString): JsonArray =
    JsonParser
      .parseString(value)
      .getAsJsonArray

  // ---------------------------------------------------------------------------
  def stringToJsonObjects(value: JsonArrayString): Iterator[JsonObject] =
    stringToJsonArray(value)
      .iterator().asScala
      .map(_.getAsJsonObject)

}

// ===========================================================================
