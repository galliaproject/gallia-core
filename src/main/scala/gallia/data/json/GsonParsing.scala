package gallia
package data.json

import com.google.gson.{JsonObject, JsonArray}
import scala.collection.JavaConverters._

import aptus.{JsonObjectString, JsonArrayString}

// ===========================================================================
object GsonParsing {
  private def illegal(anys: Any*) = throw new IllegalArgumentException(anys.mkString(","))

  // ===========================================================================
  def parseObject(value: JsonObject): Obj  = GsonToObj.jsonObjectToObj(value)

  // ---------------------------------------------------------------------------
  def parseObject(value: JsonObjectString): Obj  =
    util.Try(GsonToObj.fromObjectString(value)) match {
      case util.Failure(throwable) => illegal(s"\n\t${throwable.getMessage}\n\t\t${value}")
      case util.Success(success) => success }

  // ===========================================================================
  def parseObject(value: JsonArray): Seq[Obj] =
      value
        .iterator
        .asScala
        .map(_.getAsJsonObject)
        .map(GsonToObj.jsonObjectToObj)
        .toList

  // ---------------------------------------------------------------------------
  def parseArray(value: JsonArrayString): List[Obj] = // TODO: t201221175254 - stream rather than read all in memory
    util.Try(GsonToObj.fromArrayString(value)) match {
      case util.Failure(throwable) => illegal(throwable.getMessage, value)
      case util.Success(success)   => success.toListAndTrash }

}

// ===========================================================================
