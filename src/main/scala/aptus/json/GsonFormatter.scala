package aptus.json

import com.google.gson._
import aptus.JsonString

// ===========================================================================
object GsonFormatter { // TODO: also handle yaml here? (see https://github.com/google/gson/issues/327)
  private lazy val Compact = new GsonBuilder().disableHtmlEscaping()                    .create()
  private lazy val Pretty  = new GsonBuilder().disableHtmlEscaping().setPrettyPrinting().create()

  // ===========================================================================
  def compact(value: JsonElement): String = Compact.toJson(value)
  def pretty (value: JsonElement): String = Pretty .toJson(value)

    // ---------------------------------------------------------------------------
    def compact(value: JsonObject): String = Compact.toJson(value)
    def pretty (value: JsonObject): String = Pretty .toJson(value)

    // ---------------------------------------------------------------------------
    def compact(value: JsonArray): String = Compact.toJson(value)
    def pretty (value: JsonArray): String = Pretty .toJson(value)

  // ===========================================================================
  // reformatting

  def compact(value: JsonString): Option[String] = GsonParser.parseString(value).map(_.fold(compact, compact))
  def pretty (value: JsonString): Option[String] = GsonParser.parseString(value).map(_.fold(pretty , pretty ))
}

// ===========================================================================
