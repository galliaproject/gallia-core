package gallia
package io.in

// ===========================================================================
sealed trait InputStringType {
    val isJsonObject: Boolean = this == InputStringType.JsonObject
  }

  // ===========================================================================
  object InputStringType {

    case object Indirection extends InputStringType

    case object JsonObject extends InputStringType
    case object JsonArray  extends InputStringType
    // TODO: consider more than JSON?

    // ===========================================================================
    def parse(inputString: String): InputStringType =
        inputString
          .dropWhile(_.isWhitespace)
          .headOption
          .pipe(_parse)

      // ---------------------------------------------------------------------------
      private def _parse(firstOpt: Option[Char]): InputStringType =
        firstOpt.map { first =>
               if (first == '{') JsonObject
          else if (first == '[') JsonArray
          else                   Indirection }
        .getOrElse(Indirection)

  }

// ===========================================================================
