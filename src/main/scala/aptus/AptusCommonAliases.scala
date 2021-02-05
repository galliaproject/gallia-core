package aptus

// ===========================================================================
trait AptusCommonAliases {
  type Charset = java.nio.charset.Charset
  val  UTF_8   = java.nio.charset.StandardCharsets.UTF_8

  // ---------------------------------------------------------------------------
  type URI = java.net.URI
  type URL = java.net.URL

  // ---------------------------------------------------------------------------
  type Closeable = java.io.Closeable

  // ---------------------------------------------------------------------------
  type JavaPattern = java.util.regex.Pattern
  type Regex       = scala.util.matching.Regex
}

// ===========================================================================