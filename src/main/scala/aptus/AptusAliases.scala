package aptus

// ===========================================================================
/** typically these are meant to convey intent, not enforce invariants; often sufficient to replace a comment. */
trait AptusAliases { // TODO: t210125110827 - consider AnyVals rather? overkill?

  type Size              = Int
  type Sum               = Int
  type MirrorIndex       = Int
  type Count             = Int
  type Index             = Int
  type Rank              = Int // = index + 1

  // ---------------------------------------------------------------------------
  type Separator         = String

  type Name              = String
  type Label             = String

  type Line              = String
  type Content           = String

  type FilePath          = String
  type FileName          = String

  type JsonString        = String
  type JsonPretty        = String
  type JsonCompact       = String
  type JsonArrayString   = String
  type JsonObjectString  = String

  type UrlString         = String
  type UriString         = String

  type TableName         = String
  type DatabaseName      = String

  type StringValue       = String
  type IdValue           = String
  type DebugString       = String
  type Cell              = String
  type Prefix            = String
  type QueryString       = String

  type ErrorMsg          = String
  type FormattedNumber   = String

  // ---------------------------------------------------------------------------
  type One[T] =               T
  type Opt[T] =        Option[T]

  // TODO: t210125111144 - maybe an actual NonEmptyList such as cats'?
  type Pes[T] = Option[Seq   [T]]
  type Nes[T] =        Seq   [T]

  // ===========================================================================
  type CharsetSelector = misc.StandardCharsets.type => Charset

  // ---------------------------------------------------------------------------
  type RawRdbmsEntries = Map   [String /* field name */, Option[Any /* value */]]
  type RawRdbmsValues  = Vector[                         Option[Any /* value */]]
}

// ===========================================================================
