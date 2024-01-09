package gallia

// ===========================================================================
package object io extends _io

  // ===========================================================================
  trait _io {
    type UrlString = aptus.UrlString

    // ---------------------------------------------------------------------------
    val DefaultFieldSeparator: FieldSeparator = '\t' // because tabs less common than commas in natural language (+cut friendliness)
    val DefaultArraySeparator: String = "," // TODO: t201229101207 - or default to pipe (better practice: tabs and pipes are less common in natural language)..?
    val DefaultNullValue     : String = ""

    // ---------------------------------------------------------------------------
    type FieldSeparator = Char // TODO: t201229151510 - actually allow more than one char as sep? use case?
  }

  // ===========================================================================
  trait __io {
    type UrlLike = gallia.io.UrlLike
    val  UrlLike = gallia.io.UrlLike

    // ---------------------------------------------------------------------------
    type FormatConf = gallia.io.FormatConf
    val  FormatConf = gallia.io.FormatConf

    type CellConf = gallia.io.CellConf
    val  CellConf = gallia.io.CellConf

    // ===========================================================================
    type SupportedUriScheme = gallia.io.SupportedUriScheme
    val  SupportedUriScheme = gallia.io.SupportedUriScheme

    type IoTypeU = gallia.io.IoTypeU
    val  IoTypeU = gallia.io.IoTypeU

    type IoTypeZ = gallia.io.IoTypeZ
    val  IoTypeZ = gallia.io.IoTypeZ

    type SupportedCharset = gallia.io.SupportedCharset
    val  SupportedCharset = gallia.io.SupportedCharset

    type SupportedCompression = gallia.io.SupportedCompression
    val  SupportedCompression = gallia.io.SupportedCompression
  }

// ===========================================================================
