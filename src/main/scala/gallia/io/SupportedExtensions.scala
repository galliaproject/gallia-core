package gallia.io

import aptus.{Anything_, String_}
import enumeratum.{Enum, EnumEntry}

// ===========================================================================
sealed trait SupportedExtensions extends EnumEntry {
    // t210118103259 - proper handling
    @aptus.nonfinl def ztype: IoTypeZ = aptus.illegalState("TODO:210118103259")
    @aptus.nonfinl def utype: IoTypeU = aptus.illegalState("TODO:210118103259")
  }

  // ---------------------------------------------------------------------------
  trait CompressionExtensions extends SupportedExtensions { def compression: SupportedCompression }

  // ===========================================================================
  object SupportedExtensions extends Enum[SupportedExtensions] { val values = findValues

    def parseOpt(path: String): Option[SupportedExtensions] =
      path
        //TODO: loop over supported extensions rather
          .stripSuffix(".gz")
          .stripSuffix(".bz2")
          .stripSuffix(".zip")
        .splitBy(".")
        .in.noneIf(_.size == 1)
        .map(_.last.toLowerCase)
        .flatMap(withNameOption)

    // ---------------------------------------------------------------------------
    def parseLastOpt(path: String): Option[SupportedExtensions] =
      path
        .splitBy(".")
        .last
        .toLowerCase
        .thn(SupportedExtensions.withNameOption)

    // ===========================================================================
    case object gz    extends CompressionExtensions { def compression = SupportedCompression.Gzip  }
    case object bz2   extends CompressionExtensions { def compression = SupportedCompression.Bzip2 }
    case object zip   extends CompressionExtensions { def compression = SupportedCompression.Zip   }
    //TODO: t210118103219 - snappy, lzo, lz4, ZSTD

    //case object tgz   extends CompressionExtensions
    //case object `7z`   extends CompressionExtensions
    //case object tar   extends CompressionExtensions

    // ===========================================================================
    case object content extends SupportedExtensions { override def utype = IoTypeU.RawContent }

    case object lines   extends SupportedExtensions { override def ztype = IoTypeZ.RawLines }
    case object txt     extends SupportedExtensions { override def ztype = IoTypeZ.RawLines }

    // ---------------------------------------------------------------------------
    case object tsv  extends SupportedExtensions { override def ztype = IoTypeZ.Table }
    case object csv  extends SupportedExtensions { override def ztype = IoTypeZ.Table }

      case object tsvwh extends SupportedExtensions { override def ztype = IoTypeZ.Table }
      case object tsvnh extends SupportedExtensions { override def ztype = IoTypeZ.Table }

      case object csvwh extends SupportedExtensions { override def ztype = IoTypeZ.Table }
      case object csvnh extends SupportedExtensions { override def ztype = IoTypeZ.Table }

    // ---------------------------------------------------------------------------
    case object json  extends SupportedExtensions {
        override def utype = IoTypeU.CompactJsonObject
        override def ztype = IoTypeZ.JsonLines }

      case object jsono extends SupportedExtensions { override def utype = IoTypeU.CompactJsonObject } // non-standard
      case object jsona extends SupportedExtensions { override def ztype = IoTypeZ.JsonArray         } // non-standard

      case object jsonl extends SupportedExtensions { override def ztype = IoTypeZ.JsonLines }
      case object jsons extends SupportedExtensions { override def ztype = IoTypeZ.JsonLines } // non-standard

    // ---------------------------------------------------------------------------
    case object xml  extends SupportedExtensions
    case object html extends SupportedExtensions
    case object rdf  extends SupportedExtensions
    case object owl  extends SupportedExtensions

    // ---------------------------------------------------------------------------
    case object yml  extends SupportedExtensions
    case object yaml extends SupportedExtensions
    //TODO: conf for HOCON?

    case object xls   extends SupportedExtensions
    case object xlsx  extends SupportedExtensions
    case object rtf   extends SupportedExtensions
    case object docx  extends SupportedExtensions

    case object pdf   extends SupportedExtensions

    case object log   extends SupportedExtensions // will just read lines as _line

    // ---------------------------------------------------------------------------
    // val lines   = "lines"
    // val content = "content"

    //  // ---------------------------------------------------------------------------
    //  val  settings   = "settings"
    //  val  cfg        = "cfg"
    //  val  conf       = "conf"
    //  val  ini        = "ini"
    //  val  properties = "properties"
    //
    // ---------------------------------------------------------------------------
    //val  kvp  = "kvp" // key-value-pairs
    //val  sbk  = "sbk" // sorted-by-key
    //val  gbk  = "gbk" // grouped-by-key
    //val  usl  = "usl" // unsorted list
    //val   sl  =  "sl" //   sorted list
    //val list  = "list"
    //val sortu = "sortu" // uniquely sorted
    //val  part = "part"
    //val  par  = "par"

  }

// ===========================================================================
