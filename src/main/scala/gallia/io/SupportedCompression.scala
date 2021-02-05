package gallia.io

import java.io._

// ===========================================================================
sealed trait SupportedCompression {
    // see org.apache.commons.compress.compressors.CompressorStreamFactory
    protected def compressorNameOpt: Option[String]

    // ---------------------------------------------------------------------------
    def outputStreamWriter(urlString: String): OutputStreamWriter =
        new OutputStreamWriter(
          compressorNameOpt
            .map      (compressorOutputStream(urlString))
            .getOrElse(plainFileOutputStream (urlString)) )

      // ---------------------------------------------------------------------------
      private def compressorOutputStream(urlString: String)(compressorName: String): OutputStream =
          SupportedCompression
            .CompressorStreamFactory
            .createCompressorOutputStream(
                compressorName, // TODO: does it sync flush for gz?
                plainFileOutputStream(urlString))

        // ---------------------------------------------------------------------------
        private def plainFileOutputStream(urlString: String): OutputStream =
          new FileOutputStream(
              urlString,
              /* append = */ false)
  }

  // ===========================================================================
  object SupportedCompression {
    private lazy val CompressorStreamFactory = new org.apache.commons.compress.compressors.CompressorStreamFactory()

    // ---------------------------------------------------------------------------
    type Selector = SupportedCompression.type => SupportedCompression

    // ---------------------------------------------------------------------------
    def parse(path: String): SupportedCompression =
      SupportedExtensions
        .parseLastOpt(path)
        .flatMap {
          case x: CompressionExtensions => Some(x)
          case x                        => None }
        .map(_.compression)
        .getOrElse(NoCompression)

    // ---------------------------------------------------------------------------
    case object NoCompression extends SupportedCompression { def compressorNameOpt = None }

    case object Gzip  extends SupportedCompression { def compressorNameOpt = Some("gz") }
    case object Bzip2 extends SupportedCompression { def compressorNameOpt = Some("bzip2") }
    case object Zip   extends SupportedCompression { def compressorNameOpt = ??? /* FIXME: disallow as output */ }
    // TODO: t210118103219 - snappy, lzo, lz4, ZSTD, ...
  }

// ===========================================================================
