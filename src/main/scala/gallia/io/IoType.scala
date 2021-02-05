package gallia.io

import aptus.Anything_
import gallia.io.in._
import gallia.io.out._

// ===========================================================================
sealed trait IoTypeU {
    def defaultRead  : (StartReadUFluency, InputUStringDrivenConf) => EndReadUFluency
    def defaultFormat: gallia.Obj => String

    // ---------------------------------------------------------------------------
    // TODO: t210124100009 - rename "NonTable"
    final def urlLikeConf   (path: String): UrlLikeNonTableConfU    = UrlLikeNonTableConfU(this, path)
    final def urlLikeFluency(path: String): UrlLikeNonTableFluencyU = new UrlLikeNonTableFluencyU(urlLikeConf(path))
  }

  // ===========================================================================
  object IoTypeU { // TODO: t210118103012 - proper handling

    /** flawed as it may be, it is still the most common/convenient at the moment */
    val Default = JsonObject

    // ---------------------------------------------------------------------------
    def parsePathOpt(path: String): Option[IoTypeU] =
      SupportedExtensions
        .parseOpt(path)
        .map(_.utype)

    // ===========================================================================
    case object RawContent extends IoTypeU {
      def defaultRead   = (start, conf) => start.content
      def defaultFormat = _.text(gallia._content) }

    // ===========================================================================
    case object DirectJsonObject extends IoTypeU {
        def defaultRead = (start, conf) => start.jsonObjectString.schemaProvider(conf.schemaProvider)
        def defaultFormat = _.formatCompactJson }

      // ---------------------------------------------------------------------------
      case object JsonObject extends IoTypeU {
        def defaultRead   = (start, conf) => start.jsonObjectFile.schemaProvider(conf.schemaProvider)
        def defaultFormat = _.formatCompactJson }

  }

// ===========================================================================
sealed trait IoTypeZ { // TODO: t210118103012 - proper handling
    def defaultRead  : (StartReadZFluency, InputZStringDrivenConf) => EndReadZFluency
    def defaultFormat: gallia.Objs => Iterator[String]

    def outputConf(path: String): OutputConfZ
  }

  // ===========================================================================
  sealed trait UrlLikeIoTypeZ extends IoTypeZ {
    // TODO: t210124100009 - rename "NonTable"
    final def urlLikeConf   (path: String): UrlLikeNonTableConfZ    = UrlLikeNonTableConfZ(this, path)
    final def urlLikeFluency(path: String): UrlLikeNonTableFluencyZ = new UrlLikeNonTableFluencyZ(urlLikeConf(path))

    final def outputConf    (path: String): OutputConfZ        = urlLikeConf(path)
  }

  // ===========================================================================
  object IoTypeZ {

    /** flawed as it may be, it is still the most common/convenient at the moment */
    val Default = JsonLines

    // ---------------------------------------------------------------------------
    def parsePathOpt(path: String): Option[IoTypeZ] =
      SupportedExtensions
        .parseOpt(path)
        .map(_.ztype)

    // ===========================================================================
    case object RawLines extends UrlLikeIoTypeZ {
      def defaultRead   = (start, conf) => start.lines.thnIf(!conf.inMemoryMode)(_.iteratorMode)
      def defaultFormat = _.consume.map(_.text(gallia._line)) }

    // ===========================================================================
    case object DirectJsonArray  extends IoTypeZ {
        def defaultRead = (start, conf) => start.jsonArrayString.schemaProvider(conf.schemaProvider)
        def defaultFormat            = gallia.illegal("TODO:210118103012")
        def outputConf(path: String) = gallia.illegal("TODO:210118103013")
      }

      // ---------------------------------------------------------------------------
      case object JsonLines extends UrlLikeIoTypeZ {
        def defaultRead   = (start, conf) => start.jsonLinesFile.thnIf(!conf.inMemoryMode)(_.iteratorMode).schemaProvider(conf.schemaProvider).project(conf.projectionOpt)
        def defaultFormat = _.consume.map(_.formatCompactJson) }

      // ---------------------------------------------------------------------------
      case object JsonArray extends UrlLikeIoTypeZ {
        def defaultRead   = (start, conf) => start.jsonArrayFile.thnIf(!conf.inMemoryMode)(_.iteratorMode).schemaProvider(conf.schemaProvider).project(conf.projectionOpt)
        def defaultFormat = _.naiveFormatArrayLines }

    // ===========================================================================
    case object Table extends IoTypeZ {
      def defaultRead   = (start, conf) => start.table.thnIf(!conf.inMemoryMode)(_.iteratorMode).schemaProvider(conf.schemaProvider).project(conf.projectionOpt)
      def defaultFormat = gallia.illegal("TODO:210118103012")
      def outputConf(path: String) = UrlLikeTableConf(path, UrlLike.Default, FormatConf.Default)
    }

    // ===========================================================================
    case object Jdbc extends IoTypeZ {
      def defaultRead   = gallia.illegal("TODO:210118103012")
      def defaultFormat = gallia.illegal("TODO:210118103012")
      def outputConf(path: String) = gallia.illegal("TODO:210118103012")
    }

    // ---------------------------------------------------------------------------
    case object MongoDb2 extends IoTypeZ {
      def defaultRead   = gallia.illegal("TODO:210118103012")
      def defaultFormat = gallia.illegal("TODO:210118103012")
      def outputConf(path: String) = gallia.illegal("TODO:210118103012")
    }
  }

// ===========================================================================
