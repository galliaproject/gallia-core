package gallia.io.in

import gallia._
import gallia.actions.in._

// ===========================================================================
sealed trait InputConfU { def actionU: ActionIU }
sealed trait InputConfZ { def actionZ: ActionIZ }

// ===========================================================================
case class JsonObjectStringConf(inputString: InputString, schemaProvider: OtherSchemaProvider = InferSchema) extends InputConfU {
    def actionU: ActionIU = JsonObjectString(inputString, schemaProvider) }

  // ---------------------------------------------------------------------------
  case class JsonArrayStringConf(inputString: InputString, schemaProvider: OtherSchemaProvider = InferSchema) extends InputConfZ {
    def actionZ: ActionIZ = JsonArrayString(inputString, schemaProvider) }

// ===========================================================================
case class JsonObjectFileConf(
        inputString   : InputString,

        urlLike       : UrlLike             = UrlLike.Default,
        schemaProvider: OtherSchemaProvider = InferSchema,
        projectionOpt : Option[ReadProjection] = None)
      extends InputConfU {

    def actionU: ActionIU =
      JsonObjectFileInputU(
        InputUrlLike(
          inputString,
          urlLike.resolveCharset,
          urlLike.resolveCompression(inputString)),
        schemaProvider,
        projectionOpt)
  }

  // ---------------------------------------------------------------------------
  case class JsonArrayFileConf(
        inputString   : InputString,

        urlLike       : UrlLike                = UrlLike.Default,
        inMemoryMode  : Boolean                = true,
        schemaProvider: OtherSchemaProvider    = InferSchema,
        projectionOpt : Option[ReadProjection] = None)
      extends InputConfZ {
    def actionZ: ActionIZ =
      JsonArrayFileInputZ(
          InputUrlLike(
            inputString,
            urlLike.resolveCharset,
            urlLike.resolveCompression(inputString)),
          inMemoryMode,
          schemaProvider,
          projectionOpt)
  }

// ===========================================================================
case class InputUStringDrivenConf(
        inputString  : InputString,
        urlLike      : UrlLike = UrlLike.Default,

        schemaProvider: OtherSchemaProvider    = InferSchema,

        projectionOpt: Option[ReadProjection] = None,
        queryingOpt  : Option[ReadQuerying]   = None)
      extends InputConfU with InputUStringDrivenConfHelper {
    def actionU: ActionIU = _action
  }

  // ===========================================================================
  case class InputZStringDrivenConf(
        inputString   : InputString,

        urlLike       : UrlLike                = UrlLike.Default,

        inMemoryMode  : Boolean                = true,
        schemaProvider: OtherSchemaProvider    = InferSchema,
        projectionOpt : Option[ReadProjection] = None,
        queryingOpt   : Option[ReadQuerying]   = None)
      extends InputConfZ with InputZStringDrivenConfHelper {
    def actionZ: ActionIZ = _action
  }

// ===========================================================================
case class RawContentConf(inputString: InputString, urlLike: UrlLike = UrlLike.Default) extends InputConfU {
  def actionU: ActionIU =
    RawContentU(InputUrlLike(
      inputString,
      urlLike.resolveCharset,
      urlLike.resolveCompression(inputString)))
}

// ===========================================================================
case class RawLinesConf(
      inputString : InputString,
      urlLike     : UrlLike = UrlLike.Default,
      inMemoryMode: Boolean = true)
    extends InputConfZ {
  def actionZ: ActionIZ =
    RawLinesZ(
      InputUrlLike(
        inputString,
        urlLike.resolveCharset,
        urlLike.resolveCompression(inputString)),
      inMemoryMode)
}

// ===========================================================================
// TODO: t210114144622 - allow iterator mode for both JDBC and mongodb
case class JdbcConf(
        inputString   : InputString,
        queryingOpt   : Option[ReadQuerying] = None)
      extends InputConfZ {
    def actionZ: ActionIZ = JdbcInputZ(inputString, queryingOpt) }

  // ---------------------------------------------------------------------------
  case class MongodbConf(
        inputString   : InputString,
        schemaProvider: OtherSchemaProvider  = InferSchema,
        queryingOpt   : Option[ReadQuerying] = None)
      extends InputConfZ {
    def actionZ: ActionIZ = MongodbInputZ(inputString, schemaProvider, queryingOpt)
  }

// ===========================================================================
case class JsonLinesFileConf(
      inputString   : InputString,
      urlLike       : UrlLike                = UrlLike.Default,

      inMemoryMode  : Boolean                = true,
      schemaProvider: OtherSchemaProvider    = InferSchema,
      projectionOpt : Option[ReadProjection] = None,
      )
    extends InputConfZ {
  def actionZ: ActionIZ =
    JsonLinesFileInputZ(
        InputUrlLike(
          inputString,
          urlLike.resolveCharset,
          urlLike.resolveCompression(inputString)),
        inMemoryMode,
        schemaProvider,
        projectionOpt)
}

// ===========================================================================
case class TableConf(
      urlString: UrlString,

      // ---------------------------------------------------------------------------
      // linesPreprocessing: Option[LinesPreprocessing] = None,
      // valuePreprocessing: Option[Any => Option[Any /* 1 or Nes */]] = None,

      // ---------------------------------------------------------------------------
      urlLike             : UrlLike                = UrlLike.Default,

      formatConf          : FormatConf             = FormatConf.Default,
      cellConf            : CellConf               = CellConf.Default,

      inMemoryMode        : Boolean                = true,
      indexKeysMode       : Boolean                = false,

      schemaProvider      : TableSchemaProvider    = TableSchemaProvider.InferSchema,
      projectionOpt       : Option[ReadProjection] = None,
    ) extends InputConfZ {

  def updateFormatConf(f: FormatConf => FormatConf) = copy(formatConf = f(formatConf))
  def updateCellConf  (f: CellConf   => CellConf  ) = copy(cellConf   = f(cellConf))

  // ---------------------------------------------------------------------------
  def actionZ: ActionIZ =
    TableInputZ(
        InputUrlLike(
          inputString    = urlString,
          linesPreprocessing = None,
          compression    = urlLike.resolveCompression(urlString),
          charset        = urlLike.resolveCharset),

        formatConf,
        cellConf,

        inMemoryMode,
        indexKeysMode,

        schemaProvider,
        projectionOpt)
}

// ===========================================================================
