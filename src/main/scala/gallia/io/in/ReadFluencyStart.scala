package gallia
package io
package in

// ===========================================================================
class StartReadUFluency(val inputString: InputString) // note: no querying
    extends FluencyBase[InputUStringDrivenFluency, InputUStringDrivenConf](new InputUStringDrivenFluency(_))

    with    HasCharsetFluency    [InputUStringDrivenFluency]
    with    HasCompressionFluency[InputUStringDrivenFluency]

    with    CanSetSchemaFluency[InputUStringDrivenFluency]

    with    CanProjectFluency[InputUStringDrivenFluency]

    with    EndReadUFluency {

  val self = inputDriven
  val conf = self.conf

  // ---------------------------------------------------------------------------
  private[gallia] def inputDriven: InputUStringDrivenFluency =
    new InputUStringDrivenFluency(InputUStringDrivenConf(inputString))

  // ===========================================================================
  def content: RawContentFluency = new RawContentFluency(RawContentConf(inputString))

  // ---------------------------------------------------------------------------
  def jsonl = jsonObjectFile

    def jsonObjectFile   = new JsonObjectFileFluency  (JsonObjectFileConf  (inputString))
    def jsonArrayFile    = new JsonArrayFileFluency   (JsonArrayFileConf   (inputString))
    def jsonObjectString = new JsonObjectStringFluency(JsonObjectStringConf(inputString))

  // ===========================================================================
  // TODO: @delegate macro?

    override def charset    (value: SupportedCharset)     = inputDriven.charset    (value)
    override def compression(value: SupportedCompression) = inputDriven.compression(value)

    // ---------------------------------------------------------------------------
    override private[io] def project(projectionOpt: Option[ReadProjection]) = inputDriven.project(projectionOpt)

    // ---------------------------------------------------------------------------
    override def explicitSchema(c: Cls) = inputDriven.explicitSchema(c)
    override def inferSchema            = inputDriven.inferSchema
}

// ===========================================================================
class StartReadZFluency(private[io] val inputString: InputString)
    extends FluencyBase[InputZStringDrivenFluency, InputZStringDrivenConf](new InputZStringDrivenFluency(_))

    with    HasCharsetFluency    [InputZStringDrivenFluency]
    with    HasCompressionFluency[InputZStringDrivenFluency]

    with    CanSetMemoryModeFluency[InputZStringDrivenFluency]

    with    CanSetSchemaFluency[InputZStringDrivenFluency]

    with    CanProjectFluency[InputZStringDrivenFluency]
    with    CanQueryFluency  [InputZStringDrivenFluency]

    with    EndReadZFluency {
  private[gallia] val _inputString = inputString // FIXME: hack for mongodb...

  val self = inputDriven
  val conf = self.conf

  // ---------------------------------------------------------------------------
  private[gallia] def inputDriven: InputZStringDrivenFluency =
    new InputZStringDrivenFluency(InputZStringDrivenConf(inputString))

  // ===========================================================================
  def lines: RawLinesFluency = new RawLinesFluency(RawLinesConf(inputString))

  // ---------------------------------------------------------------------------
  def table(sep: Char): TableFluency = new TableFluency(TableConf(inputString).updateFormatConf(_.copy(explicitSeparatorOpt = Some(sep))))
  def table           : TableFluency = new TableFluency(TableConf(inputString))

    def tsv: TableFluency = table('\t')
    def csv: TableFluency = table(',')

  // ---------------------------------------------------------------------------
  def jsonl: JsonLinesFileFluency = jsonLinesFile

    def jsonLinesFile  : JsonLinesFileFluency   = new JsonLinesFileFluency  (JsonLinesFileConf(inputString))
    def jsonArrayFile  : JsonArrayFileFluency   = new JsonArrayFileFluency  (JsonArrayFileConf(inputString))

    def jsonArrayString: JsonArrayStringFluency = new JsonArrayStringFluency(JsonArrayStringConf(inputString))

  // ---------------------------------------------------------------------------
              def  jdbc   : JdbcFluency    = new JdbcFluency   (JdbcConf   (inputString))
  private[io] def _mongodb: MongodbFluency = new MongodbFluency(MongodbConf(inputString))

  // ===========================================================================
  // TODO: @delegate macro?

    override def charset    (value: SupportedCharset)     = inputDriven.charset    (value)
    override def compression(value: SupportedCompression) = inputDriven.compression(value)

    // ---------------------------------------------------------------------------
    override def iteratorMode = conf.copy(inMemoryMode  = false)

    // ---------------------------------------------------------------------------
    override def explicitSchema(c: Cls) = inputDriven.explicitSchema(c)
    override def inferSchema            = inputDriven.inferSchema

    // ---------------------------------------------------------------------------
    override private[io] def project(projectionOpt: Option[ReadProjection]) = inputDriven.project(projectionOpt)

    // ---------------------------------------------------------------------------
    override protected[io] def querying(value: ReadQuerying) = inputDriven.querying(value)
}

// ===========================================================================
