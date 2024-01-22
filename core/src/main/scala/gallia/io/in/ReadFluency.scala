package gallia
package io
package in

// ===========================================================================
// TODO: t201222094823 - improve boilerplate throughout, macro maybe (we know the copy() methods are available)?

// ---------------------------------------------------------------------------
class RawContentFluency(val conf: RawContentConf)
      extends FluencyBase[RawContentFluency, RawContentConf](new RawContentFluency(_))

      with    HasCharsetFluency    [RawContentFluency]
      with    HasCompressionFluency[RawContentFluency]

      with EndReadUFluency {
    val self = this

    // ---------------------------------------------------------------------------
    // boilerplate:
    override def charset    (value: SupportedCharset)     = conf.copy(urlLike = conf.urlLike.update(value))
    override def compression(value: SupportedCompression) = conf.copy(urlLike = conf.urlLike.update(value))
  }

  // ---------------------------------------------------------------------------
  class RawLinesFluency(val conf: RawLinesConf)
      extends FluencyBase[RawLinesFluency, RawLinesConf](new RawLinesFluency(_))

      with    HasCharsetFluency    [RawLinesFluency]
      with    HasCompressionFluency[RawLinesFluency]

      with    CanSetMemoryModeFluency[RawLinesFluency]

      with EndReadZFluency {
    val self = this

    def forceInMemory: RawLinesFluency = this // FIXME

    // ---------------------------------------------------------------------------
    // boilerplate:
    override def charset    (value: SupportedCharset)     = conf.copy(urlLike = conf.urlLike.update(value))
    override def compression(value: SupportedCompression) = conf.copy(urlLike = conf.urlLike.update(value))

    // ---------------------------------------------------------------------------
    override def inMemoryMode = conf.copy(inMemoryMode  = true) // default (see gallia.io.in package)
    override def iteratorMode = conf.copy(inMemoryMode  = false)
  }

// ===========================================================================
class JsonObjectStringFluency(val conf: JsonObjectStringConf)
      extends FluencyBase[JsonObjectStringFluency, JsonObjectStringConf](new JsonObjectStringFluency(_))

      with    CanSetSchemaFluency[JsonObjectStringFluency]

      with    EndReadUFluency {
    val self = this

    // ---------------------------------------------------------------------------
    // boilerplate:
    override def explicitSchema(c: Cls) = conf.copy(schemaProvider = ExplicitSchema(c))
    override def inferSchema            = conf.copy(schemaProvider = InferSchema) }

  // ---------------------------------------------------------------------------
  class JsonArrayStringFluency(val conf: JsonArrayStringConf)
      extends FluencyBase[JsonArrayStringFluency, JsonArrayStringConf](new JsonArrayStringFluency(_))

      with    CanSetSchemaFluency[JsonArrayStringFluency]

      with    EndReadZFluency {
    val self = this

    // ---------------------------------------------------------------------------
    // boilerplate:
    override def explicitSchema(c: Cls) = conf.copy(schemaProvider = ExplicitSchema(c))
    override def inferSchema            = conf.copy(schemaProvider = InferSchema)
  }

// ===========================================================================
class JdbcFluency(val conf: JdbcConf)
      extends FluencyBase[JdbcFluency, JdbcConf](new JdbcFluency(_))
      with    EndReadZFluency {
    val self = this

    // ---------------------------------------------------------------------------
    def table(table: String): JdbcFluency = conf.copy(queryingOpt = Some(ReadQuerying.All(table)))
    def query(query: String): JdbcFluency = conf.copy(queryingOpt = Some(ReadQuerying.Query(query)))

    // ===========================================================================
    // boilerplate:
    private[io] def querying(value: Option[ReadQuerying]): JdbcFluency = conf.copy(queryingOpt = value)
  }

  // ===========================================================================
  class MongodbFluency(val conf: MongodbConf)
      extends FluencyBase[MongodbFluency, MongodbConf](new MongodbFluency(_))
      with    EndReadZFluency {
    val self = this

    // ---------------------------------------------------------------------------
    def collection(collection: String): MongodbFluency = conf.copy(queryingOpt = Some(ReadQuerying.All(collection)))
    def query     (query: String)     : MongodbFluency = conf.copy(queryingOpt = Some(ReadQuerying.Query(query)))

    // ---------------------------------------------------------------------------
    // boilerplate:
    private[io] def querying(value: Option[ReadQuerying]): MongodbFluency = conf.copy(queryingOpt = value)
  }

  // ===========================================================================
  class JsonObjectFileFluency(val conf: JsonObjectFileConf)
      extends FluencyBase[JsonObjectFileFluency, JsonObjectFileConf](new JsonObjectFileFluency(_))

      with    HasCharsetFluency    [JsonObjectFileFluency]
      with    HasCompressionFluency[JsonObjectFileFluency]

      with    CanSetSchemaFluency[JsonObjectFileFluency]

      with    CanProjectFluency[JsonObjectFileFluency]

      with    EndReadUFluency {
    val self = this

    // ===========================================================================
    // boilerplate:
    override def charset    (value: SupportedCharset)     = conf.copy(urlLike = conf.urlLike.update(value))
    override def compression(value: SupportedCompression) = conf.copy(urlLike = conf.urlLike.update(value))

    // ---------------------------------------------------------------------------
    override def explicitSchema(c: Cls) = conf.copy(schemaProvider = ExplicitSchema(c))
    override def inferSchema            = conf.copy(schemaProvider = InferSchema)

    // ---------------------------------------------------------------------------
    override private[io] def project(projectionOpt: Option[ReadProjection]) = conf.copy(projectionOpt = projectionOpt)
  }

  // ===========================================================================
  class JsonArrayFileFluency(val conf: JsonArrayFileConf)
      extends FluencyBase[JsonArrayFileFluency, JsonArrayFileConf](new JsonArrayFileFluency(_))

      with    HasCharsetFluency    [JsonArrayFileFluency]
      with    HasCompressionFluency[JsonArrayFileFluency]

      with    CanSetMemoryModeFluency[JsonArrayFileFluency]

      with    CanSetSchemaFluency[JsonArrayFileFluency]

      with    CanProjectFluency[JsonArrayFileFluency]

      with    EndReadZFluency {
    val self = this

    // ===========================================================================
    // boilerplate:

    override def charset    (value: SupportedCharset)     = conf.copy(urlLike = conf.urlLike.update(value))
    override def compression(value: SupportedCompression) = conf.copy(urlLike = conf.urlLike.update(value))

    // ---------------------------------------------------------------------------
    override def inMemoryMode = conf.copy(inMemoryMode  = true) // default (see gallia.io.in package)
    override def iteratorMode = conf.copy(inMemoryMode  = false)

    // ---------------------------------------------------------------------------
    override def explicitSchema(c: Cls) = conf.copy(schemaProvider = ExplicitSchema(c))
    override def inferSchema            = conf.copy(schemaProvider = InferSchema)

    // ---------------------------------------------------------------------------
    override private[io] def project(projectionOpt: Option[ReadProjection]) = conf.copy(projectionOpt = projectionOpt)
  }

// ===========================================================================
class JsonLinesFileFluency(val conf: JsonLinesFileConf)
    extends FluencyBase[JsonLinesFileFluency, JsonLinesFileConf](new JsonLinesFileFluency(_))

    with    HasCharsetFluency    [JsonLinesFileFluency]
    with    HasCompressionFluency[JsonLinesFileFluency]

    with    CanSetMemoryModeFluency[JsonLinesFileFluency]

    with    CanSetSchemaFluency[JsonLinesFileFluency]

    with    CanProjectFluency[JsonLinesFileFluency]

    with    EndReadZFluency {
  val self = this

  // ---------------------------------------------------------------------------
  def nullsAs(value: Any): JsonLinesFileFluency = ??? // TODO: t201216174813 - at least allow replacing JSON nulls

  //TODO: t201216174927 - provide gson hook?

  // ===========================================================================
  // boilerplate:

  override def charset    (value: SupportedCharset)     = conf.copy(urlLike = conf.urlLike.update(value))
  override def compression(value: SupportedCompression) = conf.copy(urlLike = conf.urlLike.update(value))

  // ---------------------------------------------------------------------------
  override def inMemoryMode = conf.copy(inMemoryMode  = true) // default (see gallia.io.in package)
  override def iteratorMode = conf.copy(inMemoryMode  = false)

  // ---------------------------------------------------------------------------
  override def explicitSchema(c: Cls) = conf.copy(schemaProvider = ExplicitSchema(c))
  override def inferSchema            = conf.copy(schemaProvider = InferSchema)

  // ---------------------------------------------------------------------------
  override private[io] def project(projectionOpt: Option[ReadProjection]) = conf.copy(projectionOpt = projectionOpt)
}

// ===========================================================================
class TableFluency(val conf: TableConf)
    extends FluencyBase[TableFluency, TableConf](new TableFluency(_))

    with    HasCharsetFluency    [TableFluency]
    with    HasCompressionFluency[TableFluency]

    with    CanSetMemoryModeFluency[TableFluency]

    with    CanSetSchemaFluency[TableFluency]
    with    CanProjectFluency[TableFluency]

    with    EndReadZFluency { import TableSchemaProvider._
  val self = this

  // ===========================================================================
  def noInferring: TableFluency = conf.copy(schemaProvider = NoInferring)
  def stringsOnly: TableFluency = conf.copy(schemaProvider = StringsOnly)

  // ---------------------------------------------------------------------------
  def useIndexKeys: TableFluency = conf.copy(indexKeysMode = true)

  // ---------------------------------------------------------------------------
  def withHeader: TableFluency = conf.updateFormatConf(_.copy(explicitHasHeaderOpt = Some(true)))
  def noHeader  : TableFluency = conf.updateFormatConf(_.copy(explicitHasHeaderOpt = Some(false)))

  // ---------------------------------------------------------------------------
  def missingHeader(value1: KeyW, more: KeyW*): TableFluency = noHeader.conf.copy(schemaProvider = ExplicitKeys(KeyWz._to(value1, more).keyz))
  def missingHeader(values: KeyWz)            : TableFluency = noHeader.conf.copy(schemaProvider = ExplicitKeys(values.keyz))

  // ---------------------------------------------------------------------------
  def nullValue      (value: String): TableFluency = conf.updateCellConf(_.copy(nullValues      = Seq(value)))
  def arraySeparator (value: String): TableFluency = conf.updateCellConf(_.copy(arraySeparators = Seq(value)))

  def nullValues     (value1: String, more: String*): TableFluency = conf.updateCellConf(_.copy(nullValues      = value1 +: more))
  def arraySeparators(value1: String, more: String*): TableFluency = conf.updateCellConf(_.copy(arraySeparators = value1 +: more))

  def keepEmptyStrings: TableFluency = conf.updateCellConf(_.copy(nullValues      = Seq()))
  def noArraySeparator: TableFluency = conf.updateCellConf(_.copy(arraySeparators = Seq()))

  def cellConf(value: CellConf)    : TableFluency = conf.copy(cellConf = value)

  // ===========================================================================
  // boilerplate:

  override def explicitSchema(c: Cls) = conf.copy(schemaProvider = TableSchemaProvider.ExplicitSchema(c))
  override def inferSchema            = conf.copy(schemaProvider = TableSchemaProvider.InferSchema)

  // ---------------------------------------------------------------------------
  override def inMemoryMode = conf.copy(inMemoryMode  = true) // default (see gallia.io.in package)
  override def iteratorMode = conf.copy(inMemoryMode  = false)

  // ---------------------------------------------------------------------------
  override def charset    (value: SupportedCharset)     = conf.copy(urlLike = conf.urlLike.update(value))
  override def compression(value: SupportedCompression) = conf.copy(urlLike = conf.urlLike.update(value))

  // ---------------------------------------------------------------------------
  override private[io] def project(projectionOpt: Option[ReadProjection]) = conf.copy(projectionOpt = projectionOpt)
}

// ===========================================================================
