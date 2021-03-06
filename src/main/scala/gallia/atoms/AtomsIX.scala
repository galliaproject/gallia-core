package gallia.atoms

import aptus.{Anything_, String_}
import aptus.QueryString

import gallia._
import gallia.io._
import gallia.io.in._
import gallia.actions.in.HasProjection
import gallia.data.json.JsonParsing
import gallia.data.json.JsonNumberTax
import gallia.data.ModifyTableData
import gallia.data.multiple.streamer.Streamer

// ===========================================================================
object AtomsIX {

  case class _InMemoryInputUa(value: AObj) extends AtomIU {
    def naive: Option[Obj] = Some(value.u) }

    // ---------------------------------------------------------------------------
    case class _InMemoryInputZa(value: AObjs) extends AtomIZ {
      def naive: Option[Objs] = Some(value.z) }

    // ---------------------------------------------------------------------------
    case class _InMemoryInputUb(value: BObj) extends AtomIU {
      def naive: Option[Obj] = Some(value.forceObj) }

    // ---------------------------------------------------------------------------
    case class _InMemoryInputZb(value: BObjs) extends AtomIZ {
      def naive: Option[Objs] = Some(value.forceObjs) }

    // ---------------------------------------------------------------------------
    case class _InMemoryInputV[T: WTT](value: T) extends AtomIV {
      def naive: Option[Vle] = Some(value) }

  // ===========================================================================
  case class _RawContentU(input: InputUrlLike) extends AtomIU {
      def naive: Option[Obj] = Some(input.content.thn(Obj.content)) }

    // ---------------------------------------------------------------------------
    case class _RawLinesZ(input: InputUrlLike, inMemoryMode: Boolean) extends AtomIZ {
      def naive: Option[Objs] = Some(input.streamLines(inMemoryMode).map(Obj.line).thn(Objs.build)) }

  // ===========================================================================
  trait HasCommonObj  extends AtomIU { def commonObj : Obj  }
  trait HasCommonObjs extends AtomIZ { def commonObjs: Objs }

  // ===========================================================================
  case class _JsonObjectString(inputString: InputString, schemaProvider: OtherSchemaProvider, c: Cls) extends HasCommonObj {
    def commonObj: Obj = JsonParsing.parseObject(inputString)
      def naive: Option[Obj] = Some(commonObj.thn(JsonNumberTax.payUp(c))) }

  // ---------------------------------------------------------------------------
  case class _JsonArrayString(inputString: InputString, schemaProvider: OtherSchemaProvider, c: Cls) extends HasCommonObjs {
    def commonObjs: Objs = JsonParsing.parseArray(inputString).thn(Objs.from)
      def naive: Option[Objs] = commonObjs.map(JsonNumberTax.payUp(c)).as.some }

  // ===========================================================================
  case class _JsonObjectFileInputU(input: InputUrlLike, c: Cls) extends HasCommonObj {
    def commonObj: Obj = input.content.thn(JsonParsing.parseObject)
      def naive: Option[Obj] = commonObj.thn(JsonNumberTax.payUp(c)).as.some
  }

  // ===========================================================================
  case class _JsonLinesFileInputZ(
          input        : InputUrlLike,
          inMemoryMode : Boolean,
          projectionOpt: Option[ReadProjection],
          schema       : Cls)
        extends HasCommonObjs with HasProjection {

      def commonObjs: Objs =
          input
            .streamLines(inMemoryMode)
            .filterNot(_.trim.isEmpty) //TODO or only if last one?
            .map(JsonParsing.parseObject)
            .thn(Objs.build)

      // ---------------------------------------------------------------------------
      def naive: Option[Objs] = Some(commonObjs.map(JsonNumberTax.payUp(schema)).thn(projectData(schema)) )
    }

    // ===========================================================================
    case class _JsonArrayFileInputZ(
          input        : InputUrlLike,
          inMemoryMode : Boolean,
          projectionOpt: Option[ReadProjection],
          schema       : Cls)
        extends HasCommonObjs with HasProjection {
      def commonObjs: Objs =
          input
            .streamLines(inMemoryMode)
.toList.mkString.thn(JsonParsing.parseArray) // TODO: t201221175254 - actually stream array...
            .thn(Objs.from)

      // ---------------------------------------------------------------------------
      def naive: Option[Objs] = Some(commonObjs.map(JsonNumberTax.payUp(schema)).thn(projectData(schema)) )
    }

  // ===========================================================================
  case class _JdbcInputZ(inputString: InputString, queryingOpt: Option[ReadQuerying] /* missing if URI-driven */) extends AtomIZ {
    import aptus.misc.Rdbms.generalize
    import aptus.ResultSet_

    // ---------------------------------------------------------------------------
    def naive: Option[Objs] = {
      val sqlQuery = queryOpt.get// TODO: t210114202848 - validate

      val (rs, cls) = aptus.misc.Rdbms(new java.net.URI(inputString)).query(sqlQuery)

      Streamer
        .fromIterator((rs.rawRdbmsEntries, cls))
        .map(generalize).map(gallia.obj)
        .thn(Objs.build)
        .as.some
    }

    // ===========================================================================
    private def queryOpt: Option[QueryString] =
      tmp.map {
         case ReadQuerying.All  (table) => s"SELECT * from ${table}" /* TODO: t210114145431 - safe quoting + injection */
         case ReadQuerying.Query(query) => query }

      // ---------------------------------------------------------------------------
      private def tmp: Option[ReadQuerying] =
          queryingOpt
        .orElse {
          extractTableNameOpt(inputString, "table").map(ReadQuerying.All) }

      // ===========================================================================
      private def extractTableNameOpt(inputString: String, param: String): Option[String] = // not standard...
        inputString // FIXME: t210115205609 - decoding
          .splitBy("&")
          .find(_.startsWith(s"${param}="))
          .map(_.stripPrefix(s"${param}="))

  }

  // ===========================================================================
  case class _MongodbInputZ(
          inputString   : InputString,
          schemaProvider: OtherSchemaProvider,
          queryingOpt   : Option[ReadQuerying] /* None if URI-driven (eg "mydb.mycoll") */,
          c: Cls)
      extends HasCommonObjs { import _MongodbInputZ._
      mongoDb()

      def naive: Option[Objs] = commonObjs.map(JsonNumberTax.payUp(c)).as.some // TODO: confirm need to pay tax here

      // ===========================================================================
      // t210114153517 - must use jongo+find until figure out
      //   https://stackoverflow.com/questions/35771369/mongo-java-driver-how-to-create-cursor-in-mongodb-by-cusor-id-returned-by-a-db
      def commonObjs: Objs = {
          mongoDb.disableLogs()
          val cmd = cmdOpt.get // TODO: t210114152901 - validate

          mongoDb
            .query(new java.net.URI(inputString), None)(cmd)
            .thn { case (iter, cls) =>
              Streamer.fromIterator((iter, cls)) }
            .map(gallia.obj)
            .thn(Objs.build)
        }

        // ===========================================================================
        private def cmdOpt =
          tmp.flatMap {
             case ReadQuerying.All  (collection) => mongoDb.allFrom(collection).as.some
             case ReadQuerying.Query(query)      => mongoDb.query(query) }

        // ---------------------------------------------------------------------------
        private def tmp: Option[ReadQuerying] =
            queryingOpt
          .orElse {
            mongoDb.uriCollectionOpt(inputString).map(ReadQuerying.All) }// TODO: t210115205723 - validate URI earlier
    }

    // ===========================================================================
    object _MongodbInputZ {
      // FIXME: t201223100652 - proper DI; look into macwire

      // ---------------------------------------------------------------------------
      var mongoDbOpt: Option[utils.MongoDb] = None // TODO: make sure can only set once

        // ---------------------------------------------------------------------------
        def mongoDb(): utils.MongoDb =
          mongoDbOpt match {
            case None        => illegal("requires gallia.mongodb.injectMongoDb") // TODO: t201223101425 prettify
            case Some(value) => value }
    }

  // ===========================================================================
  case class _Table(
      input         : InputUrlLike,

      cellConf      : CellConf,

      inMemoryMode  : Boolean,
      schemaProvider: TableSchemaProvider,
      projectionOpt : Option[ReadProjection],

      // ---------------------------------------------------------------------------
      // originally from FormatConf
      sep      : FieldSeparator,
      hasHeader: Boolean,

      // ---------------------------------------------------------------------------
      defaultSchema: Cls)
  extends AtomIZ with HasProjection {
    override def formatSuccinct1 = s"${className}(${input._inputString})"    

    // ---------------------------------------------------------------------------
    def naive: Option[Objs] =
      aobjs(
           c =                                    projectMeta(defaultSchema).assert(!_.hasNesting),
           z = stringObjs(defaultSchema.keys).thn(projectData(defaultSchema)) ) // see t210106120036 (project ealier)
        .thn { x =>
          schemaProvider match {
            case TableSchemaProvider.NoInferring => x.z // nothing to do
            case _                               => new ModifyTableData(cellConf).modify(x) }}
        .as.some

    // ---------------------------------------------------------------------------
    def stringObjs(keys: Seq[Key]): Objs =
        dataRows
          .thn(_.map(datum(cellConf.nullValues)(keys)))
          .thn(Objs.build)

      // ---------------------------------------------------------------------------
      /** 201215141231 - null values are not handled here (see 201231113658 rather) */
      private def datum(nullValues: Seq[String])(keys: Seq[Key])(values: Seq[String]): Obj =
        keys
           // TODO throw proper error if duplicates
          .zip(values)   // TODO: check same size + charset and so on
          .thn(Obj.fromIterable)

    // ===========================================================================
    private def dataRows: Streamer[List[String]] = // TODO: t210330110804 - separate each as their own atom
      input
        .streamLines(inMemoryMode)
        .thnIf(hasHeader)(_.drop(1)) // TODO: t210116110159 for n >= 1?
        .map(_.splitXsv(sep))
  }

}

// ===========================================================================
