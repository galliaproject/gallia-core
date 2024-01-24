package gallia
package atoms

import aptus.{Anything_, String_}
import aptus.aptmisc.Rdbms
import io._
import io.in._
import actions.in.HasProjection
import data.json
import data.TableToGalliaData
import streamer.Streamer

// ===========================================================================
object AtomsIX { import utils.JdbcDataUtils

  case   class _GenericInputU(datum: Obj) extends AtomIU { def naive: Option[Obj] = Some(datum) }
    case class _GenericInputZ(data: Objs) extends AtomIZ { def naive: Option[Objs] = Some(data) }

    // ---------------------------------------------------------------------------
    case class _GenericInputZb(data: DataRegenerationClosure[Obj]) extends AtomIZ {
      def naive: Option[Objs] = Some(InputUrlLike.streamer(inMemoryMode = false)(data).pipe(Objs.build)) }

  // ===========================================================================
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
      def naive: Option[Obj] = Some(input.content().pipe(Obj.content)) }

    // ---------------------------------------------------------------------------
    case class _RawLinesZ(input: InputUrlLike, inMemoryMode: Boolean) extends AtomIZ {
      def naive: Option[Objs] = Some(input.streamLines(inMemoryMode).map(Obj.line).pipe(Objs.build)) }

  // ===========================================================================
  trait HasCommonObj  extends AtomIU { def commonObj : Obj  }
  trait HasCommonObjs extends AtomIZ { def commonObjs: Objs }

  // ===========================================================================
  case class _JsonObjectString(inputString: InputString, ignored /* TODO: t211231112700 - investigate */: OtherSchemaProvider, c: Cls) extends HasCommonObj {
      def naive: Option[Obj] = Some(commonObj)

      // ---------------------------------------------------------------------------
      def commonObj: Obj = {
        val o = json.GsonParsing.parseObject(inputString)

        Option(c)/* TODO: see 211230183100 - may be null */
          .map { schema => o.pipe(json.GsonToGalliaData.convertRecursively(schema)) }
          .getOrElse(o) } }

    // ---------------------------------------------------------------------------  
    object _JsonObjectString {
      def toObj(c: Cls)(value: String): Obj = _JsonObjectString(value, null /* TODO */, c).commonObj }

  // ---------------------------------------------------------------------------
  case class _JsonArrayString(inputString: InputString, ignored /* TODO: t211231112700 - investigate */: OtherSchemaProvider, c: Cls) extends HasCommonObjs {
      def naive: Option[Objs] = Some(commonObjs)
        def commonObjs: Objs = {
          val z = json.GsonParsing.parseArray(inputString).pipe(Objs.from)

          Option(c)/* TODO: see 211230183100 - may be null  */
            .map { schema => z.map(json.GsonToGalliaData.convertRecursively(schema)) }
            .getOrElse(z) } }

    // ---------------------------------------------------------------------------  
    object _JsonArrayString {
      def toObjs(c: Cls)(value: String): Objs = _JsonArrayString(value, null /* TODO */, c).commonObjs
    }

  // ===========================================================================
  case class _JsonObjectFileInputU(
        input        : InputUrlLike,
        projectionOpt: Option[ReadProjection],
        protoSchema  : Cls, // pre-projection
        schema       : Cls)
      extends HasCommonObj
         with HasProjection {

    def commonObj: Obj =
      input
        .content()
        .pipe(json.GsonParsing.parseObject)

    def naive: Option[Obj] =
      commonObj
        .pipe(json.GsonToGalliaData.convertRecursively(protoSchema))
        .pipe(projectData(schema, _))
        .in.some }

  // ===========================================================================
  case class _JsonLinesFileInputZ(
          input        : InputUrlLike,
          inMemoryMode : Boolean,
          projectionOpt: Option[ReadProjection],
          protoSchema  : Cls, // pre-projection
          schema       : Cls)
        extends HasCommonObjs
           with HasProjection {

      def commonObjs: Objs =
          input
            .streamLines(inMemoryMode)
            .filterNot(_.trim.isEmpty) // a220930162941 - for better or worse, we ignore those (ideally we'd only ignore the last one)
            .map(json.GsonParsing.parseObject) // TODO: t230112130056 - detect if documents are pretty printed (and disallow)
            .pipe(Objs.build)

      // ---------------------------------------------------------------------------
      def naive: Option[Objs] = 
        commonObjs
          .map(json.GsonToGalliaData.convertRecursively(protoSchema))
          .pipe(projectData(schema))
          .in.some
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
.toList.mkString.pipe(json.GsonParsing.parseArray) // TODO: t201221175254 - actually stream array...
            .pipe(Objs.from)

      // ---------------------------------------------------------------------------
      def naive: Option[Objs] = 
        commonObjs
          .map(json.GsonToGalliaData.convertRecursively(schema))
          .pipe(projectData(schema))
          .in.some
    }

  // ===========================================================================
  case class _JdbcInputZ1(
         inputString: InputString,
         queryingOpt: Option[ReadQuerying] /* missing if URI-driven */,
         schemaOpt  : Option[Cls] /* None for inferring */)
        extends AtomIZ {

      def columns: Rdbms.Columns = {
        val sqlQuery = readQueryingOpt.map(_.query).get// TODO: t210114202848 - validate

        aptus.aptmisc
          .Rdbms (new java.net.URI(inputString))
          .columns(sqlQuery) }

      // ---------------------------------------------------------------------------
      def naive: Option[Objs] = {
        val uri = new java.net.URI(inputString)
        val sqlQuery = readQueryingOpt.map(_.query).get// TODO: t210114202848 - validate

        Objs
          .from {
            new DataRegenerationClosure[Obj] {
              def regenerate = () => JdbcDataUtils.jdbcData(uri)(schemaOpt)(sqlQuery) } }
          .in.some }

      // ---------------------------------------------------------------------------
      private def readQueryingOpt: Option[ReadQuerying] =
          queryingOpt
        .orElse {
          JdbcDataUtils
            .extractTableNameOpt(inputString, "table")
            .map(ReadQuerying.All) }
    }
    
    // ===========================================================================
    case class _JdbcInputZ2(
         connection: java.sql.Connection,
         querying  : ReadQuerying,
         schemaOpt : Option[Cls] /* None for inferring */)
        extends AtomIZ {

      def columns: Rdbms.Columns =
        aptus.aptmisc
          .Rdbms (connection)
          .columns(querying.query)

      // ---------------------------------------------------------------------------
      def naive: Option[Objs] =
        Objs
          .from {
            new DataRegenerationClosure[Obj] {
              def regenerate = () => JdbcDataUtils.jdbcData(connection)(schemaOpt)(querying.query) } }
          .in.some }

  // ===========================================================================
  case class _MongodbInputZ(
          inputString   : InputString,
          schemaProvider: OtherSchemaProvider,
          queryingOpt   : Option[ReadQuerying] /* None if URI-driven (eg "mydb.mycoll") */,
          c: Cls)
      extends HasCommonObjs { import _MongodbInputZ._
      mongoDb()

      def naive: Option[Objs] = 
        commonObjs
          .map(json.GsonToGalliaData.convertRecursively(c)) // TODO: confirm need to pay tax here?
          .in.some

      // ===========================================================================
      // t210114153517 - must use jongo+find until figure out
      //   https://stackoverflow.com/questions/35771369/mongo-java-driver-how-to-create-cursor-in-mongodb-by-cusor-id-returned-by-a-db
      def commonObjs: Objs = {
          mongoDb().disableLogs()
          val cmd = cmdOpt.get // TODO: t210114152901 - validate

          Objs.from {
            new data.DataRegenerationClosure[Obj] {
              def regenerate = () =>
                mongoDb().closeableQuery(new java.net.URI(inputString), None)(cmd).map(obj) } }
        }

        // ===========================================================================
        private def cmdOpt =
          tmp.flatMap {
             case ReadQuerying.All  (collection) => mongoDb().allFrom(collection).in.some
             case ReadQuerying.Query(query)      => mongoDb().query(query) }

        // ---------------------------------------------------------------------------
        private def tmp: Option[ReadQuerying] =
            queryingOpt
          .orElse {
            mongoDb().uriCollectionOpt(inputString).map(ReadQuerying.All) }// TODO: t210115205723 - validate URI earlier
    }

    // ===========================================================================
    object _MongodbInputZ {
      // FIXME: t201223100652 - proper DI; look into macwire

      // ---------------------------------------------------------------------------
      var mongoDbOpt: Option[utils.MongoDb] = None // TODO: make sure can only set once

        // ---------------------------------------------------------------------------
        def mongoDb(): utils.MongoDb =
          mongoDbOpt match {
            case None        => aptus.illegalState("requires gallia.mongodb.injectMongoDb") // TODO: t201223101425 prettify
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
    def naive: Option[Objs] = {
      val schema =                                     projectMeta(defaultSchema).assert(!_.hasNesting)
      val data   = stringObjs(defaultSchema.keys).pipe(projectData(defaultSchema)) // see t210106120036 (project ealier)

      Some(schemaProvider match {
        case TableSchemaProvider.NoInferring => data // nothing to do
        case _                               => data.map(TableToGalliaData.convert(cellConf)(schema)) }) }

    // ---------------------------------------------------------------------------
    def stringObjs(keys: Seq[Key]): Objs =
        dataRows
          .pipe(_.map(datum(keys)))
          .pipe(Objs.build)

      // ---------------------------------------------------------------------------
      /** 201215141231 - null values are not handled here (see 201231113658 rather) */
      private def datum(keys: Seq[Key])(values: Seq[String]): Obj =
        keys
           // TODO throw proper error if duplicates
          .zip(values)   // TODO: check same size + charset and so on
          .pipe(Obj.fromIterable)

    // ===========================================================================
    private def dataRows: Streamer[List[String]] = // TODO: t210330110804 - separate each as their own atom
      input
        .streamLines(inMemoryMode)
        .pipeIf(hasHeader)(_.drop(1)) // TODO: t210116110159 for n >= 1?
        .filterNot(_.trim.isEmpty) // a220930162941 - for better or worse, we ignore those (ideally we'd only ignore the last one)
        .map(_.splitXsv(sep))
  }

}

// ===========================================================================