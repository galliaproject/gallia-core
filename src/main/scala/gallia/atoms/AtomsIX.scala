package gallia
package atoms

import aptus.{Anything_, String_}

import io._
import io.in._
import actions.in.HasProjection
import data.json.JsonParsing
import data.json.JsonNumberTax
import data.ModifyTableData
import data.multiple.streamer.Streamer

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
      def naive: Option[Obj] = Some(input.content.pipe(Obj.content)) }

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
      def commonObj: Obj = 
        JsonParsing
          .parseObject(inputString)
          .pipeIf(c != null /* TODO: see 211230183100 */)(JsonNumberTax.payUp(c)) }
  
    // ---------------------------------------------------------------------------  
    object _JsonObjectString {
      def toObj(c: Cls)(value: String): Obj = _JsonObjectString(value, null /* TODO */, c).commonObj
    }

  // ---------------------------------------------------------------------------
  case class _JsonArrayString(inputString: InputString, ignored /* TODO: t211231112700 - investigate */: OtherSchemaProvider, c: Cls) extends HasCommonObjs {
      def naive: Option[Objs] = Some(commonObjs)
  
      // ---------------------------------------------------------------------------
      def commonObjs: Objs =
        JsonParsing
          .parseArray(inputString)
          .pipe(Objs.from)
          .map { x => if (c == null)  /* TODO: see 211230183100 */ x else JsonNumberTax.payUp(c)(x) }
    }

    // ---------------------------------------------------------------------------  
    object _JsonArrayString {
      def toObjs(c: Cls)(value: String): Objs = _JsonArrayString(value, null /* TODO */, c).commonObjs
    }

  // ===========================================================================
  case class _JsonObjectFileInputU(input: InputUrlLike, c: Cls) extends HasCommonObj {
    def commonObj: Obj = input.content.pipe(JsonParsing.parseObject)
      def naive: Option[Obj] = commonObj.pipe(JsonNumberTax.payUp(c)).in.some
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
            .pipe(Objs.build)

      // ---------------------------------------------------------------------------
      def naive: Option[Objs] = Some(commonObjs.map(JsonNumberTax.payUp(schema)).pipe(projectData(schema)) )
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
.toList.mkString.pipe(JsonParsing.parseArray) // TODO: t201221175254 - actually stream array...
            .pipe(Objs.from)

      // ---------------------------------------------------------------------------
      def naive: Option[Objs] = Some(commonObjs.map(JsonNumberTax.payUp(schema)).pipe(projectData(schema)) )
    }

  // ===========================================================================
  case class _JdbcInputZ1(inputString: InputString, queryingOpt: Option[ReadQuerying] /* missing if URI-driven */) extends AtomIZ {
      import aptus.aptmisc.Rdbms.generalize
      import aptus.ResultSet_
  
      // ---------------------------------------------------------------------------
      def naive: Option[Objs] = {
        val sqlQuery = tmp.map(_.query).get// TODO: t210114202848 - validate
  
        val (rs, cls) = aptus.aptmisc.Rdbms(new java.net.URI(inputString)).query(sqlQuery)
  
        Streamer
          .fromIterator((rs.rawRdbmsEntries, cls))
          .map(generalize).map(obj)
          .pipe(Objs.build)
          .in.some
      }
  
      // ===========================================================================
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
    case class _JdbcInputZ2(connection: java.sql.Connection, querying: ReadQuerying) extends AtomIZ {
      import aptus.aptmisc.Rdbms.generalize
      import aptus.ResultSet_
  
      // ---------------------------------------------------------------------------
      def naive: Option[Objs] = {
        val (rs, cls) = aptus.aptmisc.Rdbms(connection/*new java.net.URI(inputString)*/).query(querying.query)
  
        Streamer
          .fromIterator((rs.rawRdbmsEntries, cls))
          .map(generalize).map(obj)
          .pipe(Objs.build)
          .in.some
      }
  
    }  

  // ===========================================================================
  case class _MongodbInputZ(
          inputString   : InputString,
          schemaProvider: OtherSchemaProvider,
          queryingOpt   : Option[ReadQuerying] /* None if URI-driven (eg "mydb.mycoll") */,
          c: Cls)
      extends HasCommonObjs { import _MongodbInputZ._
      mongoDb()

      def naive: Option[Objs] = commonObjs.map(JsonNumberTax.payUp(c)).in.some // TODO: confirm need to pay tax here

      // ===========================================================================
      // t210114153517 - must use jongo+find until figure out
      //   https://stackoverflow.com/questions/35771369/mongo-java-driver-how-to-create-cursor-in-mongodb-by-cusor-id-returned-by-a-db
      def commonObjs: Objs = {
          mongoDb.disableLogs()
          val cmd = cmdOpt.get // TODO: t210114152901 - validate

          mongoDb
            .query(new java.net.URI(inputString), None)(cmd)
            .pipe { case (iter, cls) =>
              Streamer.fromIterator((iter, cls)) }
            .map(obj)
            .pipe(Objs.build)
        }

        // ===========================================================================
        private def cmdOpt =
          tmp.flatMap {
             case ReadQuerying.All  (collection) => mongoDb.allFrom(collection).in.some
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
    def naive: Option[Objs] =
      aobjs(
           c =                                    projectMeta(defaultSchema).assert(!_.hasNesting),
           z = stringObjs(defaultSchema.keys).pipe(projectData(defaultSchema)) ) // see t210106120036 (project ealier)
        .pipe { x =>
          schemaProvider match {
            case TableSchemaProvider.NoInferring => x.z // nothing to do
            case _                               => new ModifyTableData(cellConf).modify(x) }}
        .in.some

    // ---------------------------------------------------------------------------
    def stringObjs(keys: Seq[Key]): Objs =
        dataRows
          .pipe(_.map(datum(cellConf.nullValues)(keys)))
          .pipe(Objs.build)

      // ---------------------------------------------------------------------------
      /** 201215141231 - null values are not handled here (see 201231113658 rather) */
      private def datum(nullValues: Seq[String])(keys: Seq[Key])(values: Seq[String]): Obj =
        keys
           // TODO throw proper error if duplicates
          .zip(values)   // TODO: check same size + charset and so on
          .pipe(Obj.fromIterable)

    // ===========================================================================
    private def dataRows: Streamer[List[String]] = // TODO: t210330110804 - separate each as their own atom
      input
        .streamLines(inMemoryMode)
        .pipeIf(hasHeader)(_.drop(1)) // TODO: t210116110159 for n >= 1?
        .map(_.splitXsv(sep))
  }

}

// ===========================================================================
