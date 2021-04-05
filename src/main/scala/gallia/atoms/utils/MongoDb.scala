package gallia.atoms.utils


import java.io.Closeable
import aptus._

// ===========================================================================
trait MongoDb {
  import MongoDb.MongoDbCmd

  // ---------------------------------------------------------------------------
  def setLogLevel(level: java.util.logging.Level): Unit
  def disableLogs() = setLogLevel(java.util.logging.Level.OFF)

  // ---------------------------------------------------------------------------
  def uriCollectionOpt(uriString: String): Option[String]
  def uriDatabaseOpt  (uriString: String): Option[String]

  // ---------------------------------------------------------------------------
  def allFrom(collection: String):        MongoDbCmd  = MongoDbCmd.all(collection)
  def query  (query     : String): Option[MongoDbCmd] = MongoDbCmd.parseOpt(query)

  // ---------------------------------------------------------------------------
  def query(uri: java.net.URI, dbOpt: Option[String])(cmd: MongoDbCmd): (Iterator[List[(Symbol, Any)]], Closeable)

  final def closeableQuery(uri: java.net.URI, dbOpt: Option[String])(cmd: MongoDbCmd): Closeabled[Iterator[List[(Symbol, Any)]]] =
    query(uri, dbOpt)(cmd).thn(Closeabled.fromPair)
}

// ===========================================================================
object MongoDb {
  var DefaultLogLevel: java.util.logging.Level = java.util.logging.Level.OFF

  // ===========================================================================
  case class MongoDbCmd(
      collection : String, // TODO: t201103144601 - exceptionally allow db.coll format (non-standard)?
      filter     : Option[JsonString],
      projection : Option[JsonString],
      sort       : Option[JsonString],
      limit      : Option[Int])

    // ===========================================================================
    object MongoDbCmd {

      private val Supported = Set('find, 'filter, 'projection, 'sort, 'limit)

      // ---------------------------------------------------------------------------
      val Dummy =
          MongoDbCmd(
            collection = "coll1",
            filter     = Some("""{"_id": {"$lt": 4}}"""),
            projection = Some("""{_id: 1}"""),
            sort       = None,
            limit      = None)

      // ---------------------------------------------------------------------------
      def all(collection: String) =
          MongoDbCmd(
            collection = collection,
            filter     = None,
            projection = None,
            sort       = None,
            limit      = None)

      // ===========================================================================
      def parseOpt(command: String): Option[MongoDbCmd] = { // TODO: t210110112325 - cleaner version
        val c = gallia.data.json.JsonParsing.parseObject(command) // TODO: wrap...

        // TODO: a basic gson version?
        c.string_('find)
          .map { collection =>
            // TODO: convenient/lightweight cc-based validation out there?

            if (!c.keySet.forall(Supported.contains)) ??? // TODO

            val filterOpt = c.opt('filter).map {
                case x: gallia.Obj => x.formatCompactJson
                case x => ???
              }

            val projectionOpt = c.opt('projection).map {
                case x: gallia.Obj => x.formatCompactJson
                case x => ???
              }

            val sortOpt = c.opt('sort).map {
                case x: gallia.Obj => x.formatCompactJson
                case x => ???
              }

            val limitOpt =
              c
                .text_('limit)
                .map { x => if (!x.isValidInt) x.toInt else ??? } // TODO

            MongoDbCmd(
              collection = collection,
              filter     = filterOpt,
              projection = projectionOpt,
              sort       = sortOpt,
              limit      = limitOpt)
          }
        }
    }
}

// ===========================================================================
