package gallia.io.in

import aptus.Anything_
import aptus.MirrorIndex

import gallia._
import gallia.io.in.ReadProjection._
import gallia.io.HasSelf

// ===========================================================================
trait CanSetMemoryModeFluency[Fluency] { self: HasSelf[Fluency] => // TODO: t210114145059 - generalize: inMemory, iterator, RDD (StreamerType)
  def iteratorMode: Fluency
}

// ===========================================================================
trait CanSetSchemaFluency[Fluency] { _: HasSelf[Fluency] =>
  def explicitSchema(c: Cls): Fluency
  def inferSchema           : Fluency

  // ---------------------------------------------------------------------------
  // TODO: separate validation...
  // TODO: t210128103821 - generalize schema i/o for any format schema-like that can be (reasonnably) parsed, may be useful and limit code duplication (JSON schema, ...)
  final def schema[T: WTT]                 : Fluency = cls[T]             .pipe(explicitSchema)
  final def schema(schemaFilePath: String) : Fluency = cls(schemaFilePath).pipe(explicitSchema)
  final def schema(field1: Fld, more: Fld*): Fluency = explicitSchema(Cls(field1 +: more))
  final def schema(c: Cls)                 : Fluency = explicitSchema(c)

  // ---------------------------------------------------------------------------
  private[io] final def schemaProvider(schemaProvider: OtherSchemaProvider): Fluency = schemaProvider.explicitOpt.map(c => explicitSchema(c)).getOrElse(self)
}

// ===========================================================================
trait CanProjectFluency[Fluency] {
  private[io] def project(projectionOpt: Option[ReadProjection]): Fluency

  // ===========================================================================
  protected def retain(values: RPathz): Fluency = project(Some(Retainees1(values)))

    def retain(target1: RPathW, more: RPathW*): Fluency = retain(RPathWz._to(target1, more).qpathz)
    def retain(targets: RPathWz)              : Fluency = retain(targets.qpathz)

  protected def remove(values: KPathz): Fluency = project(Some(Removees1(values)))

    def remove(target1: KPathW, more: KPathW*): Fluency = remove(KPathWz._to(target1, more).kpathz)
    def remove(targets: KPathWz)              : Fluency = remove(targets.kpathz)

  // ---------------------------------------------------------------------------
  // TODO: offer a retain/removeRank that would be 1-based (cognitive burden of dealing with the 0-based/1-based discrepancy of "index")
  def retainIndices(targets: MIndexEntries)                  : Fluency = project(Some(Retainees2(targets)))
  def retainIndices(target1: MIndexEntry, more: MIndexEntry*): Fluency = retainIndices(MIndexEntries(target1 +: more))

  def removeIndices(targets: Seq[MirrorIndex])               : Fluency = project(Some(Removees2(targets)))
  def removeIndices(target1: MirrorIndex, more: MirrorIndex*): Fluency = removeIndices(target1 +: more)

  def retainFirstFields(n: Int): Fluency = retainIndices(Range(0, n))
  def retainLastFields (n: Int): Fluency = ???//retainIndices(???) - TODO

  def removeFirstFields(n: Int): Fluency = removeIndices(Range(0, n))
  def removeLastFields (n: Int): Fluency = ???//retainIndices(???) - TODO
}

// ===========================================================================
trait CanQueryFluency[Self] {
  // TODO: allow providing reusable connections/prepared statements, ...

  protected[io] def querying(value: ReadQuerying): Self

  // ===========================================================================
  /*
   * TODO:
   * - ensure no container already in URI
   * - versions for graphql (t210128163917)/sparql (t210128163918)/excel sheet (t210128160323)/...
   */
  def allFrom(container: String): Self  = querying(ReadQuerying.All(container)) // eg table, collection, ontology

    // ---------------------------------------------------------------------------
    /**
      e.g.:
      - SQL    : """SELECT * FROM table1 LIMIT 3 OFFSET 1""" (on jdbc:myfavdb/testdb)
      - mongodb: db.zips.find("""{ age: {$gt: 18}}""") (on mongodb://zips) - see command eval counterpart
      - ES     : """{"query": {"query_string": {"query": "(new york city) OR (big apple)", "default_field": "content" }}}""" (on TODO)
      - cypher : """(:Person {name: string})-[:ACTED_IN {roles: [string]}]->(:Movie {title: string, released: number})""" (on neo4j+s://demo.neo4jlabs.com); TODO: see https://neo4j.com/developer/example-project/, http://localhost:8080/movie/The%20Matrix
      - SPARQL : """SELECT DISTINCT * WHERE {?s <http://www.w3.org/2000/01/rdf-schema#label> "common cold"} LIMIT 3""" (on http://www.disease-ontology.org?query=")
      - graphQL: """{user (id: 1) { firstname } }""" (on https://swapi.com/graphql)

      - redis  : ? TODO: no string-based QL? - https://redis.io/topics/quickstart - https://redis.io/clients
    */
    def query(query: String): Self = querying(ReadQuerying.Query(query))

}

// ===========================================================================
