package gallia
package actions.in

import aptus.Option_

import atoms.AtomsIX._

// ===========================================================================
case class JdbcInputZ1(
      inputString: String,
      queryingOpt: Option[ReadQuerying] /* missing if URI-driven */)
    extends ActionIZd {
  def vldt: Errs = Nil //TODO
  def _meta: Cls = atomiz.naive.force.toListAndTrash.pipe(SchemaInferrer.klass) //TODO: t201223092238 - use JDBC meta instead
  def atomiz = _JdbcInputZ1(inputString, queryingOpt) }

// ---------------------------------------------------------------------------
case class JdbcInputZ2(
      connection: java.sql.Connection,      
      querying: ReadQuerying)
    extends ActionIZd {
  def vldt: Errs = Nil //TODO
  def _meta: Cls = atomiz.naive.force.toListAndTrash.pipe(SchemaInferrer.klass) //TODO: t201223092238 - use JDBC meta instead
  def atomiz = _JdbcInputZ2(connection, querying) }

// ===========================================================================
case class MongodbInputZ(
      inputString   : String,
      schemaProvider: OtherSchemaProvider,
      queryingOpt   : Option[ReadQuerying] /* None if URI-driven (eg "mydb.mycoll") */)
    extends ActionIZd
    with    HasSchemaProviderZ { //TODO: t201223092203 - look into https://docs.mongodb.com/realm/mongodb/document-schemas
  def vldt: Errs = Nil //TODO: check valid URI, ...
  def _meta: Cls = __meta
  def hasCommonObjs = _MongodbInputZ(inputString, schemaProvider, queryingOpt, resultCls) } // TODO: t201214105653 - address resultCls hack

// ===========================================================================
