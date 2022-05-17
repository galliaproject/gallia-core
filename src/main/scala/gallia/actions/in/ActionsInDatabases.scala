package gallia
package actions
package in

import aptus.Option_

import atoms.AtomsIX._

// ===========================================================================
case class JdbcInputZ1(
      inputString: String,
      queryingOpt: Option[ReadQuerying] /* missing if URI-driven */)
    extends ActionIZd {
  private var c: Cls = null

  // ---------------------------------------------------------------------------
  def vldt : Errs = Nil //TODO
  def _meta: Cls  =
    _JdbcInputZ1(inputString, queryingOpt, None)
      .columns
      .pipe(utils.JdbcUtils.columnsToCls)
      .tap { c = _ }
  def atomiz: AtomIZ = _JdbcInputZ1(inputString, queryingOpt, Some(c)) }

// ---------------------------------------------------------------------------
case class JdbcInputZ2(
      connection: java.sql.Connection,      
      querying: ReadQuerying)
    extends ActionIZd {
  private var c: Cls = null

  // ---------------------------------------------------------------------------
  def vldt : Errs = Nil //TODO
  def _meta: Cls  =
    _JdbcInputZ2(connection, querying, None)
      .columns
      .pipe(utils.JdbcUtils.columnsToCls)
      .tap { c = _ }
  def atomiz: AtomIZ = _JdbcInputZ2(connection, querying, Some(c)) }

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
