package gallia.actions.in

import aptus.Anything_
import aptus.Option_

import gallia._
import gallia.atoms.AtomsIX._

// ===========================================================================
case class JdbcInputZ(
      inputString: String,
      queryingOpt: Option[ReadQuerying] /* missing if URI-driven */)
    extends ActionIZd {
  def vldt: Errs = Nil //TODO
  def _meta: Cls = atomiz.naive.force.toListAndTrash.thn(SchemaInferrer.klass) //TODO: t201223092238 - use JDBC meta instead
  def atomiz = _JdbcInputZ(inputString, queryingOpt) }

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
