package gallia
package actions
package in

// ===========================================================================
case class MongodbInputZ(
      inputString   : String,
      schemaProvider: OtherSchemaProvider,
      queryingOpt   : Option[ReadQuerying] /* None if URI-driven (eg "mydb.mycoll") */)
    extends ActionIZ01
    with    HasSchemaProviderZ { // TODO: t201223092203 - look into https://docs.mongodb.com/realm/mongodb/document-schemas
  def vldt : Errs = Nil //TODO: check valid URI, ...
  def _meta: Cls  = __meta

  // ---------------------------------------------------------------------------
  def hasCommonObjs = new AtomsIX._MongodbInputZ(inputString, queryingOpt, resultCls  )
}

// ===========================================================================