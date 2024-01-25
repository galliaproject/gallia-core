package gallia
package actions
package in

// ===========================================================================
case class MongodbInputZ(
      inputString   : String,
      schemaProvider: OtherSchemaProvider,
      queryingOpt   : Option[ReadQuerying] /* None if URI-driven (eg "mydb.mycoll") */)
    extends ActionIZ01x
    with    TodoV0 //TODO: check valid URI, ...
    with    HasSchemaProviderZx { // TODO: t201223092203 - look into https://docs.mongodb.com/realm/mongodb/document-schemas

  override def _meta  : Cls     = __meta
  override def atomizx: AtomIZx = hasCommonObjsx

    // ---------------------------------------------------------------------------
    def hasCommonObjsx = new AtomsIX._MongodbInputZ(inputString, queryingOpt) }

// ===========================================================================
