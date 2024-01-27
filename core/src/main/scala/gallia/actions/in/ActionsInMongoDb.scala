package gallia
package actions
package in

// ===========================================================================
case class MongodbInputZ(
      inputString   : String,
      schemaProvider: OtherSchemaProvider,
      queryingOpt   : Option[ReadQuerying] /* None if URI-driven (eg "mydb.mycoll") */)
    extends ActionIZ01y
    with    TodoV0 //TODO: check valid URI, ...
    with    HasSchemaProviderAndProjectionZ[AtomsIX._MongodbInputZ.Conf] with HasNoProjection {
  // TODO: t201223092203 - look into https://docs.mongodb.com/realm/mongodb/document-schemas

  // ---------------------------------------------------------------------------
  override def _meta: Cls = __meta
  override def atomizy(resultSchema: Cls): AtomIZ = new AtomsIX._MongodbInputZ(hasCommonObjs, resultSchema)

  // ---------------------------------------------------------------------------
  def hasCommonObjs = new AtomsIX._MongodbInputZ.Conf(inputString, queryingOpt) }

// ===========================================================================
