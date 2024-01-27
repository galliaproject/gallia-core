package gallia
package actions
package in

import io.in._
import atoms.AtomsIX._

// ===========================================================================
case class GenericInputU(schema: Cls, datum : Obj)    
        extends ActionIU01 with TodoV0 {
      def _meta  = schema
      def atomiu = _GenericInputU(datum) }
  
  // ---------------------------------------------------------------------------
  case class GenericInputZ(
          schema: Cls,
          data  : DataRegenerationClosure[Obj])
        extends ActionIZ01 with TodoV0 {
      def _meta  = schema
      def atomiz = _GenericInputZb(data) }

// ===========================================================================
case class JsonObjectString(inputString: InputString, schemaProvider: OtherSchemaProvider)
    extends ActionIU01y with TodoV0
       with HasSchemaProviderAndProjectionU[_JsonObjectString.Conf] with HasNoProjection {
  override def _meta: Cls = __meta
  override def atomiuy(resultSchema: Cls): AtomIU = _JsonObjectString(hasCommonObj, resultSchema)

  // ---------------------------------------------------------------------------
  override def hasCommonObj = _JsonObjectString.Conf(inputString, schemaProvider) }

// ===========================================================================
case class JsonArrayString(inputString: InputString, schemaProvider: OtherSchemaProvider)
    extends ActionIZ01y with TodoV0
       with HasSchemaProviderAndProjectionZ[_JsonArrayString.Conf]
       with HasNoProjection {
  override def _meta: Cls = __meta
  override def atomizy(resultSchema: Cls): AtomIZ = _JsonArrayString(hasCommonObjs, resultSchema)

  // ---------------------------------------------------------------------------
  override def hasCommonObjs = _JsonArrayString.Conf(inputString, schemaProvider) }

// ===========================================================================
case class JsonObjectFileInputU(
      input         : InputUrlLike,
      schemaProvider: OtherSchemaProvider,
      projectionOpt : Option[ReadProjection])
    extends ActionIU01y with TodoV0
    with    HasSchemaProviderAndProjectionU[_JsonObjectFileInputU.Conf] {
  override def _meta: Cls = __meta
  override def atomiuy(resultSchema: Cls): AtomIU = _JsonObjectFileInputU(hasCommonObj, resultSchema)

  // ---------------------------------------------------------------------------
  override def hasCommonObj = _JsonObjectFileInputU.Conf(input, projectionOpt, preProjectionSchema) }

// ===========================================================================
case class JsonLinesFileInputZ(
        input         : InputUrlLike,
        inMemoryMode  : Boolean,
        schemaProvider: OtherSchemaProvider,
        projectionOpt : Option[ReadProjection])
      extends ActionIZ01y with TodoV0 //TODO: 201104121618 - check not too big if not schema provided (or sample?)
      with    HasSchemaProviderAndProjectionZ[_JsonLinesFileInputZ.Conf] {
    override def _meta: Cls = __meta
    override def atomizy(resultSchema: Cls): AtomIZ = _JsonLinesFileInputZ(hasCommonObjs, resultSchema)

    // ---------------------------------------------------------------------------
    override def hasCommonObjs = _JsonLinesFileInputZ.Conf(input, inMemoryMode, projectionOpt, preProjectionSchema) }

  // ===========================================================================
  case class JsonArrayFileInputZ(
        input         : InputUrlLike,
        inMemoryMode  : Boolean,
        schemaProvider: OtherSchemaProvider,
        projectionOpt : Option[ReadProjection])
      extends ActionIZ01y with TodoV0 //TODO: 201104121618 - check not too big if not schema provided (or sample?)
      with    HasSchemaProviderAndProjectionZ[_JsonArrayFileInputZ.Conf] {
    override def _meta: Cls = __meta
    override def atomizy(resultSchema: Cls): AtomIZ = _JsonArrayFileInputZ(hasCommonObjs, resultSchema)

    // ---------------------------------------------------------------------------
    override def hasCommonObjs = _JsonArrayFileInputZ.Conf(input, inMemoryMode, projectionOpt) }

// ===========================================================================
