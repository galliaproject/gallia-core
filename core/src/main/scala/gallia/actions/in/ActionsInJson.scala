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
    extends ActionIU01x with TodoV0
       with HasSchemaProviderAndProjectionUx with HasNoProjection {
  override def _meta: Cls   = __meta
  override def atomiux: AtomIUx = hasCommonObjx

    // ---------------------------------------------------------------------------
    override def hasCommonObjx = _JsonObjectString(inputString, schemaProvider) }

// ===========================================================================
case class JsonArrayString(inputString: InputString, schemaProvider: OtherSchemaProvider)
    extends ActionIZ01x with TodoV0
       with HasSchemaProviderAndProjectionZx with HasNoProjection {
  override def _meta  : Cls    = __meta
  override def atomizx: AtomIZx = hasCommonObjsx

    // ---------------------------------------------------------------------------
    override def hasCommonObjsx = _JsonArrayString(inputString, schemaProvider) }

// ===========================================================================
case class JsonObjectFileInputU(
      input         : InputUrlLike,
      schemaProvider: OtherSchemaProvider,
      projectionOpt : Option[ReadProjection])
    extends ActionIU01x with TodoV0
    with    HasSchemaProviderAndProjectionUx {
  override def _meta  : Cls     = __meta
  override def atomiux: AtomIUx = hasCommonObjx

    // ---------------------------------------------------------------------------
    override def hasCommonObjx = _JsonObjectFileInputU(input, projectionOpt, preProjectionSchema) }

// ===========================================================================
case class JsonLinesFileInputZ(
        input         : InputUrlLike,
        inMemoryMode  : Boolean,
        schemaProvider: OtherSchemaProvider,
        projectionOpt : Option[ReadProjection])
      extends ActionIZ01x with TodoV0 //TODO: 201104121618 - check not too big if not schema provided (or sample?)
      with    HasSchemaProviderAndProjectionZx {
    override def _meta  : Cls     = __meta
    override def atomizx: AtomIZx = hasCommonObjsx

      // ---------------------------------------------------------------------------
      override def hasCommonObjsx = _JsonLinesFileInputZ(input, inMemoryMode, projectionOpt, preProjectionSchema) }

  // ===========================================================================
  case class JsonArrayFileInputZ(
        input         : InputUrlLike,
        inMemoryMode  : Boolean,
        schemaProvider: OtherSchemaProvider,
        projectionOpt : Option[ReadProjection])
      extends ActionIZ01x with TodoV0 //TODO: 201104121618 - check not too big if not schema provided (or sample?)
      with    HasSchemaProviderAndProjectionZx {
    override def _meta  : Cls     = __meta
    override def atomizx: AtomIZx = hasCommonObjsx

      // ---------------------------------------------------------------------------
      override def hasCommonObjsx = _JsonArrayFileInputZ(input, inMemoryMode, projectionOpt) }

// ===========================================================================
