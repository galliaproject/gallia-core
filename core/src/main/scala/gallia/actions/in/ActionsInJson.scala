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
    extends ActionIU01x with TodoV0 with HasSchemaProviderUx {
  override def _meta: Cls   = __meta
  override def hasCommonObjx = _JsonObjectString(inputString, schemaProvider)
  override def atomiux: AtomIUx = hasCommonObjx }

// ===========================================================================
case class JsonArrayString(inputString: InputString, schemaProvider: OtherSchemaProvider)
    extends ActionIZ01x with TodoV0 with HasSchemaProviderZx {
  def _meta: Cls    = __meta
  def hasCommonObjsx = _JsonArrayString(inputString, schemaProvider) }

// ===========================================================================
case class JsonObjectFileInputU(
      input         : InputUrlLike,
      schemaProvider: OtherSchemaProvider,
      projectionOpt : Option[ReadProjection])
    extends ActionIU01 with TodoV0
    with    HasSchemaProviderAndProjectionU0 {
  def _meta: Cls   = __meta
  def hasCommonObj = _JsonObjectFileInputU(input, projectionOpt, preProjectionSchema, resultCls /* 211230183100 */) }

// ===========================================================================
case class JsonLinesFileInputZ(
        input         : InputUrlLike,
        inMemoryMode  : Boolean,
        schemaProvider: OtherSchemaProvider,
        projectionOpt : Option[ReadProjection])
      extends ActionIZ01 with TodoV0 //TODO: 201104121618 - check not too big if not schema provided (or sample?)
      with    HasSchemaProviderAndProjectionZ0 {
    def _meta: Cls    = __meta
    def hasCommonObjs = _JsonLinesFileInputZ(input, inMemoryMode, projectionOpt, preProjectionSchema, resultCls /* 211230183100 */) } // TODO: t201214105653 - address resultCls hack

  // ===========================================================================
  case class JsonArrayFileInputZ(
        input         : InputUrlLike,
        inMemoryMode  : Boolean,
        schemaProvider: OtherSchemaProvider,
        projectionOpt : Option[ReadProjection])
      extends ActionIZ01 with TodoV0 //TODO: 201104121618 - check not too big if not schema provided (or sample?)
      with    HasSchemaProviderAndProjectionZ0 {
    def _meta: Cls    = __meta
    def hasCommonObjs = _JsonArrayFileInputZ(input, inMemoryMode, projectionOpt, resultCls /* 211230183100 */) }

// ===========================================================================
