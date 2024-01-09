package gallia
package actions
package in

import io.in._
import atoms.AtomsIX._

// ===========================================================================
case class GenericInputU(schema: Cls, datum : Obj)    
        extends ActionIUd with TodoV0 {
      def _meta  = schema
      def atomiu = _GenericInputU(datum) }
  
  // ---------------------------------------------------------------------------
  case class GenericInputZ(
          schema: Cls,
          data  : DataRegenerationClosure[Obj])
        extends ActionIZd with TodoV0 {
      def _meta  = schema
      def atomiz = _GenericInputZb(data) }

// ===========================================================================
case class JsonObjectString(inputString: InputString, schemaProvider: OtherSchemaProvider) extends ActionIUd with TodoV0 with HasSchemaProviderU {
  def _meta: Cls   = __meta
  def hasCommonObj = _JsonObjectString(inputString, schemaProvider, resultCls /* 211230183100 */) }

// ===========================================================================
case class JsonArrayString(inputString: InputString, schemaProvider: OtherSchemaProvider) extends ActionIZd with TodoV0 with HasSchemaProviderZ {
  def _meta: Cls    = __meta
  def hasCommonObjs = _JsonArrayString(inputString, schemaProvider, resultCls /* 211230183100 */) }

// ===========================================================================
case class JsonObjectFileInputU(
      input         : InputUrlLike,
      schemaProvider: OtherSchemaProvider,
      projectionOpt : Option[ReadProjection])
    extends ActionIUd with TodoV0
    with    HasSchemaProviderAndProjectionU {
  def _meta: Cls   = __meta
  def hasCommonObj = _JsonObjectFileInputU(input, projectionOpt, protoSchema, resultCls /* 211230183100 */) }

// ===========================================================================
case class JsonLinesFileInputZ(
        input         : InputUrlLike,
        inMemoryMode  : Boolean,
        schemaProvider: OtherSchemaProvider,
        projectionOpt : Option[ReadProjection])
      extends ActionIZd with TodoV0 //TODO: 201104121618 - check not too big if not schema provided (or sample?)
      with    HasSchemaProviderAndProjectionZ {
    def _meta: Cls    = __meta
    def hasCommonObjs = _JsonLinesFileInputZ(input, inMemoryMode, projectionOpt, protoSchema, resultCls /* 211230183100 */) } // TODO: t201214105653 - address resultCls hack

  // ===========================================================================
  case class JsonArrayFileInputZ(
        input         : InputUrlLike,
        inMemoryMode  : Boolean,
        schemaProvider: OtherSchemaProvider,
        projectionOpt : Option[ReadProjection])
      extends ActionIZd with TodoV0 //TODO: 201104121618 - check not too big if not schema provided (or sample?)
      with    HasSchemaProviderAndProjectionZ {
    def _meta: Cls    = __meta
    def hasCommonObjs = _JsonArrayFileInputZ(input, inMemoryMode, projectionOpt, resultCls /* 211230183100 */) }

// ===========================================================================
