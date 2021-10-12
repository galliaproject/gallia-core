package gallia
package actions.in

import io.in._
import atoms.AtomsIX._

// ===========================================================================
case class JsonObjectString(inputString: InputString, schemaProvider: OtherSchemaProvider) extends ActionIUd with TodoV0 with HasSchemaProviderU {
  def _meta: Cls   = __meta
  def hasCommonObj = _JsonObjectString(inputString, schemaProvider, resultCls) }

// ===========================================================================
case class JsonArrayString(inputString: InputString, schemaProvider: OtherSchemaProvider) extends ActionIZd with TodoV0 with HasSchemaProviderZ {
  def _meta: Cls    = __meta
  def hasCommonObjs = _JsonArrayString(inputString, schemaProvider, resultCls) }

// ===========================================================================
case class JsonObjectFileInputU(
      input         : InputUrlLike,
      schemaProvider: OtherSchemaProvider,
      projectionOpt : Option[ReadProjection])
    extends ActionIUd with TodoV0
    with    HasSchemaProviderAndProjectionU {
  def _meta: Cls   = __meta
  def hasCommonObj = _JsonObjectFileInputU(input, resultCls) } // FIXME: data projection

// ===========================================================================
case class JsonLinesFileInputZ(
        input         : InputUrlLike,
        inMemoryMode  : Boolean,
        schemaProvider: OtherSchemaProvider,
        projectionOpt : Option[ReadProjection])
      extends ActionIZd with TodoV0 //TODO: 201104121618 - check not too big if not schema provided (or sample?)
      with    HasSchemaProviderAndProjectionZ
      with    HasProjection {
    def _meta: Cls    = __meta
    def hasCommonObjs = _JsonLinesFileInputZ(input, inMemoryMode, projectionOpt, resultCls) } // TODO: t201214105653 - address resultCls hack

  // ===========================================================================
  case class JsonArrayFileInputZ(
        input         : InputUrlLike,
        inMemoryMode  : Boolean,
        schemaProvider: OtherSchemaProvider,
        projectionOpt : Option[ReadProjection])
      extends ActionIZd with TodoV0 //TODO: 201104121618 - check not too big if not schema provided (or sample?)
      with    HasSchemaProviderAndProjectionZ {
    def _meta: Cls    = __meta
    def hasCommonObjs = _JsonArrayFileInputZ(input, inMemoryMode, projectionOpt, resultCls) }

// ===========================================================================
