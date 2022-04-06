package gallia
package actions

// ===========================================================================
package object in extends gallia.__io with gallia._io {
  val AtomsIX = gallia.atoms.AtomsIX

  // ===========================================================================
  type ReadProjection = gallia.io.in.ReadProjection
  val  ReadProjection = gallia.io.in.ReadProjection

  type ReadQuerying = gallia.io.in.ReadQuerying
  val  ReadQuerying = gallia.io.in.ReadQuerying

  // ===========================================================================
  val SchemaInferrer = gallia.inferring.SchemaInferrer

  // ---------------------------------------------------------------------------
  type OtherSchemaProvider = gallia.io.in.OtherSchemaProvider
  val  OtherSchemaProvider = gallia.io.in.OtherSchemaProvider

  // ===========================================================================
  type InputUrlLike = gallia.io.in.InputUrlLike

  // ---------------------------------------------------------------------------
  trait HasSchemaProvider { val schemaProvider: OtherSchemaProvider }
}

// ===========================================================================
