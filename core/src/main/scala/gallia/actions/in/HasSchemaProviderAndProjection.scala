package gallia
package actions
package in

import atoms.AtomsIX._

// ===========================================================================
sealed trait HasSchemaProviderAndProjectionx extends HasSchemaProvider with HasProjection {
      private[in] var preProjectionSchema: Cls = null // TODO: t220615121216 - address hack

      // ---------------------------------------------------------------------------
      protected def infer(): Cls

      // ---------------------------------------------------------------------------
      final def __meta: Cls =
          schemaProvider
            .explicitOpt
            .getOrElse {
              infer() }
            .tap { preProjectionSchema = _ }
            .pipe(projectMeta) }

  // ===========================================================================
  trait HasSchemaProviderAndProjectionUx extends HasSchemaProviderAndProjectionx {
      protected def hasCommonObjx: HasCommonObjx

      // ---------------------------------------------------------------------------
      protected def infer(): Cls = // TODO: t210115194646 - check if file size > x
        hasCommonObjx
          .commonObjx
          .pipe(SchemaInferrer.klass) }

    // ===========================================================================
    trait HasSchemaProviderAndProjectionZx extends HasSchemaProviderAndProjectionx {
      protected def hasCommonObjsx: HasCommonObjsx

      // ---------------------------------------------------------------------------
      protected def infer(): Cls = // TODO: t210115194646 - check if file size > x
        hasCommonObjsx
          .commonObjsx
          .toListAndTrash // because of c201104121618
          .pipe(SchemaInferrer.klass) }

// ===========================================================================
