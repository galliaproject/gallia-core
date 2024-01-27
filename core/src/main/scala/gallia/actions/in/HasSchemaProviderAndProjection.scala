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
  trait HasSchemaProviderAndProjectionU[$HasCommonObj <: HasCommonObj]
        extends HasSchemaProviderAndProjectionx {
      protected def hasCommonObj: $HasCommonObj

      // ---------------------------------------------------------------------------
      protected final override def infer(): Cls = // TODO: t210115194646 - check if file size > x
        hasCommonObj
          .commonObj
          .pipe(SchemaInferrer.klass) }

    // ===========================================================================
    trait HasSchemaProviderAndProjectionZ[$HasCommonObjs <: HasCommonObjs]
        extends HasSchemaProviderAndProjectionx {
      protected def hasCommonObjs: $HasCommonObjs

      // ---------------------------------------------------------------------------
      protected final override def infer(): Cls = // TODO: t210115194646 - check if file size > x
        hasCommonObjs
          .commonObjs
          .toListAndTrash // because of c201104121618
          .pipe(SchemaInferrer.klass) }

// ===========================================================================
