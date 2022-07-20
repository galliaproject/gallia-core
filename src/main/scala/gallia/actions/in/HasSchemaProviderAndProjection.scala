package gallia
package actions
package in

import atoms.AtomsIX._

// ===========================================================================
trait HasSchemaProviderU extends HasSchemaProviderAndProjectionU { val projectionOpt: Option[ReadProjection] = None }
trait HasSchemaProviderZ extends HasSchemaProviderAndProjectionZ { val projectionOpt: Option[ReadProjection] = None }

// ===========================================================================
trait HasSchemaProviderAndProjectionU extends HasSchemaProvider with HasProjection {
    private[in] var protoSchema: Cls = null // pre-projection schema; TODO: t220615121216 - address hack

    // ---------------------------------------------------------------------------
    def __meta: Cls =
      schemaProvider
        .resolve(default = infer())
        .tap { protoSchema = _ }
        .pipe(projectMeta)

      // ---------------------------------------------------------------------------
      // TODO: t210115194646 - check if file size > x
      private def infer(): Cls =
        hasCommonObj
          .commonObj
          .pipe(SchemaInferrer.klass)

    // ===========================================================================
    protected def hasCommonObj: HasCommonObj

    // ---------------------------------------------------------------------------
    final def atomiu = hasCommonObj
  }

  // ===========================================================================
  trait HasSchemaProviderAndProjectionZ extends HasSchemaProvider with HasProjection {
    private[in] var protoSchema: Cls = null // pre-projection schema; TODO: t220615121216 - address hack (also see t201214105653)

    // ---------------------------------------------------------------------------
    def __meta: Cls =
      schemaProvider
        .resolve(default = infer())
        .tap { protoSchema = _ }
        .pipe(projectMeta)

      // ---------------------------------------------------------------------------
      // TODO: t210115194646: check if file size > x
      private def infer(): Cls =
        hasCommonObjs
          .commonObjs
          .toListAndTrash // because of c201104121618
          .pipe(SchemaInferrer.klass)

    // ===========================================================================
    protected def hasCommonObjs: HasCommonObjs

    // ---------------------------------------------------------------------------
    final def atomiz = hasCommonObjs
  }


// ===========================================================================
