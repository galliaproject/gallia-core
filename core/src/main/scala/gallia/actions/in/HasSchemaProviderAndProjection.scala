package gallia
package actions
package in

import atoms.AtomsIX._

// ===========================================================================
trait HasSchemaProviderU extends HasSchemaProviderAndProjectionU0 with HasNoProjection
trait HasSchemaProviderZ extends HasSchemaProviderAndProjectionZ0 with HasNoProjection

trait HasSchemaProviderUx extends HasSchemaProviderAndProjectionUx with HasNoProjection
trait HasSchemaProviderZx extends HasSchemaProviderAndProjectionZx with HasNoProjection

// ===========================================================================
                                                            @deprecated trait HasSchemaProviderAndProjectionU0 extends HasSchemaProvider with HasProjection {
                                                                private[in] var preProjectionSchema: Cls = null // TODO: t220615121216 - address hack

                                                                // ---------------------------------------------------------------------------
                                                                def __meta: Cls =
                                                                    schemaProvider
                                                                      .explicitOpt
                                                                      .getOrElse {
                                                                        infer() }
                                                                      .tap { preProjectionSchema = _ }
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
                                                                final def atomiu = hasCommonObj }

                                                              // ===========================================================================
                                                              @deprecated trait HasSchemaProviderAndProjectionZ0 extends HasSchemaProvider with HasProjection {
                                                                private[in] var preProjectionSchema: Cls = null // pre-projection schema; TODO: t220615121216 - address hack (also see t201214105653)

                                                                // ---------------------------------------------------------------------------
                                                                def __meta: Cls =
                                                                    schemaProvider
                                                                      .explicitOpt
                                                                      .getOrElse {
                                                                        infer() }
                                                                      .tap { preProjectionSchema = _ }
                                                                      .pipe(projectMeta)

                                                                  // ---------------------------------------------------------------------------
                                                                  // TODO: t210115194646 - check if file size > x
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

    protected def infer(): Cls = // TODO: t210115194646 - check if file size > x
      hasCommonObjx
        .commonObjx
        .pipe(SchemaInferrer.klass)

    // ---------------------------------------------------------------------------
    protected def hasCommonObjx: HasCommonObjx }

  // ===========================================================================
  trait HasSchemaProviderAndProjectionZx extends HasSchemaProviderAndProjectionx {
    protected def infer(): Cls = // TODO: t210115194646 - check if file size > x
      hasCommonObjsx
        .commonObjsx
        .toListAndTrash // because of c201104121618
        .pipe(SchemaInferrer.klass)

    // ---------------------------------------------------------------------------
    final def atomizx: AtomIZx = hasCommonObjsx

      // ---------------------------------------------------------------------------
      protected def hasCommonObjsx: HasCommonObjsx }

// ===========================================================================
