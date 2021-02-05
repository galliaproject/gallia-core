package gallia.io.in

import gallia._

// ===========================================================================
sealed trait TableSchemaProvider {
    def isNoInferring: Boolean = this == TableSchemaProvider.NoInferring
    def isStringsOnly: Boolean = this == TableSchemaProvider.StringsOnly
    def isInfer      : Boolean = this == TableSchemaProvider.InferSchema
  }

  // ===========================================================================
  object TableSchemaProvider {

    case class  ExplicitSchema(c: Cls)   extends TableSchemaProvider

    /** mostly if header missing */
    case class  ExplicitKeys(keys: Keyz) extends TableSchemaProvider

    /** may be costly */
    object InferSchema extends TableSchemaProvider

    /** halfway */
    object StringsOnly extends TableSchemaProvider

    /** may be desirable if very heterogenous data that needs to be cleaned up */
    object NoInferring extends TableSchemaProvider
  }

// ===========================================================================
