package gallia
package io
package in

// ===========================================================================
sealed trait OtherSchemaProvider {

    def explicitOpt: Option[Cls] =
      this match {
         case OtherSchemaProvider.InferSchema       => None
         case OtherSchemaProvider.ExplicitSchema(c) => Some(c) } }

  // ===========================================================================
  object OtherSchemaProvider {

    /** may be costly */
    object InferSchema extends OtherSchemaProvider

    case class ExplicitSchema(c: Cls) extends OtherSchemaProvider }

// ===========================================================================
