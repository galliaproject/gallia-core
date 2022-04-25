package gallia
package atoms
package utils

import aptus._

// ===========================================================================
case class ObjModifierCtx(
    nameOpt: ClsNameOpt, // only useful if multiple nestings (union)
    values : Seq[ObjModifierQualifyingFld])

  // ===========================================================================
  object ObjModifierCtx {

    def parse
            (c: Cls)
            (qualifies: Fld => Boolean) // eg "is int" as in JSON
          : Option[ObjModifierCtx] =
        c .fields
          .flatMap { field =>
            field
              .pipe(itemOpt(qualifies))
              .map { item =>
                ObjModifierQualifyingFld(field.key, field.isMultiple, item) } }
          .in.noneIf(_.isEmpty)
          .map(ObjModifierCtx.apply(c.nameOpt, _))

      // ---------------------------------------------------------------------------
      private def itemOpt(qualifies: Fld => Boolean)(field: Fld): Option[ObjModifierItem] =
        field
          .nestedClassesOpt
           match {
            case None =>
              if (qualifies(field)) Some(ObjModifierAction)
              else                  None
            case Some(ncs) =>
              ncs
                .flatMap(parse(_)(qualifies))
                 match {
                  case Nil       => None
                  case Seq(sole) => Some(ObjModifierNesting(sole))
                  case more      => Some(ObjModifierNestings(more)) } }

  }

  // ===========================================================================
  case class ObjModifierQualifyingFld(key: Key, multiple: Boolean, item: ObjModifierItem)

    // ---------------------------------------------------------------------------
    sealed trait ObjModifierItem

      // ---------------------------------------------------------------------------
      /** e.g. turn Double into Int, as in JSON */
      case object ObjModifierAction extends ObjModifierItem

      // ---------------------------------------------------------------------------
      case class ObjModifierNesting(nesting: ObjModifierCtx) extends ObjModifierItem

      // ---------------------------------------------------------------------------
      case class ObjModifierNestings(nestings: Seq[ObjModifierCtx]) extends ObjModifierItem {
        require(nestings.nonEmpty)
        require(nestings.forall(_.nameOpt.nonEmpty), nestings.map(_.nameOpt))

        // ---------------------------------------------------------------------------
        def withTypeName(typeOpt: Option[String]): ObjModifierCtx = typeOpt match {
            case None        => ??? // TODO: t220422111733 - try and guess based on keys (unsupported yet)
            case Some(value) => nestings.find(_.nameOpt == Some(value)).getOrElse(???) }
      }

// ===========================================================================
