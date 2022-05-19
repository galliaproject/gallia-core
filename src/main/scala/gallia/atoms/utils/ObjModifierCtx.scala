package gallia
package atoms
package utils

import aptus._

// ===========================================================================
case class ObjModifierCtx(
      nameOpt: ClsNameOpt, // only useful if multiple nestings (union)
      values : Seq[ObjModifierQualifyingFld]) {

    val keySet: Set[Key] = values.map(_.key).toSet

    // ---------------------------------------------------------------------------
    override def toString: String = formatDefault
      def formatDefault: String =
        values
          .map(_.formatDefault)
          .section(nameOpt.getOrElse(""))
  }
  // ===========================================================================
  trait ObjModifierQualifyingFld {
      val key        : Key
      val item       : ObjModifierItem

      // ---------------------------------------------------------------------------
      def formatDefault: String
    }

    // ===========================================================================
    case class ObjModifierQualifyingNonUnionFld(
            key     : Key,
            multiple: Multiple,
            item    : ObjModifierItemNonUnion)
          extends ObjModifierQualifyingFld {

        override def toString: String = formatDefault
          def formatDefault: String =
            Seq(
                key.name.append("\t"),
                formatMultiple(multiple).append("\t"),
                item.formatDefault)
              .join
      }
    // ===========================================================================
    case class ObjModifierQualifyingUnionFld(
            key        : Key,
            item       : ObjModifierItemUnion)
          extends ObjModifierQualifyingFld {

        override def toString: String = formatDefault
          def formatDefault: String =
            s"${key}\t${item.formatDefault}"
      }

  // ===========================================================================
  sealed trait ObjModifierItem { def formatDefault: String }
      sealed trait ObjModifierItemNonUnion extends ObjModifierItem
      sealed trait ObjModifierItemUnion    extends ObjModifierItem

    // ---------------------------------------------------------------------------
    case object ObjModifierNonUnionAction extends ObjModifierItemNonUnion {
        def formatDefault: String = getClass.getSimpleName }

      // ---------------------------------------------------------------------------
      case object ObjModifierUnionAction extends ObjModifierItemUnion {
        def formatDefault: String = getClass.getSimpleName }

    // ---------------------------------------------------------------------------
    case class ObjModifierNonUnionNesting(nesting: ObjModifierCtx) extends ObjModifierItemNonUnion {
        def formatDefault: String = nesting.formatDefault }

      // ---------------------------------------------------------------------------
      case class ObjModifierUnionNesting(alsoValue: Boolean, nesting: ObjModifierCtx) extends ObjModifierItemUnion {
          def formatDefault: String = s"${alsoValue}\t${nesting.formatDefault}" }

    // ---------------------------------------------------------------------------
    case class ObjModifierUnionNestings(alsoValue: Boolean, nestings: Seq[ObjModifierCtx]) extends ObjModifierItemUnion {
      require(nestings.nonEmpty)

      // ---------------------------------------------------------------------------
      def formatDefault: String = nestings.map(_.formatDefault).section(alsoValue.str)

      // ---------------------------------------------------------------------------
      def withTypeNameOpt(o: Obj)(typeOpt: Option[String]): Option[ObjModifierCtx] = typeOpt match {
          case None        => ObjModifierCtxUtils.guess(nestings)(o).in.some
          case Some(value) => nestings.find(_.nameOpt == Some(value)) }
    }

// ===========================================================================
