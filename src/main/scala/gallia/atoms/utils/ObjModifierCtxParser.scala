package gallia.atoms.utils

import aptus._
import gallia.{Cls, Fld}
import gallia.meta.SubInfo

// ===========================================================================
object ObjModifierCtxParser {

  def parse
        (c: Cls)
        (qualifies: Fld => Boolean) // eg "is int" as in JSON
      : Option[ObjModifierCtx] =
    c .fields
      .flatMap { field =>
        field
          .info.union.ifOne(
            sole     => nonUnionItemOpt(qualifies)(field)(sole)    .map(ObjModifierQualifyingNonUnionFld(field.key, field.hasMultiple, _)),
            multiple =>    unionItemOpt(qualifies)(field)(multiple).map(ObjModifierQualifyingUnionFld   (field.key,                    _))) }
      .in.noneIf(_.isEmpty)
      .map(ObjModifierCtx.apply(c.nameOpt, _))

  // ===========================================================================
  private def nonUnionItemOpt(qualifies: Fld => Boolean)(field: Fld)(subInfo: SubInfo): Option[ObjModifierItemNonUnion] =
    subInfo
      .nestedClassOpt
      .flatMap(parse(_)(qualifies).map(ObjModifierNonUnionNesting.apply))
      .orElse {
        if (qualifies(field)) Some(ObjModifierNonUnionAction)
        else                  None }

  // ---------------------------------------------------------------------------
  private def unionItemOpt(qualifies: Fld => Boolean)(field: Fld)(union: Seq[SubInfo]): Option[ObjModifierItemUnion] = {
    val doesQualify = qualifies(field)

    val actionOpt: Option[ObjModifierItemUnion] =
      if (doesQualify) Some(ObjModifierUnionAction)
      else             None

    union
      .flatMap(_.nestedClassOpt)
       match {
        case Nil => actionOpt

        // ---------------------------------------------------------------------------
        case Seq(sole) =>
          parse(sole)(qualifies)
            .map(ObjModifierUnionNesting(doesQualify, _))
            .orElse(actionOpt)

        // ---------------------------------------------------------------------------
        case multiple =>
          multiple
            .flatMap(parse(_)(qualifies))
            .inNoneIfEmpty
            .map(ObjModifierUnionNestings(doesQualify, _))
            .orElse(actionOpt) } }

}

// ===========================================================================
