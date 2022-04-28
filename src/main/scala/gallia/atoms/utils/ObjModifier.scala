package gallia
package atoms
package utils

import scala.reflect.runtime.universe.weakTypeTag

// ===========================================================================
class ObjModifier[T: WTT]( // 220425095249
     rootCtx  : ObjModifierCtx,
     transform: T => Any) {

  private val wtto: WeakTypeTagDecorator[T] = new WeakTypeTagDecorator(weakTypeTag[T])

  // ===========================================================================
  def modify(o: Obj): Obj = modify(rootCtx)(o)

    // ---------------------------------------------------------------------------
    private def modify(ctx: ObjModifierUnionNestings)(o: Obj): Obj =
      ctx
        .withTypeNameOpt(o)(o.string_(_type))
        .map(modify(_)(o))
        .getOrElse(o)

    // ---------------------------------------------------------------------------
    private def modify(ctx: ObjModifierCtx)          (o: Obj): Obj =
      ctx
          .values
          .foldLeft(o) { (curr, qualifyingField) =>
            val key = qualifyingField.key

            if (!curr.contains(key)) curr
            else                     curr.transformPath(key, qualifyingField match {
              case x: ObjModifierQualifyingNonUnionFld => nonUnionTransformation(x)
              case x: ObjModifierQualifyingUnionFld    =>    unionTransformation(x) } ) }

  // ===========================================================================
  private def nonUnionTransformation(qualifyingField: ObjModifierQualifyingNonUnionFld): AnyValue => AnyValue =
      (qualifyingField.multiple, qualifyingField.item) match {
        case (false, ObjModifierNonUnionAction)             => _.asInstanceOf[    T ]     .pipe(transform)
        case (true , ObjModifierNonUnionAction)             => _.asInstanceOf[Seq[T]]     .map (transform)
        case (false, ObjModifierNonUnionNesting(nestedCtx)) => _.asInstanceOf[    Obj    ].pipe(modify(nestedCtx))
        case (true , ObjModifierNonUnionNesting(nestedCtx)) => _.asInstanceOf[Seq[Obj   ]].map (modify(nestedCtx)) }

  // ---------------------------------------------------------------------------
  private def unionTransformation(qualifyingField: ObjModifierQualifyingUnionFld): AnyValue => AnyValue = {
    def actUnion(value: AnyValue): AnyValue =
      if (!wtto.sameType(value)) value
      else                       value.asInstanceOf[T].pipe(transform)

    // ---------------------------------------------------------------------------
    qualifyingField.item match {

      case ObjModifierUnionAction =>
        _ match {
          case seq: Seq[_] => seq.map(actUnion)
          case sgl         =>         actUnion(sgl) }

      // ---------------------------------------------------------------------------
      case ObjModifierUnionNesting(alsoValue, nestedCtx) =>
        _ match {
          case seq: Seq[_] => seq.map {
            case obj: Obj    => modify(nestedCtx)(obj)
            case sgl         => if (alsoValue) actUnion(sgl) else sgl }
          case obj: Obj      => modify(nestedCtx)(obj)
          case sgl           => if (alsoValue) actUnion(sgl) else sgl }

      // ---------------------------------------------------------------------------
      case nestings: ObjModifierUnionNestings => import nestings.alsoValue
        _ match {
          case seq: Seq[_] => seq.map {
            case obj: Obj    => modify(nestings)(obj)
            case sgl         => if (alsoValue) actUnion(sgl) else sgl }
          case obj: Obj      => modify(nestings)(obj)
          case sgl           => if (alsoValue) actUnion(sgl) else sgl } }
  }

}

// ===========================================================================
