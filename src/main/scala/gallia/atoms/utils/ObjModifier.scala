package gallia
package atoms
package utils

// ===========================================================================
class ObjModifier[T: WTT]( // 220425095249
     union    : Boolean,
     rootCtx  : ObjModifierCtx,
     transform: T => Any) {

  private val wtto: WeakTypeTagDecorator[T] = new WeakTypeTagDecorator(scala.reflect.runtime.universe.weakTypeTag[T])

    // ===========================================================================
            def modify                          (o: Obj): Obj = modify(rootCtx)(o)
    private def modify(ctx: ObjModifierNestings)(o: Obj): Obj = o.string_(_type).pipe { typeNameOpt => modify(ctx.withTypeName(typeNameOpt))(o) }
    private def modify(ctx: ObjModifierCtx)     (o: Obj): Obj =
      ctx
          .values
          .foldLeft(o) { (curr, qualifyingField) =>
            val key = qualifyingField.key

            if (!curr.contains(key)) curr
            else                     curr.transformPath(key, transformation(qualifyingField)) }

    // ===========================================================================
    private def transformation(qualifyingField: ObjModifierQualifyingFld): AnyValue => AnyValue =
      if (!union)
        (qualifyingField.multiple, qualifyingField.item) match {
          case (false, ObjModifierAction)             => _.asInstanceOf[    T ]     .pipe(transform)
          case (true , ObjModifierAction)             => _.asInstanceOf[Seq[T]]     .map (transform)
          case (false, ObjModifierNesting(nestedCtx)) => _.asInstanceOf[    Obj    ].pipe(modify(nestedCtx))
          case (true , ObjModifierNesting(nestedCtx)) => _.asInstanceOf[Seq[Obj   ]].map (modify(nestedCtx))
          case (_    , _: ObjModifierNestings)        => aptus.illegalState(qualifyingField, rootCtx) }

      // ---------------------------------------------------------------------------
      else
        qualifyingField.item match {

          case ObjModifierAction =>
            qualifyingField.multiple match {
              case false =>                                 v => if (!wtto.sameType(v)) v else v.asInstanceOf[T].pipe(transform)
              case true  => _.asInstanceOf[Seq[_   ]].map { v => if (!wtto.sameType(v)) v else v.asInstanceOf[T].pipe(transform) } } // TODO: only to check type for first one

          case ObjModifierNesting(nestedCtx) =>
            qualifyingField.multiple match {
              case false =>                               { case o: Obj => modify(nestedCtx)(o); case v => v }
              case true  => _.asInstanceOf[Seq[_   ]].map { case o: Obj => modify(nestedCtx)(o); case v => v } }

          case nestings: ObjModifierNestings =>
            qualifyingField.multiple match {
              case false =>                               { case o: Obj => modify(nestings)(o); case v => v }
              case true  => _.asInstanceOf[Seq[_   ]].map { case o: Obj => modify(nestings)(o); case v => v } } }

}

// ===========================================================================
