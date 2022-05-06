package gallia
package actions
package utils

// ===========================================================================
object NestedTransformUnionAmbiguityResolver {

  // ---------------------------------------------------------------------------
  // vldt

  def vldt(c: Cls, kpath: KPath)(nameOpt: ClsNameOpt, _vldt: Cls => Errs): Errs =
      kpath
        .pipe(c.field_).toSeq
        .map(nestedClassesEither(nameOpt))
        .flatMap {
          case Right(sole)    => _vldt(sole)
          case Left(multiple) =>
            val validationErrors = multiple.map(_vldt)

            validationErrors.ifOneMatch(_.isEmpty)(
               soleNoErrors => soleNoErrors,
              _             => validationErrors.flatten /* may still be empty */) }

    // ---------------------------------------------------------------------------
    private def nestedClassesEither(nameOpt: ClsNameOpt)(field: Fld): Either[Seq[Cls], Cls] =
      field
        .ofni.infos.map(_.containee)
        .flatMap(_.nestingOpt)
        .ifOneMatchEither { nc => nameOpt.forall {
          name => nc.nameOpt == Some(name) } }

  // ===========================================================================
  // meta

  def transformNestedClass(field: Fld)(nameOpt: ClsNameOpt)(f: Cls  => Cls): Fld =
      nameOpt
        .map { name => ??? }//field.transformNestedClass(name)(f) }
        .getOrElse {
          val nestedClasses = field.forceNestedClasses
          if (nestedClasses.size == 1)  field.transformSoleNestedClass(f)
          else                               _transformNestedClass(nestedClasses)(field)(f) }

    // ---------------------------------------------------------------------------
    private def _transformNestedClass(nestedClasses: Seq[Cls])(field: Fld)(f: Cls  => Cls): Fld =
      nestedClasses
        .map { nc => util.Try(f(nc)) }
        .ifOneMatch(_.isSuccess)(
          sole => field.transformOfni(_.transformNestedClass(sole.get)(f)),
          _    => ???) // TODO: error message

}

// ===========================================================================
