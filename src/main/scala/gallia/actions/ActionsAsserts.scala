package gallia
package actions

import target._
import FunctionWrappers._
import meta.InfoLike
import atoms.AtomsUV._
import atoms.AtomsAsserts._

// ===========================================================================
object ActionsAsserts {

  case class AssertDataClass(node: TypeNode) extends ActionUUa with IdentityM1 with IdentityUUa {
      def vldt(c: Cls): Errs = node.leaf.dataClassEither match {
        case Left (errorDetails)   => _Error.InvalidDataClass(errorDetails).errs
        case Right(validDataClass) => _vldt.classCompatibilities(c, validDataClass).toSeq } }

  // ===========================================================================
  case class AssertMeta(pred: Cls => Boolean) extends IdentityM1 with IdentityUUa {
        def vldt (c: Cls): Errs =
          _Error.MetaAssertionFailure.errsIf(test = !pred(c)) }

    // ---------------------------------------------------------------------------
    case class AssertField(target: KPath, _error: _Error, pred: InfoLike => Boolean) extends IdentityM1 with IdentityUUa {
          def vldt (c: Cls): Errs =
            _error.errsIf(test = !c.field_(target).exists(pred)) }

      // ---------------------------------------------------------------------------
      // TODO: as predicate of Container rather
      case class AssertContainer(target: KPath, container: Container) extends IdentityM1 with IdentityUUa { // TODO: t220427091730 - change to optional/multiple
          def vldt (c: Cls): Errs =
            _Error.ContainerAssertionFailure(target, container).errsIf(
                !c.field_(target).exists(
                    _.info.container1 == container)) }

      // ---------------------------------------------------------------------------
      //TODO: as predicate of rather
      case class AssertBasicType(target: KPath, basicType: BasicType) extends IdentityM1 with IdentityUUa {
            def vldt (c: Cls): Errs =
              _Error.ValueTypeAssertionFailure(target, basicType).errsIf(
                  !c.field_(target).exists(
                      _.areAllBasicType(basicType))) }

      // ---------------------------------------------------------------------------
      case class AssertUnionType(target: KPath, negated: Boolean) extends IdentityM1 with IdentityUUa {
            def vldt (c: Cls): Errs =
              _Error.ValueTypeAssertionFailure(target, null).errsIf(
                  !c.field_(target).exists(field =>
                      (!negated &&  field.isUnionType) ||
                      ( negated && !field.isUnionType))) }

  // ===========================================================================
  case class AssertDataUnsafeU(pred: gallia.Obj => Boolean) extends ActionUU with IdentityVM1 with AtomsUUd {
        def atomuu: AtomUU = _AssertUnsafeO(pred) }

    case class AssertDataUnsafeZ(pred: gallia.Objs => Boolean) extends ActionZZ with IdentityVM1 with AtomsZZd {
        def atomzz: AtomZZ = _AssertUnsafeZ(pred) }

  // ===========================================================================
  case class AssertDataU1[I1](from: TtqKPath, pred: I1 => Boolean) extends ActionUUc with IdentityM1 {
      def  vldt (c: Cls): Errs  = from.vldtAsOrigin(c)

      def atomuu(c: Cls): AtomUU =
        from
          .tq.resolve(c)
          .pipe { path =>
            if (from.tq.isRequired(c)) _AssertO1a(path, pwrap11(pred))
            else                       _AssertO1b(path, pwrap11(pred)) } }

    // ---------------------------------------------------------------------------
    @Max5 case class AssertDataU2[I1, I2](from: TtqKPath2, pred: (I1, I2) => Boolean) extends ActionUUc with IdentityM1 {
        def  vldt (c: Cls): Errs   = from.vldtAsOrigin(c)
        def atomuu(c: Cls): AtomUU = _SquashU2(from.pathPairT(c), pwrap21(pred)).pipe(_AssertO2) }

      // ---------------------------------------------------------------------------
      case class AssertDataU3[I1, I2, I3](from: TtqKPath3, pred: (I1, I2, I3) => Boolean) extends ActionUUc with IdentityM1 {
        def  vldt (c: Cls): Errs   = from.vldtAsOrigin(c)
        def atomuu(c: Cls): AtomUU = _SquashU3(from.pathPairT(c), pwrap31(pred)).pipe(_AssertO3) }

      // ---------------------------------------------------------------------------
      case class AssertDataU4[I1, I2, I3, I4](from: TtqKPath4, pred: (I1, I2, I3, I4) => Boolean) extends ActionUUc with IdentityM1 {
        def  vldt (c: Cls): Errs   = from.vldtAsOrigin(c)
        def atomuu(c: Cls): AtomUU = _SquashU4(from.pathPairT(c), pwrap41(pred)).pipe(_AssertO4) }

      // ---------------------------------------------------------------------------
      case class AssertDataU5[I1, I2, I3, I4, I5](from: TtqKPath5, pred: (I1, I2, I3, I4, I5) => Boolean) extends ActionUUc with IdentityM1 {
        def  vldt (c: Cls): Errs   = from.vldtAsOrigin(c)
        def atomuu(c: Cls): AtomUU = _SquashU5(from.pathPairT(c), pwrap51(pred)).pipe(_AssertO5) }
}

// ===========================================================================
