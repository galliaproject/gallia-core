package gallia
package actions

import target._
import FunctionWrappers._
import meta.InfosLike
import atoms.AtomsUV._
import atoms.AtomsAsserts._

// ===========================================================================
object ActionsAsserts {

  case class AssertDataClass(node: TypeNode) extends ActionUUa with IdentityM1 with IdentityUUa {
      def vldt (c: Cls): Errs = node.leaf.dataClassEither match {
        case Left (errorDetails)   => _Error.InvalidDataClass(errorDetails).errs
        case Right(validDataClass) => _vldt.classCompatibilities(c, validDataClass).toSeq } }

  // ===========================================================================
  case class AssertMeta(pred: Cls => Boolean) extends IdentityM1 with IdentityUUa {
        def vldt (c: Cls): Errs =
          _Error.MetaAssertionFailure.errsIf(test = !pred(c)) }

    // ---------------------------------------------------------------------------
    case class AssertField(target: KPath, _error: _Error, pred: InfosLike => Boolean) extends IdentityM1 with IdentityUUa {
          def vldt (c: Cls): Errs =
            _error.errsIf(test = !c.field_(target).exists(pred)) }

      // ---------------------------------------------------------------------------
      // TODO: as predicate of Container rather
      case class AssertContainer(target: KPath, container: Container) extends IdentityM1 with IdentityUUa {
          def vldt (c: Cls): Errs =
            _Error.ContainerAssertionFailure(target, container).errsIf(
                !c.field_(target).exists(
                    _.isContainer(container))) }

      // ---------------------------------------------------------------------------
      //TODO: as predicate of rather
      case class AssertBasicType(target: KPath, basicType: BasicType) extends IdentityM1 with IdentityUUa {
            def vldt (c: Cls): Errs =
              _Error.ContaineeAssertionFailure(target, basicType).errsIf(
                  !c.field_(target).exists(
                      _.info1.isBasicType(basicType))) }

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

}

// ===========================================================================
