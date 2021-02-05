package gallia.actions

import aptus.Anything_

import gallia._
import gallia.selection.typed.TqRPathz
import gallia.atoms.AtomsUUConverts._
import gallia.atoms.AtomsAsserts._

// ===========================================================================
object ActionsUUConverts {
  import utils.ActionsUUUtils._

  // ---------------------------------------------------------------------------
  case class ConvertToString(target: TqRPathz) extends ActionUUb with TodoV1 { // TODO: validate can reasonably formatted to a string
      def _meta  (c: Cls): Cls     = target.resolve(c).foldLeft(c)(_.toStr(_))
      def atomuus(c: Cls): AtomUUs = target.resolve(c).thn(_atoms(c)(_ConvertToString)) }

  // ---------------------------------------------------------------------------
  case class ConvertToInt(target: TqRPathz) extends ActionUUb with TodoV1 {
    def _meta  (c: Cls): Cls     = target.resolve(c).foldLeft(c)(_.toInt(_))
    def atomuus(c: Cls): AtomUUs = target.resolve(c).thn(_atoms(c)(_ConvertToInt)) }

  // ---------------------------------------------------------------------------
  case class ConvertToDouble(target: TqRPathz) extends ActionUUb with TodoV1 {
    def _meta  (c: Cls): Cls     = target.resolve(c).foldLeft(c)(_.toDouble(_))
    def atomuus(c: Cls): AtomUUs = target.resolve(c).thn(_atoms(c)(_ConvertToDouble)) }

  // ---------------------------------------------------------------------------
  case class ConvertToFlag[T: WTT](target: TqRPathz, trueValue: T, strict: Boolean) extends ActionUUb with TodoV1 {
    def _meta  (c: Cls): Cls     = target.resolve(c).foldLeft(c)(_.toOptionalBoolean(_)) // TODO: t210108114447 - support own "flag" type?
    def atomuus(c: Cls): AtomUUs = target.resolve(c).thn(_atoms(c)(_ConvertToFlag(_, trueValue, strict))) }

  // ---------------------------------------------------------------------------
  case class ConvertToBoolean[T: WTT](target: TqRPathz, trueValue: T, falseValue: T) extends ActionUUb with TodoV1 {
    def _meta  (c: Cls): Cls     = target.resolve(c).foldLeft(c)(_.toBoolean(_))
    def atomuus(c: Cls): AtomUUs = target.resolve(c).thn(_atoms(c)(_ConvertToBoolean(_, trueValue, falseValue))) }

  // ---------------------------------------------------------------------------
  case class ConvertToOptionalBoolean[T: WTT](target: TqRPathz, trueValue: T, falseValue: T, nullValue: T) extends ActionUUb with TodoV1 {
    def _meta  (c: Cls): Cls  = target.resolve(c).foldLeft(c)(_.toOptionalBoolean(_))
    def atomuus(c: Cls): AtomUUs = target.resolve(c).thn(_atoms(c)(_ConvertToOptionalBoolean(_, trueValue, falseValue, nullValue))) }

  // ===========================================================================
  case class ToNonRequired(targets: TqRPathz, strict: Boolean) extends ActionUUa with IdentityUUa {
    def  vldt(c: Cls): Errs = targets.vldtAsOrigin(c) ++ (if (!strict) Nil else
                              targets.resolve(c).fromz.thn(_vldt.checkRequired(c, _)))
    def _meta(c: Cls): Cls  = targets.resolve(c).foldLeft(c)(_ toNonRequired _) }

  // ---------------------------------------------------------------------------
  case class ToRequired(targets: TqRPathz, strict: Boolean) extends ActionUUc {
    def  vldt (c: Cls): Errs   = targets.vldtAsOrigin(c) ++ (if (!strict) Nil else
                                 targets.resolve(c).fromz.thn(_vldt.checkNonRequired(c, _)))
    def _meta (c: Cls): Cls    = targets.resolve(c).foldLeft(c)(_ toRequired _)
    def atomuu(c: Cls): AtomUU = targets.resolve(c).fromz.thn(_AssertIsDefined) }

  // ---------------------------------------------------------------------------
  case class ToNonMultiple(targets: TqRPathz, strict: Boolean) extends ActionUUb {
    def  vldt  (c: Cls): Errs    = targets.vldtAsOrigin(c) ++ (if (!strict) Nil else
                                   targets.resolve(c).fromz.thn(_vldt.checkMultiple(c, _)))
    def _meta  (c: Cls): Cls     = targets.resolve(c).foldLeft(c)(_ toNonMultiple _)
    def atomuus(c: Cls): AtomUUs =
      if (targets.isRequired(c)) targets.resolve(c).thn(_atoms(_, _ForceOneA))
      else                       targets.resolve(c).thn(_atoms(_, _ForceOneB)) }

  // ---------------------------------------------------------------------------
  case class ToMultiple(targets: TqRPathz, strict: Boolean) extends ActionUUb {
    def  vldt  (c: Cls): Errs    = targets.vldtAsOrigin(c) ++ (if (!strict) Nil else
                                   targets.resolve(c).fromz.thn(_vldt.checkNonMultiple(c, _)))
    def _meta  (c: Cls): Cls     = targets.resolve(c).foldLeft(c)(_ toMultiple _)
    def atomuus(c: Cls): AtomUUs =
      if (targets.isRequired(c)) targets.resolve(c).thn(_atoms(_, _ForceSeqA))
      else                       targets.resolve(c).thn(_atoms(_, _ForceSeqB)) }

}

// ===========================================================================
