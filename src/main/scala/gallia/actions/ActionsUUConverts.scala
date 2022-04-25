package gallia
package actions

import selection.typed.TqRPathz
import atoms.AtomsUUConverts._
import atoms.AtomsAsserts._

// ===========================================================================
object ActionsUUConverts {
  import utils.ActionsUtils._

  // ---------------------------------------------------------------------------
  case class ConvertToString(target: TqRPathz) extends ActionUUb {
      def vldt   (c: Cls): Errs    = target.vldtAsOrigin(c) // TODO: validate can reasonably formatted to a string
      def _meta  (c: Cls): Cls     = target.resolve(c).foldLeft(c) { _.transformSoleInfo(_, _.toStr) }
      def atomuus(c: Cls): AtomUUs = target.resolve(c).pipe(_atoms(c)(_ConvertToString)) }

  // ---------------------------------------------------------------------------
  case class ConvertToInt(target: TqRPathz) extends ActionUUbb {
    def vldt                     (c: Cls): Errs    = target.vldtAsOrigin(c)
    def _meta                    (c: Cls): Cls     = target.resolve(c).foldLeft(c) { _.transformSoleInfo(_, _.toInt) }
    def atomuus(origin: CallSite)(c: Cls): AtomUUs = target.resolve(c).pipe(_atoms(c)(_ConvertToInt(origin))) }

  // ---------------------------------------------------------------------------
  case class ConvertToDouble(target: TqRPathz) extends ActionUUbb {
    def vldt                     (c: Cls): Errs    = target.vldtAsOrigin(c)
    def _meta                    (c: Cls): Cls     = target.resolve(c).foldLeft(c) { _.transformSoleInfo(_, _.toDouble) }
    def atomuus(origin: CallSite)(c: Cls): AtomUUs = target.resolve(c).pipe(_atoms(c)(_ConvertToDouble(origin))) }

  // ---------------------------------------------------------------------------
  case class ConvertToFlag[T: WTT](target: TqRPathz, trueValue: T, strict: Boolean) extends ActionUUbb {
    def vldt                     (c: Cls): Errs    = target.vldtAsOrigin(c)
    def _meta                    (c: Cls): Cls     = target.resolve(c).foldLeft(c) { _.transformSoleInfo(_, _.toOptionalBoolean) } // TODO: t210108114447 - support own "flag" type? 
    def atomuus(origin: CallSite)(c: Cls): AtomUUs = target.resolve(c).pipe(_atoms(c)(_ConvertToFlag(origin)(_, trueValue, strict))) }

  // ---------------------------------------------------------------------------
  case class ConvertToBoolean[T: WTT](target: TqRPathz, trueValue: T, falseValue: T) extends ActionUUbb {
    def vldt                     (c: Cls): Errs    = target.vldtAsOrigin(c)
    def _meta                    (c: Cls): Cls     = target.resolve(c).foldLeft(c) { _.transformSoleInfo(_, _.toBoolean) }
    def atomuus(origin: CallSite)(c: Cls): AtomUUs = target.resolve(c).pipe(_atoms(c)(_ConvertToBoolean(origin)(_, trueValue, falseValue))) }

  // ---------------------------------------------------------------------------
  case class ConvertToOptionalBoolean[T: WTT](target: TqRPathz, trueValue: T, falseValue: T, nullValue: T) extends ActionUUbb {
    def vldt                     (c: Cls): Errs    = target.vldtAsOrigin(c)
    def _meta                    (c: Cls): Cls     = target.resolve(c).foldLeft(c) { _.transformSoleInfo(_, _.toOptionalBoolean) }
    def atomuus(origin: CallSite)(c: Cls): AtomUUs = target.resolve(c).pipe(_atoms(c)(_ConvertToOptionalBoolean(origin)(_, trueValue, falseValue, nullValue))) }

  // ===========================================================================
  case class ToNonRequired(targets: TqRPathz, strict: Boolean) extends ActionUUa with IdentityUUa {
    def  vldt(c: Cls): Errs = targets.vldtAsOrigin(c) ++ (if (!strict) Nil else
                              targets.resolve(c).fromz.pipe(_vldt.checkRequired(c, _)))
    def _meta(c: Cls): Cls  = targets.resolve(c).foldLeft(c)(_ toNonRequired _) }

  // ---------------------------------------------------------------------------
  case class ToRequired(targets: TqRPathz, strict: Boolean) extends ActionUUc {
    def  vldt (c: Cls): Errs   = targets.vldtAsOrigin(c) ++ (if (!strict) Nil else
                                 targets.resolve(c).fromz.pipe(_vldt.checkNonRequired(c, _)))
    def _meta (c: Cls): Cls    = targets.resolve(c).foldLeft(c)(_ toRequired _)
    def atomuu(c: Cls): AtomUU = targets.resolve(c).fromz.pipe(_AssertIsDefined) }

  // ---------------------------------------------------------------------------
  case class ToNonMultiple(targets: TqRPathz, strict: Boolean) extends ActionUUb {
    def  vldt  (c: Cls): Errs    = targets.vldtAsOrigin(c) ++ (if (!strict) Nil else
                                   targets.resolve(c).fromz.pipe(_vldt.checkMultiple(c, _)))
    def _meta  (c: Cls): Cls     = targets.resolve(c).foldLeft(c)(_ toNonMultiple _)
    def atomuus(c: Cls): AtomUUs =
      if (targets.isRequired(c)) targets.resolve(c).pipe(_atoms(_, _ForceOneA))
      else                       targets.resolve(c).pipe(_atoms(_, _ForceOneB)) }

  // ---------------------------------------------------------------------------
  case class ToMultiple(targets: TqRPathz, strict: Boolean) extends ActionUUb {
    def  vldt  (c: Cls): Errs    = targets.vldtAsOrigin(c) ++ (if (!strict) Nil else
                                   targets.resolve(c).fromz.pipe(_vldt.checkNonMultiple(c, _)))
    def _meta  (c: Cls): Cls     = targets.resolve(c).foldLeft(c)(_ toMultiple _)
    def atomuus(c: Cls): AtomUUs =
      if (targets.isRequired(c)) targets.resolve(c).pipe(_atoms(_, _ForceSeqA))
      else                       targets.resolve(c).pipe(_atoms(_, _ForceSeqB)) }

}

// ===========================================================================
