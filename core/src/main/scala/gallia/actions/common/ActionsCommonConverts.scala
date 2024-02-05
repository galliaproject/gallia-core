package gallia
package actions
package common

import selection.typed.TqRPathz
import atoms.common.AtomsCommonConverts._
import atoms.AtomsAsserts._

// ===========================================================================
object ActionsCommonConverts {
  import utils.ActionsUtils._

  // ---------------------------------------------------------------------------
  sealed trait ConverBase {
    val target: TqRPathz

    // ---------------------------------------------------------------------------
    final def vldtAsOrigin(c: Cls): Errs   = target.vldtAsOrigin(c)
    final def rpathz      (c: Cls): RPathz = target.resolve(c)

    // ---------------------------------------------------------------------------
    final def foldRpathz     (c: Cls)(f: (Cls, RPath) => Cls):     Cls  = target.resolve(c).foldLeft(c)(f)
    final def soleSubInfo    (c: Cls)(f: SubInfo => SubInfo) :     Cls  = target.resolve(c).foldLeft(c) { _.transformSoleSubInfo(_)(f) }
    final def soleSubInfoOswo(c: Cls)(f: SubInfo => SubInfo) : Seq[Cls] = rpathz(c).intraClss(c) { _.transformSoleSubInfo(_)(f) } }

  // ===========================================================================
  case class ConvertToString(target: TqRPathz) extends ActionUU1N with ConverBase {
    def vldt   (c: Cls): Errs    = vldtAsOrigin(c) // TODO: validate can reasonably formatted to a string
    def _meta  (c: Cls): Cls     = soleSubInfo (c)(_.copy(valueType = BasicType._String))
    def atomuus(c: Cls): AtomUUs = rpathz        (c).pipe(_atoms(c)(_ConvertToString)) }

  // ---------------------------------------------------------------------------
  case class ConvertToEnum(target: TqRPathz, values: Seq[EnumValue]) extends ActionUU1N with ConverBase {
    def vldt   (c: Cls): Errs    = vldtAsOrigin(c) // TODO: validate can reasonably formatted to a string
      .orIfEmpty { _vldt.checkAreValidEnumValues(values).toSeq }
    def _meta  (c: Cls): Cls     = soleSubInfo (c)(_.copy(valueType = BasicType._Enm(values)))
    def atomuus(c: Cls): AtomUUs = rpathz(c).pipe(_atoms(c)(_ConvertToEnum)) } // TODO: optim: only if not String field already

  // ---------------------------------------------------------------------------
  case class ConvertToInt(target: TqRPathz) extends ActionUU1N with ConverBase {
    def vldt   (c: Cls): Errs    = vldtAsOrigin(c)
    def _meta  (c: Cls): Cls     = soleSubInfo (c)(_.toInt)
    def atomuus(c: Cls): AtomUUs = rpathz(c).pipe(_atoms(c)(_ConvertToInt.apply)) }

  // ---------------------------------------------------------------------------
  case class ConvertToDouble(target: TqRPathz) extends ActionUU1Noswo with ConverBase {
    def vldt   (c: Cls): Errs    = vldtAsOrigin(c)
    def _meta  (c: Cls): Cls     = soleSubInfoOswo(c)(_.toDouble)
.pipe(storeIntraMetas)
    def atomuus(c: Cls): AtomUUs = rpathz(c).pipe(_atoms(c)(_ConvertToDouble.apply))
.pipe(updateAtomMetas) }

  // ---------------------------------------------------------------------------
  case class ConvertToFlag[T: WTT](target: TqRPathz, trueValue: T, strict: Boolean) extends ActionUU1N with ConverBase {
    def vldt   (c: Cls): Errs    = vldtAsOrigin(c)
    def _meta  (c: Cls): Cls     = foldRpathz  (c) { _.updateInfo(_, Info.optBoolean) } // TODO: t210108114447 - support own "flag" type?
    def atomuus(c: Cls): AtomUUs = rpathz(c).pipe(_atoms(c)(_ConvertToFlag(_, trueValue, strict))) }

  // ---------------------------------------------------------------------------
  case class ConvertToBoolean[T: WTT](target: TqRPathz, trueValue: T, falseValue: T) extends ActionUU1N with ConverBase {
    def vldt   (c: Cls): Errs    = vldtAsOrigin(c)
    def _meta  (c: Cls): Cls     = soleSubInfo (c)(_.toBoolean)
    def atomuus(c: Cls): AtomUUs = rpathz(c).pipe(_atoms(c)(_ConvertToBoolean(_, trueValue, falseValue))) }

  // ---------------------------------------------------------------------------
  case class ConvertToOptionalBoolean[T: WTT](target: TqRPathz, trueValue: T, falseValue: T, nullValue: T) extends ActionUU1N with ConverBase {
    def vldt   (c: Cls): Errs    = vldtAsOrigin(c)
    def _meta  (c: Cls): Cls     = foldRpathz  (c) { _.updateInfo(_, Info.optBoolean) }
    def atomuus(c: Cls): AtomUUs = rpathz(c).pipe(_atoms(c)(_ConvertToOptionalBoolean(_, trueValue, falseValue, nullValue))) }

  // ===========================================================================
  case class ToOptional(targets: TqRPathz, strict: Boolean) extends ActionUU0N with IdentityUU0N {
    def  vldt(c: Cls): Errs = targets.vldtAsOrigin(c) ++ (if (!strict) Nil else
                              targets.resolve(c).fromz.pipe(_vldt.checkRequired(c, _)))
    def _meta(c: Cls): Cls  = targets.resolve(c).foldLeft(c)(_ toOptional _) }

  // ---------------------------------------------------------------------------
  case class ToRequired(targets: TqRPathz, strict: Boolean) extends ActionUU11 {
    def  vldt (c: Cls): Errs   = targets.vldtAsOrigin(c) ++ (if (!strict) Nil else
                                 targets.resolve(c).fromz.pipe(_vldt.checkNonRequired(c, _)))
    def _meta (c: Cls): Cls    = targets.resolve(c).foldLeft(c)(_ toRequired _)
    def atomuu(c: Cls): AtomUU = targets.resolve(c).fromz.pipe(_AssertIsDefined) }

  // ---------------------------------------------------------------------------
  case class ToSingle(targets: TqRPathz, strict: Boolean) extends ActionUU1N {
    def  vldt  (c: Cls): Errs    = targets.vldtAsOrigin(c) ++ (if (!strict) Nil else
                                   targets.resolve(c).fromz.pipe(_vldt.checkMultiple(c, _)))
    def _meta  (c: Cls): Cls     = targets.resolve(c).foldLeft(c)(_ toSingle _)
    def atomuus(c: Cls): AtomUUs =
      if (targets.isRequired(c)) targets.resolve(c).pipe(_atoms(_, _ForceOneA))
      else                       targets.resolve(c).pipe(_atoms(_, _ForceOneB)) }

  // ---------------------------------------------------------------------------
  case class ToMultiple(targets: TqRPathz, strict: Boolean) extends ActionUU1N {
    def  vldt  (c: Cls): Errs    = targets.vldtAsOrigin(c) ++ (if (!strict) Nil else
                                   targets.resolve(c).fromz.pipe(_vldt.checkNonMultiple(c, _)))
    def _meta  (c: Cls): Cls     = targets.resolve(c).foldLeft(c)(_ toMultiple _)
    def atomuus(c: Cls): AtomUUs =
      if (targets.isRequired(c)) targets.resolve(c).pipe(_atoms(_, _ForceSeqA))
      else                       targets.resolve(c).pipe(_atoms(_, _ForceSeqB)) }

}

// ===========================================================================
