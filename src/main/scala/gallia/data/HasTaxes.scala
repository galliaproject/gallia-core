package gallia
package data

import aptus._
import reflect._

// ===========================================================================
trait HasTaxes {
  def values: Seq[HasTaxOpt]

  // ---------------------------------------------------------------------------
  def payUp(value: AObj) : Obj  = payUp(value.c)(value.o)
  def payUp(value: AObjs): Objs = payUp(value.c, value.z)

  // ===========================================================================
  def payUp(c: Cls)(o: Obj): Obj  = taxesOpt(c) match {
      case None        => o
      case Some(taxes) => taxes(o) }

  // ---------------------------------------------------------------------------
  def payUp(c: Cls, z: Objs): Objs = taxesOpt(c) match {
      case None        => z
      case Some(taxes) => z.map(taxes) }

  // ===========================================================================
  private def taxesOpt(c: Cls): Option[Obj => Obj] = // TODO: t220405114144 - optimize
    values
      .flatMap  (_.valueOpt(c))
      .in.noneIf(_.isEmpty)
      .map { taxes => taxes.foldLeft(_) { (curr, tax) => tax(curr) } }

  // ===========================================================================
  // TODO: t220427122749 - less opaque, so can guess better when needed
  protected def _tax[T: WTT](c: Cls)(pred: Fld => Boolean)(f: T => Any): Option[Obj => Obj] =
    atoms.utils.ObjModifierCtxParser
      .parse(c)(pred)
      .map(new atoms.utils.ObjModifier[T](_, f))
      .map(_.modify)

    // ---------------------------------------------------------------------------
    abstract class StringTax[T <: HasFieldHasType with HasParseString](t: T) extends HasTaxOpt { final override def valueOpt(c: Cls) = _tax[String](c)(t.has)(t.parseString) }
    abstract class DoubleTax[T <: HasFieldHasType with HasParseDouble](t: T) extends HasTaxOpt { final override def valueOpt(c: Cls) = _tax[Double](c)(t.has)(t.parseDouble) }

    trait StringEnumTax extends HasTaxOpt { final override def valueOpt(c: Cls) = _tax[String](c)(BasicType._Enm.has)(BasicType._Enm.parseString) }
}

// ===========================================================================