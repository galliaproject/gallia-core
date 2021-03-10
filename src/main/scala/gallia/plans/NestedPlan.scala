package gallia.plans

import aptus.Anything_

import gallia._
import gallia.FunctionWrappers._

// ===========================================================================
class NestedPlan(parent: AtomPlan) extends Serializable {
  import parent._

  def u2u(optional: Boolean): _ff11 =
    if (optional) _naiveRunU2U_ _
    else          _naiveRunU2U  _

  def z2z(optional: Boolean): _ff11 =
    if (optional) _naiveRunZZ_ _
    else          _naiveRunZZ  _

  // ---------------------------------------------------------------------------
  def u2z(optional: Boolean): _ff11 =
    if (optional) _naiveRunUZ_ _
    else          _naiveRunUZ  _

  def z2u(optional: Boolean): _ff11 =
    if (optional) _naiveRunZU_ _
    else          _naiveRunZU  _

  // ---------------------------------------------------------------------------
  def u2v(optional: Boolean): _ff11 =
      if (optional) _naiveRunUV_ _
      else          _naiveRunUV  _

  def z2v(optional: Boolean): _ff11 =
      if (optional) _naiveRunZV_ _
      else          _naiveRunZV  _

  def uu2u(optional1: Boolean, optional2: Boolean): _ff21 =
      if (optional1 || optional2) ??? // FIXME
      else                        _naiveRunUu2U _

  // ===========================================================================
  private def _naiveRunU2U (missingInput : Any): Any = naiveRunUU (missingInput .asInstanceOf[       Obj ])
  private def _naiveRunU2U_(missingInput : Any): Any = naiveRunUU_(missingInput .asInstanceOf[Option[Obj]])

  private def _naiveRunUV (missingInput: Any): Any = naiveRunUV (missingInput.asInstanceOf[       Obj ])
  private def _naiveRunUV_(missingInput: Any): Any = naiveRunUV_(missingInput.asInstanceOf[Option[Obj]])

  private def _naiveRunZV (missingInput: Any): Any = naiveRunZV (missingInput.asInstanceOf[       Seq[Obj] ].thn(Objs.from))
  private def _naiveRunZV_(missingInput: Any): Any = naiveRunZV_(missingInput.asInstanceOf[Option[Seq[Obj]]].map(Objs.from))

  private def _naiveRunZZ (missingInput: Any): Any = naiveRunZZ (missingInput.asInstanceOf[       Seq[Obj] ].thn(Objs.from))      .toListAndTrash
  private def _naiveRunZZ_(missingInput: Any): Any = naiveRunZZ_(missingInput.asInstanceOf[Option[Seq[Obj]]].map(Objs.from)).map(_.toListAndTrash)

  private def _naiveRunUZ_(missingInput: Any): Any = naiveRunUZ_(missingInput.asInstanceOf[Option[Obj]]).toList
  private def _naiveRunUZ (missingInput: Any): Any = naiveRunUZ (missingInput.asInstanceOf[       Obj ]).toListAndTrash

  private def _naiveRunZU_(missingInput: Any): Any = naiveRunZU_(missingInput.asInstanceOf[Option[Seq[Obj]]].map(Objs.from))
  private def _naiveRunZU (missingInput: Any): Any = naiveRunZU (missingInput.asInstanceOf[       Seq[Obj] ].thn(Objs.from))

  private def _naiveRunUu2U(missingInput1: Any, missingInput2: Any): Any = naiveRunUu2U(missingInput1.asInstanceOf[Obj], missingInput2.asInstanceOf[Obj])
}

// ===========================================================================
