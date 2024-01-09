package gallia
package plans

import FunctionWrappers._

// ===========================================================================
// note: to be phased out
class NestedPlan(parent: AtomPlan) extends Serializable {

  def nestedRunneru2u(optional: Boolean): _ff11 =
    if (optional) _naiveRunU2U_ _
    else          _naiveRunU2U  _

  def nestedRunnerz2z(optional: Boolean): _ff11 =
    if (optional) _naiveRunZZ_ _
    else          _naiveRunZZ  _

  // ---------------------------------------------------------------------------
  def nestedRunneru2z(optional: Boolean): _ff11 =
    if (optional) _naiveRunUZ_ _
    else          _naiveRunUZ  _

  def nestedRunnerz2u(optional: Boolean): _ff11 =
    if (optional) _naiveRunZU_ _
    else          _naiveRunZU  _

  // ---------------------------------------------------------------------------
  def nestedRunneru2v(optional: Boolean): _ff11 =
      if (optional) _naiveRunUV_ _
      else          _naiveRunUV  _

  def nestedRunnerz2v(optional: Boolean): _ff11 =
      if (optional) _naiveRunZV_ _
      else          _naiveRunZV  _

  def nestedRunneruu2u(optional1: Boolean, optional2: Boolean): _ff21 =
      if (optional1 || optional2) ??? // FIXME
      else                        _naiveRunUu2U _

  // ===========================================================================
  private def _naiveRunU2U (missingInput : Any): Any = parent.V1.naiveRunUU (missingInput .asInstanceOf[       Obj ])
  private def _naiveRunU2U_(missingInput : Any): Any = parent.V1.naiveRunUU_(missingInput .asInstanceOf[Option[Obj]])

  private def _naiveRunUV (missingInput: Any): Any = parent.V1.naiveRunUV (missingInput.asInstanceOf[       Obj ])
  private def _naiveRunUV_(missingInput: Any): Any = parent.V1.naiveRunUV_(missingInput.asInstanceOf[Option[Obj]])

  private def _naiveRunZV (missingInput: Any): Any = parent.V1.naiveRunZV (missingInput.asInstanceOf[       List[Obj] ].pipe(Objs.from))
  private def _naiveRunZV_(missingInput: Any): Any = parent.V1.naiveRunZV_(missingInput.asInstanceOf[Option[List[Obj]]].map(Objs.from))

  private def _naiveRunZZ (missingInput: Any): Any = parent.V1.naiveRunZZ (missingInput.asInstanceOf[       List[Obj] ].pipe(Objs.from))      .toListAndTrash
  private def _naiveRunZZ_(missingInput: Any): Any = parent.V1.naiveRunZZ_(missingInput.asInstanceOf[Option[List[Obj]]].map(Objs.from)).map(_.toListAndTrash)

  private def _naiveRunUZ_(missingInput: Any): Any = parent.V1.naiveRunUZ_(missingInput.asInstanceOf[Option[Obj]]).toList
  private def _naiveRunUZ (missingInput: Any): Any = parent.V1.naiveRunUZ (missingInput.asInstanceOf[       Obj ]).toListAndTrash

  private def _naiveRunZU_(missingInput: Any): Any = parent.V1.naiveRunZU_(missingInput.asInstanceOf[Option[List[Obj]]].map(Objs.from))
  private def _naiveRunZU (missingInput: Any): Any = parent.V1.naiveRunZU (missingInput.asInstanceOf[       List[Obj] ].pipe(Objs.from))

  private def _naiveRunUu2U(missingInput1: Any, missingInput2: Any): Any = parent.V1.naiveRunUu2U(missingInput1.asInstanceOf[Obj], missingInput2.asInstanceOf[Obj])
}

// ===========================================================================
