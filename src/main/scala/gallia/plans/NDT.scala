package gallia.plans

import gallia._
import gallia.Vle

// ===========================================================================
/** NDT = NodeDataType */
trait NDT {
    def value: Any

    // ---------------------------------------------------------------------------
    def forceVle: Vle   = this.asInstanceOf[NDT.V].value

    def forceT[T]: T    = this.asInstanceOf[NDT.V].value.asInstanceOf[T]
    def forceO   : Obj  = this.asInstanceOf[NDT.O].value
    def forceZ   : Objs = this.asInstanceOf[NDT.Z].value
  }

  // ===========================================================================
  object NDT {
    case class O(value: Obj ) extends NDT
    case class Z(value: Objs) extends NDT
    case class V(value: Any ) extends NDT

    // ===========================================================================
    implicit def to (value: Obj ): NDT = new NDT.O(value)
    implicit def to (value: Objs): NDT = new NDT.Z(value)
             def vle(value: Vle ): NDT = new NDT.V(value) // implicit is tricky here
  }

// ===========================================================================
