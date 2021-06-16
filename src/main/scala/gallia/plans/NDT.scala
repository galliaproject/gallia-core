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
    
    // ===========================================================================
    def anyToO (value: Any): O = O(          value.asInstanceOf[    Obj ])
    def anyToZ (value: Any): Z = Z(Objs.from(value.asInstanceOf[Seq[Obj]]))
    
    // ---------------------------------------------------------------------------
    def anyToO_(value: Any): Option[O] = value.asInstanceOf[Option[    Obj ]]               .map(O.apply)
    def anyToZ_(value: Any): Option[Z] = value.asInstanceOf[Option[Seq[Obj]]].map(Objs.from).map(Z.apply)    
  }

// ===========================================================================
