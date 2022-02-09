package gallia
package plans

import aptus._
import enumeratum.{Enum, EnumEntry}

// ===========================================================================
sealed trait DataInput extends EnumEntry {
    def obj  :  Obj         = illegalState("220209122414")
    def objs :  Objs        = illegalState("220209122414")
    def vle  :  Vle         = illegalState("220209122414")

    def obj2 : (Obj , Obj ) = illegalState("220209122414")
    def objs2: (Objs, Objs) = illegalState("220209122414") 
  }
  
  // ===========================================================================
  object DataInput extends Enum[DataInput] {  
    val values = findValues

    // ===========================================================================
    case object NoInput extends DataInput
    
    // ---------------------------------------------------------------------------
    case class  SingleInput(value: NDT) extends DataInput {
      override def obj : Obj  = value.forceO
      override def objs: Objs = value.forceZ
      override def vle : Vle  = value.forceVle      
    }
    
    // ---------------------------------------------------------------------------
    case class  MultipleInputs(values: Seq[NDT]) extends DataInput {
      override def obj2 : (Obj , Obj ) = values.force.tuple2.mapAll(_.forceO, _.forceO)
      override def objs2: (Objs, Objs) = values.force.tuple2.mapAll(_.forceZ, _.forceZ)      
    }
  }

// ===========================================================================
