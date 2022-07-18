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
    def vle2 : (Vle , Vle ) = illegalState("220209122414")

    def objsVle: (Objs , Vle ) = illegalState("220209122414")
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
      override def vle : Vle  = value.forceVle }

    // ---------------------------------------------------------------------------
    case class  SingleForkInput(value: NDT) extends DataInput {
      override def obj : Obj  = value.forceO
      override def objs: Objs = value.forceZ
        .pipe { x =>
          if (!x.isIteratorBased) x
          else                    x.values.asInstanceOfIteratorStreamer.fork.pipe(Objs.build) }
      override def vle : Vle  = value.forceVle }

    // ---------------------------------------------------------------------------
    case class  MultipleInputs(values: Seq[NDT]) extends DataInput {
      override def obj2   : (Obj , Obj ) = values.force.tuple2.mapAll(_.forceO  , _.forceO)
      override def objs2  : (Objs, Objs) = values.force.tuple2.mapAll(_.forceZ  , _.forceZ)
      override def vle2   : (Vle , Vle ) = values.force.tuple2.mapAll(_.forceVle, _.forceVle)
      override def objsVle: (Objs, Vle ) = values.force.tuple2.mapAll(_.forceZ  , _.forceVle) }
  }

// ===========================================================================
