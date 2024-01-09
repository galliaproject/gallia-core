package gallia

// ===========================================================================
/** convenient to encapsulate custom behavior (as opposed to in-lining it) */
trait CustomTraits {

  trait ClsToCls { def meta(c: Cls): Cls }

  // ---------------------------------------------------------------------------
  trait ObjToObj   extends ClsToCls { def data(o:      Obj ):      Obj  }
  trait ObjsToObjs extends ClsToCls { def data(z: List[Obj]): List[Obj] }
  trait ObjsToObj  extends ClsToCls { def data(z: List[Obj]):      Obj  }
  trait ObjToObjs  extends ClsToCls { def data(o:      Obj ): List[Obj] }

}

// ===========================================================================


