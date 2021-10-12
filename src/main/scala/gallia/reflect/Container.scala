package gallia
package reflect

import enumeratum.{Enum, EnumEntry}

// ===========================================================================
sealed trait Container extends EnumEntry {
    val isOne: Boolean = this == Container._One

    val isMultiple: Boolean
    val isOptional: Boolean

    // ---------------------------------------------------------------------------
    def containerWrap(f: Any => Any): Any => Any =
      this match {
        case Container._One =>                                          f
        case Container._Opt => _.asInstanceOf[Option    [_] ]      .map(f)
        case Container._Nes => _.asInstanceOf[       Seq[_] ]      .map(f)
        case Container._Pes => _.asInstanceOf[Option[Seq[_]]].map(_.map(f)) }
  }

  // ===========================================================================
  object Container extends Enum[Container] {
    val values = findValues

    // ---------------------------------------------------------------------------
    // TODO: change name upon serialization (eg "one" instead of "_One")

    case object _One extends Container { val isOptional: Boolean = false; val isMultiple: Boolean = false }
    case object _Opt extends Container { val isOptional: Boolean = true ; val isMultiple: Boolean = false }
    case object _Nes extends Container { val isOptional: Boolean = false; val isMultiple: Boolean = true  }
    case object _Pes extends Container { val isOptional: Boolean = true ; val isMultiple: Boolean = true  }

    // ---------------------------------------------------------------------------
    /*
      match {
        case Container._One => ???
        case Container._Opt => ???
        case Container._Nes => ???
        case Container._Pes => ???
      }
    */
    
    // ===========================================================================    
    def containerPairOpt(value: Any): Option[(Container, Any)] =
      value match {
        case None    => None // can't distinguish between _Opt and _Pes here
        case Some(x) => x match {
          case y: Seq[_] => Some(Container._Pes -> y.head)
          case y         => Some(Container._Opt -> y) }
        case x: Seq[_] => Some(Container._Nes -> x.head)
        case x         => Some(Container._One -> x) }        

    // ===========================================================================
    def combine(c1: Container, c2: Container): Container =
      c1 match {

        // ---------------------------------------------------------------------------
        case Container._One => c2 match {
          case Container._One => _One
          case Container._Opt => _Opt
          case Container._Nes => _Nes
          case Container._Pes => _Pes
        }

        // ---------------------------------------------------------------------------
        case Container._Opt => c2 match {
          case Container._One => _Opt
          case Container._Opt => _Opt
          case Container._Nes => _Pes
          case Container._Pes => _Pes
        }

        // ---------------------------------------------------------------------------
        case Container._Nes => c2 match {
          case Container._One => _Nes
          case Container._Opt => _Pes
          case Container._Nes => _Nes
          case Container._Pes => _Pes
        }

        // ---------------------------------------------------------------------------
        case Container._Pes => c2 match {
          case Container._One => _Pes
          case Container._Opt => _Pes
          case Container._Nes => _Pes
          case Container._Pes => _Pes
        }
      }
  }

// ===========================================================================