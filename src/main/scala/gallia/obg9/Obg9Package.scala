package gallia
package atoms

import aptus._    

// ===========================================================================
/** obg9 is codename for memory-optimized Obj counterpart (not in use yet) - good for dense data */
package object obg9 {
  var hack = new atoms.obg9.TempHack()

  // ===========================================================================
  type Ori  = Array[Any]
  type Dest = Array[Any]

  // ---------------------------------------------------------------------------
  type Index = aptus.Index
  type Size  = aptus.Size

  // ===========================================================================
  import Obg9Extensions._
  implicit def _Option9_[T](x: Option[T]) : Option9_  [T] = new Option9_[T](x)
  implicit def _ArrayAny9_ (x: Array[Any]): ArrayAny9_    = new ArrayAny9_(x)

  // ---------------------------------------------------------------------------
  implicit def _Cls9_   (x: Cls)   : Cls9_    = new Cls9_   (x)
  implicit def _Fld9_   (x: Fld)   : Fld9_    = new Fld9_   (x)
  implicit def _KPath9_ (x: KPath) : KPath9_  = new KPath9_ (x)
  implicit def _KPathz9_(x: KPathz): KPathz9_ = new KPathz9_(x)

  // ---------------------------------------------------------------------------
  implicit def _Obj9_   (x: Obj): Obj9_ = new Obj9_(x)

  // ===========================================================================
  // TODO: thread locality - see aptus.aptmisc.ThreadLocalWrapper now
  class TempHack() { // 220413092436
      private var hack: Boolean = false

      // ---------------------------------------------------------------------------
      def temporarily[T](f: => T): T = { hack = true; val result = f; hack = false; result }
    }

    // ---------------------------------------------------------------------------
    object TempHack {
      implicit def _tempHackToBoolean(value: TempHack): Boolean = value.hack
    }

  // ===========================================================================  
  private[obg9] def generic(c1: Cls, c2: Cls)(f: Obg9 => Obg9) = _Generic9(c1, c2, f)

    case class _Generic9(c1: Cls, c2: Cls, f: Obg9 => Obg9) extends AtomUU { def naive(o: Obj) = wrap9(o)(c1, c2)(f) } // for development
  
  // ---------------------------------------------------------------------------
  private def wrap9(in: Obj)(c1: Cls, c2: Cls)(f: Obg9 => Obg9): Obj = {
    val x = Obg9Conversions.fromObj(c1)(in)
    val y = f(x)    
    Obg9Conversions.backToObj(c2)(y)
  }
  
  // ===========================================================================
  case class PPairs(values: Seq[PPair]) { // = indices path    
      def head = values.head
      def tailOpt: Option[PPairs] = values.tail match {
        case Nil => None
        case more => Some(PPairs(more)) }
      
      // ---------------------------------------------------------------------------
      def prepend(value: PPair): PPairs = PPairs(value +: values)
      def  append(value: PPair): PPairs = PPairs(         values :+ value) }
  
    // ---------------------------------------------------------------------------
    case class PPair(index: Int, optional: Optional, multiple: Multiple)

  // ===========================================================================
  type NonRecursiveReorderingCtx = Seq[Index]
  
  case class RecursiveReordering(values: Seq[RecursiveReorderingItem])
    case class RecursiveReorderingItem(index: Index, nesting: Option[RecursiveReorderingNesting])
      case class RecursiveReorderingNesting(reordering: RecursiveReordering, optional: Optional, multiple: Multiple)

  // ---------------------------------------------------------------------------
  case class RemoveOrRetainWithNestingCtx(newSize: Size, topLevelIndices: Set[Index], nesting: Map[Index, RemoveOrRetainWithNestingSubCtx])  
    case class RemoveOrRetainWithNestingSubCtx(ctx: RemoveOrRetainWithNestingCtx, optional: Optional, multiple: Multiple)
    
  // ===========================================================================
  implicit class Obg9__(o: Obg9) {

    def formatDebug: String = {
      o .data
        .zipWithIndex
        .map { case (value, index) =>
          index + ": " + formatValueDebug(value) }
      .toList
      .joinln
      .sectionAllOff    
    }

    // ---------------------------------------------------------------------------
    private def formatValueDebug(value: Any): Any =
      value match {
          case null        => "null"
          case seq: Seq[_] =>
            seq
              .head
               match {
                case null    => ???//"null" -- can't happen
                case _: Obg9 => seq.map(_.asInstanceOf[Obg9].formatDebug).joinln
                case _       => seq.join("|") }
          case obj: Obg9 => obj.formatDebug
          case sgl       => assert(sgl != None); sgl }

  }

}

// ===========================================================================
