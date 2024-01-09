package gallia
package atoms
//package obg9 -- FIXME

import aptus._
import gallia.domain.KPaths2

// ===========================================================================
object Obg9Extensions {  
  import obg9.{PPair, PPairs}
  
  // ---------------------------------------------------------------------------
  implicit class Option9_[T](u: Option[T]) {
    def orNull: T = u.getOrElse(null.asInstanceOf[T])
  }

  // ---------------------------------------------------------------------------
  implicit class ArrayAny9_(data: Array[Any]) {
    def value_(index: Index): Any = Option(data(index))
    def value (index: Index): Any = data(index)
    def value (ppair: PPair): Any = if (ppair.optional) Option(data(ppair.index)) else data(ppair.index) 
  }
  
  // ===========================================================================
  implicit class Cls9_(c: Cls) {
    
    def indices(keyz: Keyz): Seq[Index] = keyz.map(c.keyIndex).sorted.distinct      
    
    /*protected - see transform */def keyIndex(key: Key): Int = c.keys.indexOf(key)
    
    def ppair(key: Key): PPair = c.field(key).pipe { f => c.keys.indexOf(key).pipe(f.ppair) }

    // ---------------------------------------------------------------------------    
    def either(path: KPath): Either[Index, PPairs] =
      path.leafOpt match {
        case Some(leaf) => Left(c.keyIndex(leaf))        
        case None       => Right(pathIndices(path)) }

    // ---------------------------------------------------------------------------
    def pathIndices(path: KPath): PPairs =
      path.tailPair match {
        case (leaf  , None      ) => PPairs(Seq(c.ppair(leaf)))
        case (parent, Some(tail)) =>                      
          c .field(parent)
            .forceNestedClass
            .pathIndices(tail)
            .prepend(c.ppair(parent)) }

    // ---------------------------------------------------------------------------
    def pathIndicess(paths: KPaths2) = (pathIndices(paths.path1), pathIndices(paths.path2))
  }

  // ===========================================================================
  implicit class Fld9_(field: Fld) {
    def ppair(index: Index): PPair = PPair(index, field.isOptional, field.subInfo1.isMultiple)
  }
      
  // ---------------------------------------------------------------------------
  implicit class Obj9_(o: Obj) {
    def lookup: Map[Key, Any] = o._data.toMap
  }
  
  // ---------------------------------------------------------------------------
  implicit class KPath9_(path: KPath) {    
  }
  
  // ---------------------------------------------------------------------------
  implicit class KPathz9_(pathz: KPathz) {    
    def topLevelsOnly: Keyz = pathz.flatten(_.leafOpt).pipe(Keyz.apply)               
  }
}

// ===========================================================================
