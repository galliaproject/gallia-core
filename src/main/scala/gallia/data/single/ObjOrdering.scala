package gallia
package data.single

import reflect.BasicType
import domain.PathPair
import domain.SortingPair

// ===========================================================================
object ObjOrdering {

  def optionObjOrdering(c: Cls, pair: SortingPair): Ordering[Option[Obj]] = {
    implicit val ord = objOrdering(c, pair)
    implicitly[Ordering[Option[Obj]]]
  }

  // ===========================================================================
  def objOrdering(c: Cls, pair: SortingPair): Ordering[Obj] =
    new Ordering[Obj] {

      def compare(left: Obj, right: Obj): Int = // TODO: worth trying to optimize? (eg trivial cases, ...)
          //TODO: just use Iterable's?
          c .fields
              .view
              .map(compareField(left, right))
              .find(_ != 0) // TODO: comfirm lazy
              .getOrElse(0)

       // ---------------------------------------------------------------------------
       def compareField(left: Obj, right: Obj)(field: Fld): Int = {
            val key      = field.key
            val optional = field.isOptional
            
            field
              .ofni
              .infos
              .view
              .map(compareInfo(key, optional)(left, right))
              .find(_ != 0)        
              .getOrElse(0)
          }

          // ---------------------------------------------------------------------------
          private def compareInfo(key: Key, optional: Boolean)(left: Obj, right: Obj)(info: meta.Info): Int =                 
            info.containee match {
  
              // ---------------------------------------------------------------------------
              case tipe: BasicType =>
                val ori = PathPair(KPath.from(key), optional)
                val container = reflect.Container.from(optional, info.multiple)

                tipe.compare(container, pair.descending, pair.missingLast)(
                    ori.lookup(left),
                    ori.lookup(right))
  
              // ---------------------------------------------------------------------------
              case c: Cls => ??? // FIXME: recurse
            }
  }

}

// ===========================================================================
