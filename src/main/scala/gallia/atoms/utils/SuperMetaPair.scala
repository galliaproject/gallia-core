package gallia
package atoms
package utils

import scala.reflect.ClassTag
import domain.SortingPair
import data.single.ObjOrdering

// ===========================================================================
case class SuperMetaPair[T]( // TODO: rename...
      ctag: ClassTag[T], // required by Spark RDD for sorting
      ord : Ordering[T])

  // ===========================================================================
  object SuperMetaPair {

    def parse(c: Cls, path: KPath, pair: SortingPair) = {
      val field = c.field(path)

      field
        .forceBasicType /* FIXME: t220426143741 */
        .superPair(
          field.info.container1,
          pair.descending,
          pair.missingLast)
    }

    // ---------------------------------------------------------------------------
    def optionObjSuperMetaPair(c: Cls, pair: domain.SortingPair): SuperMetaPair[Option[Obj]] =
      SuperMetaPair(
        ctag = scala.reflect.classTag[Option[Obj]],
        ord  = ObjOrdering.optionObjOrdering(c, pair))
  }

// ===========================================================================
