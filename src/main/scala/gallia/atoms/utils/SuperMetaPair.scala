package gallia
package atoms.utils

import scala.reflect.ClassTag
import domain.SortingPair

// ===========================================================================
case class SuperMetaPair[T](
      ctag: ClassTag[T],
      ord : Ordering[T])

  // ---------------------------------------------------------------------------
  object SuperMetaPair {

    def parse(c: Cls, path: KPath, pair: SortingPair) = {
      val field = c.field(path)

      field
        .forceBasicType /* FIXME: t220426143741 */
        .superPair(
          field.ofni.container,
          pair.descending,
          pair.missingLast)
    }
  }

// ===========================================================================
