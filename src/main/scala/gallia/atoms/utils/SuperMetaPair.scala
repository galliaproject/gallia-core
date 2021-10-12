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

    def parse(c: Cls, path: KPath, pair: SortingPair) =
      c .field(path)
        .info
        .superPair(pair)
  }

// ===========================================================================
