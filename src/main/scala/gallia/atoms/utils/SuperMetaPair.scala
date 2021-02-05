package gallia.atoms.utils

import gallia._
import scala.reflect.ClassTag
import gallia.domain.SortingPair

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
