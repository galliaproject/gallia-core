package gallia
package atoms
package utils

import scala.reflect.ClassTag
import domain.{PathPair1, PathPair2, PathPair3, Sorter}
import meta.PNF

// ===========================================================================
object SortWrapping {

  trait SortWrapperN[T] {
      protected def values: Seq[SortWrapper1[_]]

      // ---------------------------------------------------------------------------
      final def pnfs(c: Cls): Seq[PNF] = values.map(_.ori.path).map(c.toPNF)

      // ---------------------------------------------------------------------------
      def meta: SuperMetaPair[T] // for non-spilling
      def data(o: Obj): T        // for non-spilling

      // ---------------------------------------------------------------------------
      def gnuSortFields = spilling.GnuSortFields.from(values) // for spilling
    }

  // ===========================================================================
  case class SortWrapper1[K](
                         ori        : PathPair1,
            override val meta       : SuperMetaPair[K],
                         multiple   : Boolean,
                         numerical  : Boolean,
                         reverse    : Boolean,
                         missingLast: Boolean)
        extends SortWrapperN[K] with spilling.SortWrapper {
      override val values = Seq(this)
      override def data(o: Obj): K = ori.lookup(o).asInstanceOf[K]
    }

  // ===========================================================================
  case class SortWrapper2[K1, K2](value1: SortWrapper1[K1], value2: SortWrapper1[K2]) extends SortWrapperN[(K1, K2)] {
      override val values = Seq(value1, value2)

      // ---------------------------------------------------------------------------
      override def meta = SuperMetaPair(
        implicitly[ClassTag[(K1, K2)]],
        Ordering.Tuple2(value1.meta.ord, value2.meta.ord))

      // ---------------------------------------------------------------------------
      override def data(o: Obj): (K1, K2) = PathPair2(value1.ori, value2.ori).lookup(o).asInstanceOf[(K1, K2)]
    }

  // ===========================================================================
  case class SortWrapper3[K1, K2, K3](value1: SortWrapper1[K1], value2: SortWrapper1[K2], value3: SortWrapper1[K3]) extends SortWrapperN[(K1, K2, K3)] {
      override val values = Seq(value1, value2, value3)

      // ---------------------------------------------------------------------------
      override def meta = SuperMetaPair(
        implicitly[ClassTag[(K1, K2, K3)]],
        Ordering.Tuple3(value1.meta.ord, value2.meta.ord, value3.meta.ord))

      // ---------------------------------------------------------------------------
      override def data(o: Obj): (K1, K2, K3) = PathPair3(value1.ori, value2.ori, value3.ori).lookup(o).asInstanceOf[(K1, K2, K3)]
    }

  // ===========================================================================
  object SortWrapper1 { def from(c: Cls)(sorter : Sorter)                                  : SortWrapper1[_]       =              sorter .sortWrapper(c) }
  object SortWrapper2 { def from(c: Cls)(sorter1: Sorter, sorter2: Sorter)                 : SortWrapper2[_, _]    = SortWrapper2(sorter1.sortWrapper(c), sorter2.sortWrapper(c)) }
  object SortWrapper3 { def from(c: Cls)(sorter1: Sorter, sorter2: Sorter, sorter3: Sorter): SortWrapper3[_, _, _] = SortWrapper3(sorter1.sortWrapper(c), sorter2.sortWrapper(c), sorter3.sortWrapper(c)) }

}

// ===========================================================================