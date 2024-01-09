package gallia
package dag

// ===========================================================================
object GraphUtils {

  /** inspired by https://en.wikipedia.org/w/index.php?title=Topological_sorting&oldid=805893309#Kahn's_algorithm */
  def kahn[A](originalEdges: Seq[(A /* from */, A /* to */)]): Either[Seq[(A, A)], Seq[A]] = {
    import collection.mutable.ListBuffer

    def vertices: Seq[A] =
      originalEdges
        .flatMap(e => Seq(e._1, e._2))
        .distinct

    def roots: Seq[A] =
      vertices
        .filterNot(
            originalEdges
            .map(_._2)
            .toSet
            .contains)

    val list = ListBuffer[A]()

    val set  : ListBuffer[ A    ] = cross.immutableSeqToListBuffer(roots)         // because of 2.12
    val edges: ListBuffer[(A, A)] = cross.immutableSeqToListBuffer(originalEdges) // because of 2.12

    while (set.nonEmpty) {
      val n = set.remove(0)
      list += n

      edges
        .filter(_._1 == n)
        .map { e =>
          val (_, m) = e

          edges -= e

          if (edges.filter(_._2 == m).isEmpty)
            set += m
        }
    }

    if (edges.nonEmpty) Left(edges.toList)
    else                Right(list.toList)
  }

}

// ===========================================================================
