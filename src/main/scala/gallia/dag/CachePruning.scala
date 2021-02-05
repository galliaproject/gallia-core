package gallia.dag

import aptus.{Anything_, Seq_}

// ===========================================================================
private object CachePruning {

  def apply[$NodeType](asg: DAG[$NodeType], cached: NodeId => Boolean): DAG[$NodeType] = {
    val visited = collection.mutable.Set[NodeId]()

    // ---------------------------------------------------------------------------
    def rec(tmpDag: DAG[$NodeType]): DAG[$NodeType] = {
      val currentNodeId =
        tmpDag
          .rootIds
          .filterNot(visited.contains)
          .force.one

      // ---------------------------------------------------------------------------
      visited += currentNodeId

      // ---------------------------------------------------------------------------
      if (cached(currentNodeId)) tmpDag
      else
        asg
          .afferentIds(currentNodeId)
          .foldLeft(tmpDag) { case (currDag, afferentNodeId) =>
            /* if already visited (diamond), then the recursive part has already been handled, we only need the extra edge */
            if (visited.contains(afferentNodeId))
              currDag.addEdge(afferentNodeId -> currentNodeId)
            else
              currDag
                .prependNode(asg.lookup(afferentNodeId), currentNodeId)
                .thn(rec)
          }
    }

    // ---------------------------------------------------------------------------
    asg
      .leaves.force.one
      .thn(DAG.trivial(asg.idResolver))
      .thn(rec)
  }

}

// ===========================================================================
