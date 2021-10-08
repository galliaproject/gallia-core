package gallia
package dag

import aptus.Seq_

// ===========================================================================
object DAGIn {

  def idDag(nodes: Seq[DagTmp]*): IdDAG =
    nodes
      .flatMap {
        _ .sliding(2)
          .toSeq
          .map(_.force.tuple2)
          .flatMap { case (a, b) =>
            a.expand(b) } }
      .pipe(DAG.from(_:_*))

  // ---------------------------------------------------------------------------
  def idDag0(nodes: Seq[NodeId]*): IdDAG = // TODO: still needed?
    nodes
      .flatMap {
        _ .sliding(2)
          .toSeq
          .map(_.force.tuple2) }
      .pipe(DAG.from(_:_*))

  // ---------------------------------------------------------------------------
  def chain(nodes: Seq[NodeId]): IdDAG =
    nodes
      .sliding(2)
      .toSeq
      .map(_.force.tuple2)
      .pipe(DAG.from)

  // ===========================================================================
  case class DagTmp private (values: Seq[NodeId]) {

      def expand(that: DagTmp): Seq[(NodeId, NodeId)] = {
        require(this.values.size == 1 || that.values.size == 1)

        for (
            v1 <- this.values;
            v2 <- that.values)
          yield (v1 -> v2)
      }

    }

    // ===========================================================================
    object DagTmp {
      implicit def toDagTmp1(x: Seq[NodeId]): DagTmp = DagTmp(x)
        implicit def toDagTmp1(x:  NodeId                                 ): DagTmp = DagTmp(Seq(x))
        implicit def toDagTmp1(x: (NodeId, NodeId                        )): DagTmp = DagTmp(Seq(x._1, x._2))
        implicit def toDagTmp1(x: (NodeId, NodeId, NodeId                )): DagTmp = DagTmp(Seq(x._1, x._2, x._3))
        implicit def toDagTmp1(x: (NodeId, NodeId, NodeId, NodeId        )): DagTmp = DagTmp(Seq(x._1, x._2, x._3, x._4))
        implicit def toDagTmp1(x: (NodeId, NodeId, NodeId, NodeId, NodeId)): DagTmp = DagTmp(Seq(x._1, x._2, x._3, x._4, x._5))

      implicit def toDagTmp2(x: Seq[Symbol]): DagTmp = DagTmp(x.map(_.name))
        implicit def toDagTmp2(x:  Symbol                                 ): DagTmp = toDagTmp2(Seq(x))
        implicit def toDagTmp2(x: (Symbol, Symbol                        )): DagTmp = toDagTmp2(Seq(x._1, x._2))
        implicit def toDagTmp2(x: (Symbol, Symbol, Symbol                )): DagTmp = toDagTmp2(Seq(x._1, x._2, x._3))
        implicit def toDagTmp2(x: (Symbol, Symbol, Symbol, Symbol        )): DagTmp = toDagTmp2(Seq(x._1, x._2, x._3, x._4))
        implicit def toDagTmp2(x: (Symbol, Symbol, Symbol, Symbol, Symbol)): DagTmp = toDagTmp2(Seq(x._1, x._2, x._3, x._4, x._5))
    }

}

// ===========================================================================
