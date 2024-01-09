package gallia
package env

import dag._

// ===========================================================================
class HandlerHelper() {

  private[gallia] def start(input: ActionVM0): RootId = {
    val dagId  = Env.nextDagId()
    val rootId = Env.nextNodeId()
    Env.associateNode(rootId -> dagId)

    val dag: ActionDag = DAG.trivial[gallia.env.NodePair](_._1)(rootId -> input)
    Env.associateDag(dagId -> dag)

    rootId
  }

  // ---------------------------------------------------------------------------
  private[gallia] def chain(nodeId: NodeId, action: Node): NodeId = {
    val (dagId, originalDag) = Env.retrieveDagPair(nodeId)

    val newNodeId = Env.nextNodeId()
    val updatedDag = originalDag.appendNode(nodeId -> (newNodeId, action))

    Env.associateNode(newNodeId -> dagId)
    Env.associateDag(dagId -> updatedDag)

    newNodeId
  }

  // ---------------------------------------------------------------------------
  private[gallia] def join2(thisNodeId: NodeId, thatNodeId: NodeId)(action: Node): LeafId = {
    val newDagId  = Env.nextDagId()
    val newNodeId = Env.nextNodeId()

    val (originalDagId1, originalDag1) = Env.retrieveDagPair(thisNodeId)
    val (originalDagId2, originalDag2) = Env.retrieveDagPair(thatNodeId)

    val newDag =
      originalDag1.appendNode(thisNodeId -> (newNodeId, action))
        .mergeBlindly(
      originalDag2.appendNode(thatNodeId -> (newNodeId, action)) )

    Env.associateDag(newDagId -> newDag)
    Env.dissociateDag(originalDagId1)
    Env.dissociateDag(originalDagId2)

    newDag
      .nodeIds
      .foreach { nodeId =>
        // TODO: differentiate reassociate?
        Env.associateNode(nodeId -> newDagId) }

    newNodeId
  }

  // ===========================================================================
  import gallia.actions.CanForceAs1

  // ---------------------------------------------------------------------------
  def updateAs(nodeId: NodeId, key: Key) = { // TODO: t210116192032 - generalize
      val (dagId, dag) = Env.retrieveDagPair(nodeId)

      val updatedDag: ActionDag =
        dag.transformNode[Node](nodeId) { // by design
          _ .asInstanceOf[CanForceAs1[_]]
            .forceAs(key)
            .asInstanceOf[Node] }

      Env.associateDag(dagId -> updatedDag)
      
      ()
    }

}

// ===========================================================================
