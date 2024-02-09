package gallia
package env

import dag._

// ===========================================================================
class HandlerHelper() {

  private[gallia] def start(input: ActionVM0): RootId = {
    val dagId  = Env.nextDagId()
    val rootId = Env.nextNodeId()
    Env.associateNode(rootId -> dagId)

    val dag: plans.ActionMetaDag = DAG.trivial[ActionMetaNode](_.id)(ActionMetaNode(rootId, input))
    Env.associateDag(dagId -> dag)

    rootId }

  // ---------------------------------------------------------------------------
  private[gallia] def chain(nodeId: NodeId, action: ActionVMN): NodeId = {
    val (dagId, originalDag) = Env.retrieveDagPair(nodeId)

    val newNodeId = Env.nextNodeId()
    val updatedDag = originalDag.appendNode(nodeId -> ActionMetaNode(newNodeId, action))

    Env.associateNode(newNodeId -> dagId)
    Env.associateDag(dagId -> updatedDag)

    newNodeId }

  // ---------------------------------------------------------------------------
  private[gallia] def join2(thisNodeId: NodeId, thatNodeId: NodeId)(action: ActionVMN): LeafId = {
    val newDagId  = Env.nextDagId()
    val newNodeId = Env.nextNodeId()

    val (originalDagId1, originalDag1) = Env.retrieveDagPair(thisNodeId)
    val (originalDagId2, originalDag2) = Env.retrieveDagPair(thatNodeId)

    val newDag =
      originalDag1.appendNode(thisNodeId -> ActionMetaNode(newNodeId, action))
        .mergeBlindly(
      originalDag2.appendNode(thatNodeId -> ActionMetaNode(newNodeId, action)) )

    Env.associateDag(newDagId -> newDag)
    Env.dissociateDag(originalDagId1)
    Env.dissociateDag(originalDagId2)

    newDag
      .nodeIds
      .foreach { nodeId =>
        // TODO: differentiate reassociate?
        Env.associateNode(nodeId -> newDagId) }

    newNodeId }

  // ===========================================================================
  def updateAs(nodeId: NodeId, key: Key) = { // TODO: t210116192032 - generalize
      val (dagId, dag) = Env.retrieveDagPair(nodeId)

      val updatedDag: plans.ActionMetaDag =
        dag.transformNodeTypeConditionally(_ == nodeId) {
          _.tranform {
            _ .asInstanceOf[actions.CanForceAs1[_]] /* by design */
              .forceAs(key)
              .asInstanceOf[ActionVMN] /* by design */ } }

      Env.associateDag(dagId -> updatedDag)
      
      () } }

// ===========================================================================
