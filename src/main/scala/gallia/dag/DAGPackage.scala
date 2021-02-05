package gallia

import aptus.IdValue

// ===========================================================================
package object dag {
  trait HasNodeId { val id: NodeId }

  type DagId  = IdValue
  type NodeId = IdValue
  type RootId = NodeId
  type LeafId = NodeId

  type AfferentNodeId = NodeId
  type EfferentNodeId = NodeId

  type ForkId = NodeId

  type ParentNodeId = NodeId

  type ChainId = String

  type Edge = (NodeId, NodeId)

  // ---------------------------------------------------------------------------
  type IdDAG   = DAG  [NodeId]
  type IdChain = Chain[NodeId]
}

// ===========================================================================
