package gallia

import aptus.IdValue

// ===========================================================================
package object dag {
  trait HasNodeId { /*protected - TODO: use GalliaDAG now to make it protected */val id: NodeId }

  // ---------------------------------------------------------------------------
  trait HasNodeContext[$Ctx] { protected val ctxOpt: Option[$Ctx] }

    // ---------------------------------------------------------------------------
    trait HasNoNodeContext
      extends HasNodeContext[Nothing] {
        final override protected val ctxOpt = None }

  // ---------------------------------------------------------------------------
  trait HasNodeTarget[$NodeType] { protected val target: $NodeType }

  // ===========================================================================
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
  type IdChain = Chain[NodeId] }

// ===========================================================================
