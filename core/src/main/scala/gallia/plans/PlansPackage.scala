package gallia

import aptus.Seq_

// ===========================================================================
package object plans {
  case class Clss(values: Seq[Cls]) { // eg join
    def isEmpty  :  Boolean   = values.isEmpty
    def forceOne :  Cls       = values.force.one
    def forcePair: (Cls, Cls) = values.force.tuple2 }

  // ---------------------------------------------------------------------------
  case class ClsS(values: Seq[Cls]) // eg intermediate steps

  // ---------------------------------------------------------------------------
  case class ClsIO(in: Cls, out: Cls)

  // ===========================================================================
  type DAG[T] = dag.DAG[T]
  type NodeId = dag.NodeId

  // ---------------------------------------------------------------------------
  type ActionMetaDag = dag.DAG[env.ActionMetaNode]

  // ===========================================================================
  case class AtomMetaContext(
    actionId     : NodeId,
    actionMetaCtx: ActionMetaContext)

  // ---------------------------------------------------------------------------
  private[plans] def atomId(id: NodeId, index: Int): NodeId = id + s"-${index}" /* TODO: not if only one? */ }

// ===========================================================================
