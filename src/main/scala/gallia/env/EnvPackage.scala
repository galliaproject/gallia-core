package gallia

import gallia.dag._

// ===========================================================================
package object env {
  type Node      = ActionVMN
  type NodePair  = (NodeId, Node)
  type ActionDag = DAG[NodePair]

  // ===========================================================================
  val Global = new Env(idPrefix = "")
}

// ===========================================================================