package gallia

import dag._

// ===========================================================================
package object env {
  type Node = ActionVN with ActionMN

  // ---------------------------------------------------------------------------
  case class NodePair(id: NodeId, actionvmn: Node) {
    def tranform(f: Node => Node): NodePair = copy(actionvmn = f(actionvmn))

    // ---------------------------------------------------------------------------
    def isNestingMetaPlaceholder: Boolean = actionvmn.isInstanceOf[heads.HeadsNestingHandler.NestingMetaPlaceholder]
    def isActionXO: Boolean =
      actionvmn.isInstanceOf[ActionUO] ||
      actionvmn.isInstanceOf[ActionZO] }

  // ---------------------------------------------------------------------------
  type ActionDag = DAG[NodePair] }

// ===========================================================================
