package gallia
package dag

// ===========================================================================
abstract class GalliaDAG[$NodeType <:
           HasNodeId
      with HasNodeContext[$NodeContext]
      with HasNodeTarget [$NodeTarget],
    $NodeContext, $NodeTarget] (
      dag: DAG[$NodeType]) {
  //TODO: t240209122802 - use abstraction to simplify some of the dag.transformX (since we can easily get node ID/ctx now)
}

// ===========================================================================
