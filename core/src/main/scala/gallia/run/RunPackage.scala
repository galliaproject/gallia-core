package gallia

// ===========================================================================
package object run {
  type MetaErrorResult = IntermediateMetaPlan

  // ---------------------------------------------------------------------------
  type    NodeId         = gallia.dag.   NodeId
  type HasNodeId         = gallia.dag.HasNodeId
  type HasNodeContext[T] = gallia.dag.HasNodeContext[T]
}

// ===========================================================================