package gallia.dag

// ===========================================================================
private object AfferentSubGraphExtractor {

  def apply[$NodeType](dag: DAG[$NodeType])(id: NodeId): DAG[$NodeType] = {
    val node = dag.lookup(id)

    dag
      ._afferents
      .get(id)
       match {
        case None          => DAG.trivial(dag.idResolver)(node)
        case Some(parents) =>
          parents
            .map { parent =>
              apply(dag)(parent) // recursive call
                .appendNode(parent -> node) }
            .reduceLeft(_ mergeBlindly _)
      }
  }

}

// ===========================================================================
