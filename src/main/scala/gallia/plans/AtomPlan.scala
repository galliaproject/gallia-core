package gallia.plans

import aptus.{Anything_, Seq_}

import gallia._
import gallia.dag._

// ===========================================================================
case class AtomPlan(dag: DAG[AtomNode]) {

  override def toString: String = formatDefault
    def formatDefault: String = dag.formatDefault

  // ---------------------------------------------------------------------------
  private def nestingPlaceholderRootIds(dag: DAG[AtomNode]): Seq[RootId] =
    dag.roots.filter(_.atom == NestingDataPlaceholder).map(_.id)

  // ---------------------------------------------------------------------------
  def naiveRun(missingInputs: Map[RootId, NDT] = Map()): NDT = { // TODO: t201027130649 - abstract runner strategy
      val mut = collection.mutable.Map[NodeId, NDT]()
      val proc = new AtomProcessor(missingInputs.apply, mut.apply)

      dag
        .kahnTraverseNodes
        .foreach { node =>
          mut +=
            node.id ->
              proc.process(dag.afferentIds(node.id))(node) }

      // ---------------------------------------------------------------------------
      dag
        .leaveIds
        .force.one // TODO
        .thn(mut.apply) // TODO
    }

    // ===========================================================================
    def naiveRunUU_(missingInput: Option[Obj]): Option[Obj] = missingInput.map(naiveRunUU)
    def naiveRunUU (missingInput:        Obj ): Obj         = naiveRun(Map(nestingPlaceholderRootIds(dag).force.one -> NDT.O(missingInput))).forceO

    def naiveRunZZ_(missingInput: Option[Objs]): Option[Objs] = missingInput.map(naiveRunZZ)
    def naiveRunZZ (missingInput:        Objs ):        Objs  = naiveRun(Map(nestingPlaceholderRootIds(dag).force.one -> NDT.Z(missingInput))).forceZ

    // ---------------------------------------------------------------------------
    def naiveRunUZ_(missingInput: Option[Obj]): Option[Objs] = missingInput.map(naiveRunUZ)
    def naiveRunUZ (missingInput:        Obj ):        Objs  = naiveRun(Map(nestingPlaceholderRootIds(dag).force.one -> NDT.O(missingInput))).forceZ

    def naiveRunZU_(missingInput: Option[Objs]): Option[Obj] = missingInput.map(naiveRunZU)
    def naiveRunZU (missingInput:        Objs ):        Obj  = naiveRun(Map(nestingPlaceholderRootIds(dag).force.one -> NDT.Z(missingInput))).forceO

    // ---------------------------------------------------------------------------
    def naiveRunUV_(missingInput: Option[Obj]):         AnyValue = missingInput.map(naiveRunUV)
    def naiveRunUV (missingInput:        Obj ):         AnyValue = naiveRun(Map(nestingPlaceholderRootIds(dag).force.one -> NDT.O(missingInput))).forceVle

    def naiveRunZV_(missingInput: Option[Objs]): Option[AnyValue] = missingInput.map(naiveRunZV)
    def naiveRunZV (missingInput:        Objs ):        AnyValue  = naiveRun(Map(nestingPlaceholderRootIds(dag).force.one -> NDT.Z(missingInput))).forceVle

    // ---------------------------------------------------------------------------
    def naiveRunUu2U(missingInput1: Obj, missingInput2: Obj): Obj  = // only if rootIds are ordered... TODO: ok?
      nestingPlaceholderRootIds(dag)
        .force.tuple2
        .thn { case (rootId1, rootId2) =>
          naiveRun(
              Map(
                  rootId1 -> NDT.O(missingInput1),
                  rootId2 -> NDT.O(missingInput2)))
            .forceO }
}

// ===========================================================================
