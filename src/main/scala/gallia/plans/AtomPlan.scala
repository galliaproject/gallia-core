package gallia.plans

import scala.util.chaining._
import aptus._

import gallia._
import gallia.dag._

// ===========================================================================
case class AtomPlan(dag: DAG[AtomNode]) {

  override def toString: String = formatSuccinct1
    def formatDefault  : String = dag.formatDefault
    def formatSuccinct1: String = dag.formatDefault2(x => s"${x.id}:${x.atom.formatSuccinct1}")
    
    def formatDot = s"""
      digraph {
        ${dag
          .nodes
          .map { node => s"${node.id.quote} [label=${node.formatSuccinct1.quote}]" }
          .joinln}
      
        ${dag.edges.map{ case (f, t) => s"${f.quote} -> ${t.quote}"}.joinln}
      }"""

  // ---------------------------------------------------------------------------
  private def nestingPlaceholderRootIds(dag: DAG[AtomNode]): Seq[RootId] =
    dag.roots.filter(_.atom == NestingDataPlaceholder).map(_.id)

  // ===========================================================================    
  def naiveRun(missingInputs: Map[RootId, NDT] = Map()): NDT = // TODO: t201027130649 - abstract runner strategy       
       if (dag.isChain) 
         dag
           .nodes // TODO: t210614142629 - confirm/enforce guaranteed topologically sorted if chain?
           .pipe(AtomNodes.apply)
           .pruneChain
           .pipe(ChainDataRun(missingInputs))    
       else
         NaiveGraphDataRun(missingInputs)(dag)

  // ===========================================================================
  @deprecated object V1 { // to be phased out in favor of V2's approach
         
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
        .pipe { case (rootId1, rootId2) =>
          naiveRun(
              Map(
                  rootId1 -> NDT.O(missingInput1),
                  rootId2 -> NDT.O(missingInput2)))
            .forceO }
  }
  
  // ===========================================================================
  object V2 {
    def forceNestingDataPlaceholderRootId: RootId = dag.roots.filter(_.atom == NestingDataPlaceholder).map(_.id).force.one

    // ---------------------------------------------------------------------------
    def naiveRunUU(missingInput: Any): Obj =
      Map(forceNestingDataPlaceholderRootId -> NDT.anyToO(missingInput))
        .pipe(naiveRun)
        .forceO

    // ---------------------------------------------------------------------------
    def naiveRunUU_(missingInput: Any): Option[Obj] =
      NDT.anyToO_(missingInput)
        .map { input =>
          Map(forceNestingDataPlaceholderRootId -> input)
            .pipe(naiveRun)
            .forceO }
    
    // ---------------------------------------------------------------------------      
    def naiveRunZZ(missingInput: Any): Seq[Obj] =
      Map(forceNestingDataPlaceholderRootId -> NDT.anyToZ(missingInput))
        .pipe(naiveRun)
        .forceZ
        .toListAndTrash
    
    // ---------------------------------------------------------------------------
    def naiveRunZZ_(missingInput: Any): Option[Seq[Obj]] =
      NDT.anyToZ_(missingInput)
        .map { input =>
          Map(forceNestingDataPlaceholderRootId -> input)
            .pipe(naiveRun)
            .forceZ
            .toListAndTrash }
  }
}

// ===========================================================================
object AtomPlan {

  def stitchAll(atomPlans: Seq[AtomPlan]): AtomPlan =  
    atomPlans
      .map(_.dag)
      .map(_.exciseAllFromChain(_.isBar))
      .reduceLeft { (previous, current) =>
        val previousLeaf = previous.leaveIds.force.one  
        val currentRoot  = current .rootIds .force.one

        current.mergeDisjointContinuous(
            previous)(
            previousLeaf -> currentRoot) }
    .pipe(AtomPlan.apply)

}

// ===========================================================================
