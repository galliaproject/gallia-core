package gallia.plans

import aptus.{Anything_, Seq_}

import gallia._
import gallia.dag._
import gallia.FunctionWrappers._

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

    // ===========================================================================
    object FromNesting {

      def u2u(optional: Boolean): _ff11 =
        if (optional) _naiveRunU2U_ _
        else          _naiveRunU2U  _

      def z2z(optional: Boolean): _ff11 =
        if (optional) _naiveRunZZ_ _
        else          _naiveRunZZ  _

      // ---------------------------------------------------------------------------
      def u2z(optional: Boolean): _ff11 =
        if (optional) _naiveRunUZ_ _
        else          _naiveRunUZ  _

      def z2u(optional: Boolean): _ff11 =
        if (optional) _naiveRunZU_ _
        else          _naiveRunZU  _

      // ---------------------------------------------------------------------------
      def u2v(optional: Boolean): _ff11 =
          if (optional) _naiveRunUV_ _
          else          _naiveRunUV  _

      def z2v(optional: Boolean): _ff11 =
          if (optional) _naiveRunZV_ _
          else          _naiveRunZV  _

      def uu2u(optional1: Boolean, optional2: Boolean): _ff21 =
          if (optional1 || optional2) ??? // FIXME
          else                        _naiveRunUu2U _

      // ===========================================================================
      private def _naiveRunU2U (missingInput : Any): Any = naiveRunUU (missingInput .asInstanceOf[       Obj ])
      private def _naiveRunU2U_(missingInput : Any): Any = naiveRunUU_(missingInput .asInstanceOf[Option[Obj]])

      private def _naiveRunUV (missingInput: Any): Any = naiveRunUV (missingInput.asInstanceOf[       Obj ])
      private def _naiveRunUV_(missingInput: Any): Any = naiveRunUV_(missingInput.asInstanceOf[Option[Obj]])

      private def _naiveRunZV (missingInput: Any): Any = naiveRunZV (missingInput.asInstanceOf[       Seq[Obj] ].thn(Objs.from))
      private def _naiveRunZV_(missingInput: Any): Any = naiveRunZV_(missingInput.asInstanceOf[Option[Seq[Obj]]].map(Objs.from))

      private def _naiveRunZZ (missingInput: Any): Any = naiveRunZZ (missingInput.asInstanceOf[       Seq[Obj] ].thn(Objs.from))      .toListAndTrash
      private def _naiveRunZZ_(missingInput: Any): Any = naiveRunZZ_(missingInput.asInstanceOf[Option[Seq[Obj]]].map(Objs.from)).map(_.toListAndTrash)

      private def _naiveRunUZ_(missingInput: Any): Any = naiveRunUZ_(missingInput.asInstanceOf[Option[Obj]]).toList
      private def _naiveRunUZ (missingInput: Any): Any = naiveRunUZ (missingInput.asInstanceOf[       Obj ]).toListAndTrash

      private def _naiveRunZU_(missingInput: Any): Any = naiveRunZU_(missingInput.asInstanceOf[Option[Seq[Obj]]].map(Objs.from))
      private def _naiveRunZU (missingInput: Any): Any = naiveRunZU (missingInput.asInstanceOf[       Seq[Obj] ].thn(Objs.from))

      private def _naiveRunUu2U(missingInput1: Any, missingInput2: Any): Any = naiveRunUu2U(missingInput1.asInstanceOf[Obj], missingInput2.asInstanceOf[Obj])
    }
  }

// ===========================================================================
