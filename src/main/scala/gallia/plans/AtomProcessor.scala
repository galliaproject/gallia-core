package gallia.plans

import aptus.{Anything_, Seq_, Tuple2_}

import gallia._
import gallia.dag._

// ===========================================================================
private[plans] class AtomProcessor(missingInputs: RootId => NDT, data: NodeId => NDT) {

  def process(afferentNodeIds: Seq[NodeId])(node: AtomNode): NDT = {
    def ndt :  NDT       = data(afferentNodeIds.force.one)
    def ndt2: (NDT, NDT) = afferentNodeIds.force.tuple2.mapAll(data, data)

    // ---------------------------------------------------------------------------
      def obj : Obj  = ndt.forceO
      def objs: Objs = ndt.forceZ
      def vle : Vle  = ndt.forceVle

      // ---------------------------------------------------------------------------
    def obj2 : (Obj , Obj ) = ndt2.mapAll(_.forceO, _.forceO)
    def objs2: (Objs, Objs) = ndt2.mapAll(_.forceZ, _.forceZ)

    // ===========================================================================
    import InputData._

    def inputData(atom: Atom): InputData = // this is a bit of an afterthought... TODO: t210114125607 - improve
      atom match {
        case NestingDataPlaceholder => _None

        // ---------------------------------------------------------------------------
        case x: AtomIU => _None
        case x: AtomIZ => _None
        case x: AtomIV => _None

        // ---------------------------------------------------------------------------
        case x: AtomUO => _Obj (obj)
        case x: AtomZO => _Objs(objs)
        case x: AtomVO => _Vle (vle)

        // ---------------------------------------------------------------------------
        case x: AtomUU => _Obj (obj)
        case x: AtomZZ => _Objs(objs)// TODO: t210114111539: identity wrapped Us vs full on Zs, so can pinpoint problematic object

        // ---------------------------------------------------------------------------
        case x: AtomZU => _Objs(objs)
        case x: AtomUZ => _Obj (obj)

        // ---------------------------------------------------------------------------
        case x: AtomUUtoU => _Obj2 .tupled(obj2 )
        case x: AtomZZtoZ => _Objs2.tupled(objs2)

        // ---------------------------------------------------------------------------
        case x: AtomUV => _Obj (obj )
        case x: AtomZV => _Objs(objs)
        case x: AtomVV => _Vle (vle )
      }

    // ===========================================================================
    util.Try { // TODO: t210114143028 ability to opt out (t210118134814 - confirm runtime cost?)
      (node.atom match {

        case NestingDataPlaceholder => missingInputs(node.id)

        // ---------------------------------------------------------------------------
        case x: AtomIU => x.naive.map(NDT.to ).getOrElse(missingInputs(node.id) /* TODO: error message if not found */)
        case x: AtomIZ => x.naive.map(NDT.to ).getOrElse(missingInputs(node.id))
        case x: AtomIV => x.naive.map(NDT.vle).getOrElse(missingInputs(node.id))

        // ---------------------------------------------------------------------------
        case x: AtomUO => obj .sideEffect(x.naive)
        case x: AtomZO => objs.sideEffect(x.naive)
        case x: AtomVO => vle .sideEffect(x.naive).thn(NDT.vle) // TODO: keep? instead of "grab"?

        // ---------------------------------------------------------------------------
        case x: AtomUU => x.naive(obj)
        case x: AtomZZ => x.naive(objs)

        // ---------------------------------------------------------------------------
        case x: AtomZU => x.naive(objs)
        case x: AtomUZ => x.naive(obj)

        // ---------------------------------------------------------------------------
        case x: AtomUUtoU => (x.naive _).tupled(obj2 )
        case x: AtomZZtoZ => (x.naive _).tupled(objs2)

        // ---------------------------------------------------------------------------
        case x: AtomUV => x.naive(obj ).thn(NDT.vle)
        case x: AtomZV => x.naive(objs).thn(NDT.vle)
        case x: AtomVV => x.naive(vle ).thn(NDT.vle)

      }): NDT

    } match {

      //TODO: t210114111111 - distinguish RuntimeErrors (eg not distinct checking it is) from others (eg transform "".head or bugs)
      case util.Failure(throwable) =>
        throw new Exception(
            RunCtx(node, throwable, inputData(node.atom)).formatDefault, // will be pretty ugly...
            throwable)

      // ---------------------------------------------------------------------------
      case util.Success(ndt) => ndt
    }
  }

}

// ===========================================================================
