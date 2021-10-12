package gallia.plans

import aptus.{Anything_, Seq_, Tuple2_}

import gallia._
import gallia.dag._

// ===========================================================================
private[plans] object AtomProcessor {

  def apply
        (input: Either[Seq[NDT], NDT], missingInputs: RootId => NDT)
        (nodeId: NodeId, nodeAtom: Atom, debug: AtomNodeDebugging)
      : NDT = {
    def obj  :  Obj         = input.right.get.forceO
    def objs :  Objs        = input.right.get.forceZ
    def vle  :  Vle         = input.right.get.forceVle

    def obj2 : (Obj , Obj ) = input.left.get.force.tuple2.mapAll(_.forceO, _.forceO)
    def objs2: (Objs, Objs) = input.left.get.force.tuple2.mapAll(_.forceZ, _.forceZ)

    // ===========================================================================
    import InputData._

    def inputData(atom: Atom): InputData = // this is a bit of an afterthought... TODO: t210114125607 - improve
      util.Try{
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
      }.getOrElse(_Undetermined)

    // ===========================================================================
    util.Try { // TODO: t210114143028 ability to opt out (t210118134814 - confirm runtime cost?)
      (nodeAtom match {

        case x: AtomUU => x.naive(obj)
        case x: AtomZZ => x.naive(objs)
        
        // ===========================================================================
        case NestingDataPlaceholder => missingInputs(nodeId)

        // ---------------------------------------------------------------------------
        case x: AtomIU => x.naive.map(NDT.to ).get // by design
        case x: AtomIZ => x.naive.map(NDT.to ).get
        case x: AtomIV => x.naive.map(NDT.vle).get

        // ===========================================================================
        case x: AtomUO => obj .tap(x.naive)
        case x: AtomZO => objs.tap(x.naive)
        case x: AtomVO => vle .tap(x.naive).pipe(NDT.vle) // TODO: keep? instead of "grab"?
        
        // ===========================================================================
        // less common

        case x: AtomZU => x.naive(objs)
        case x: AtomUZ => x.naive(obj)

        // ---------------------------------------------------------------------------
        case x: AtomUUtoU => (x.naive _).tupled(obj2 )
        case x: AtomZZtoZ => (x.naive _).tupled(objs2)

        // ---------------------------------------------------------------------------
        case x: AtomVV => x.naive(vle ).pipe(NDT.vle)
        case x: AtomUV => x.naive(obj ).pipe(NDT.vle)
        case x: AtomZV => x.naive(objs).pipe(NDT.vle)

      }): NDT

    } match {

      //TODO: t210114111111 - distinguish RuntimeErrors (eg not distinct checking it is) from others (eg transform "".head or bugs)
      case util.Failure(throwable) =>
        System.err.println(s"${throwable.getMessage}: ${throwable.getStackTrace.toList.mkString(", ")}")
        throw new Exception(
            RunCtx(nodeId, nodeAtom, debug, throwable, inputData(nodeAtom)).formatDefault, // will be pretty ugly...
            throwable)

      // ---------------------------------------------------------------------------
      case util.Success(ndt) => ndt
    }
  }

}

// ===========================================================================
