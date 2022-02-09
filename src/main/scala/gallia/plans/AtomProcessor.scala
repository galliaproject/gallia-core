package gallia
package plans

import aptus.{Seq_, Tuple2_}

// ===========================================================================
private[plans] object AtomProcessor {

  def apply
        (input: DataInput, missingInputs: dag.RootId => NDT)
        (nodeId: NodeId, nodeAtom: Atom, debug: AtomNodeDebugging)
      : NDT = { import InputData._

    def inputData(atom: Atom): InputData = // this is a bit of an afterthought... TODO: t210114125607 - improve
      util.Try{
        atom match {
          case NestingDataPlaceholder => _None
  
          // ---------------------------------------------------------------------------
          case x: AtomIU => _None
          case x: AtomIZ => _None
          case x: AtomIV => _None
  
          // ---------------------------------------------------------------------------
          case x: AtomUO => _Obj (input.obj)
          case x: AtomZO => _Objs(input.objs)
          case x: AtomVO => _Vle (input.vle)
  
          // ---------------------------------------------------------------------------
          case x: AtomUU => _Obj (input.obj)
          case x: AtomZZ => _Objs(input.objs)// TODO: t210114111539: identity wrapped Us vs full on Zs, so can pinpoint problematic object
  
          // ---------------------------------------------------------------------------
          case x: AtomZU => _Objs(input.objs)
          case x: AtomUZ => _Obj (input.obj)
  
          // ---------------------------------------------------------------------------
          case x: AtomUUtoU => _Obj2 .tupled(input.obj2 )
          case x: AtomZZtoZ => _Objs2.tupled(input.objs2)
  
          // ---------------------------------------------------------------------------
          case x: AtomUV => _Obj (input.obj )
          case x: AtomZV => _Objs(input.objs)
          case x: AtomVV => _Vle (input.vle )
        }
      }.getOrElse(_Undetermined)

    // ===========================================================================
    util.Try { // TODO: t210114143028 ability to opt out (t210118134814 - confirm runtime cost?)
      (nodeAtom match {

        case x: AtomUU => x.naive(input.obj)
        case x: AtomZZ => x.naive(input.objs)
        
        // ===========================================================================
        case NestingDataPlaceholder => missingInputs(nodeId)

        // ---------------------------------------------------------------------------
        case x: AtomIU => x.naive.map(NDT.to ).get // by design
        case x: AtomIZ => x.naive.map(NDT.to ).get
        case x: AtomIV => x.naive.map(NDT.vle).get

        // ===========================================================================
        case x: AtomUO => input.obj .tap(x.naive)
        case x: AtomZO => input.objs.tap(x.naive)
        case x: AtomVO => input.vle .tap(x.naive).pipe(NDT.vle) // TODO: keep? instead of "grab"?
        
        // ===========================================================================
        // less common

        case x: AtomZU => x.naive(input.objs)
        case x: AtomUZ => x.naive(input.obj)

        // ---------------------------------------------------------------------------
        case x: AtomUUtoU => (x.naive _).tupled(input.obj2 )
        case x: AtomZZtoZ => (x.naive _).tupled(input.objs2)

        // ---------------------------------------------------------------------------
        case x: AtomVV => x.naive(input.vle ).pipe(NDT.vle)
        case x: AtomUV => x.naive(input.obj ).pipe(NDT.vle)
        case x: AtomZV => x.naive(input.objs).pipe(NDT.vle)

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
