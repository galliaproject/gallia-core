package gallia
package plans

import aptus.String_

// ===========================================================================
private[plans] object AtomProcessor {

  def apply
        (input: DataInput, missingInputs: dag.RootId => NDT)
        (nodeId: NodeId, nodeAtom: Atom)
        (afferentSchemas: Clss, efferentSchema: Cls)
        (debug: AtomNodeDebugging)
      : NDT = { import InputData._

    def inputData(atom: Atom): InputData = // this is a bit of an afterthought... TODO: t210114125607 - improve
      util.Try {
        atom match {
          case NestingDataPlaceholder => _None
  
          // ---------------------------------------------------------------------------
          case _: AtomIU  => _None
          case _: AtomIZx => _None
          case _: AtomIV  => _None

          // ---------------------------------------------------------------------------
          case _: AtomUO => _Obj (input.obj)
          case _: AtomZO => _Objs(input.objs)
          case _: AtomVO => _Vle (input.vle)

          // ---------------------------------------------------------------------------
          case _: AtomUU => _Obj (input.obj)
          case _: AtomZZ => _Objs(input.objs)// TODO: t210114111539: identity wrapped Us vs full on Zs, so can pinpoint problematic object

          // ---------------------------------------------------------------------------
          case _: AtomZU => _Objs(input.objs)
          case _: AtomUZ => _Obj (input.obj)

          // ---------------------------------------------------------------------------
          case _: AtomUUtoU => (_Obj2 .apply _).tupled(input.obj2 )
          case _: AtomZZtoZ => (_Objs2.apply _).tupled(input.objs2)

          // ---------------------------------------------------------------------------
          case _: AtomUV => _Obj (input.obj )
          case _: AtomZV => _Objs(input.objs)
          case _: AtomVV => _Vle (input.vle )

          // ---------------------------------------------------------------------------
          case _: AtomVU => _Vle (input.obj )
          case _: AtomVZ => _Vle (input.objs)
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

        case x: AtomIUx => x.naive(efferentSchema).map(NDT.to ).get
        case x: AtomIZx => x.naive(efferentSchema).map(NDT.to ).get

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

        // ---------------------------------------------------------------------------
        case x: AtomVU => x.naive(input.vle)//.pipe(NDT.to)
        case x: AtomVZ => x.naive(input.vle)//.pipe(NDT.to)

        // ---------------------------------------------------------------------------
        case x: AtomVv2V  => (x.naive _).tupled(input.vle2)   .pipe(NDT.vle)
        case x: AtomZVtoZ => (x.naive _).tupled(input.objsVle)
      }): NDT

    } match {

      //TODO: t210114111111 - distinguish RuntimeErrors (eg not distinct checking it is) from others (eg transform "".head or bugs)
      case util.Failure(throwable) =>
        throw new Exception( // will be pretty ugly...
          RunCtx(nodeId, nodeAtom, debug, throwable, inputData(nodeAtom))
            .formatDefault
            .prepend("240125155846:"))

      // ---------------------------------------------------------------------------
      case util.Success(ndt) => ndt
    }
  }

}

// ===========================================================================
