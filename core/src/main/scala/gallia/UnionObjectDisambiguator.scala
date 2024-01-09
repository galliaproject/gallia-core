package gallia

// ===========================================================================
private[gallia] sealed trait UnionObjectDisambiguator {
    def filter(ncs: Seq[Cls]): Seq[Cls]

    // ---------------------------------------------------------------------------
    final def _vldt(f: Fld) =
      f .valueTypes
        .flatMap(_.nestingOpt)
        .pipe(filter)
        .ifOneElement(_ => Nil, _ => vldt._Error.NotExactlyOneNestedClass(f).errs)
  }

  // ===========================================================================
  private[gallia] case class   DisambiguateByClassIndex(value: aptus.Index) extends UnionObjectDisambiguator { def filter(ncs: Seq[Cls]) = Seq(ncs(value)) }

  // ---------------------------------------------------------------------------
  private[gallia] sealed trait DisambiguateByClassPredicate extends UnionObjectDisambiguator {
      val meta: Cls => Boolean
      final def filter(ncs: Seq[Cls]) = meta.pipe(ncs.filter) }

    // ---------------------------------------------------------------------------
    private[gallia] case class DisambiguateByClassPredicateU(meta: Cls => Boolean, data:     Obj  => Boolean) extends DisambiguateByClassPredicate
    private[gallia] case class DisambiguateByClassPredicateZ(meta: Cls => Boolean, data: Seq[Obj] => Boolean) extends DisambiguateByClassPredicate

// ===========================================================================