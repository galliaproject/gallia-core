package gallia.data.multiple

import aptus.{Anything_, Seq_}

import gallia.obj

// ===========================================================================
trait ObjsAggregations { self: Objs =>
  @deprecated private def tmp(keyObjOpt: Option[Obj]) = keyObjOpt.get // FIXME: t210202163714

  // ===========================================================================
  def group1N(groupee: Key, groupers: Keyz): Objs =
    flatMapToStreamer { o => o.opt(groupee).map(o.retainOpt(groupers) -> _) }
      .groupByKey
      .map { case (keyObjOpt, values) => tmp(keyObjOpt).put(groupee, values) }
      .thn(Objs.build)

  // ---------------------------------------------------------------------------
  def groupNN(groupees: Keyz, grouper: Keyz, as: Key): Objs =
    flatMapToStreamer { o => o.retainOpt(groupees).map(o.retainOpt(grouper) -> _) }
      .groupByKey
      .map { case (keyObjOpt, values) => tmp(keyObjOpt).put(as, values) }
      .thn(Objs.build)

  // ===========================================================================
  def aggregateNumbers1(groupee: Key, grouper: Keyz, as: Key)(f: List[Option[Any]] => Any): Objs =
    mapToStreamer { o => o.retainOpt(grouper) -> o.opt(groupee) }
       // TODO: if empty? wrap runtime error
      .groupByKey
      .map { case (k, values) => tmp(k).put(as, f(values))}
      .thn(Objs.build)

  // ---------------------------------------------------------------------------
  def aggregateNumbersN(groupees: Keyz, grouper: Keyz, as: Key)(f: Seq[Any] => Any): Objs =
    flatMapToStreamer { o => o.retainOpt(groupees).map(o.retainOpt(grouper) -> _) }
      .groupByKey
      .map { case (k, values) => tmp(k).put(as, f(values)) }
      .thn(Objs.build)

  // ===========================================================================
  def countLike(groupees: Keyz, grouper: Keyz, as: Key)(f: List[Option[Obj]] => Any): Objs =
    mapToStreamer { o => o.retainOpt(grouper) -> o.retainOpt(groupees) }
      .groupByKey
      .map { case (keyObjOpt, values) => tmp(keyObjOpt).put(as, f(values)) }
      .thn(Objs.build)

  // ===========================================================================
  @deprecated("see 210118083814 for new version") def numberStats(groupee: Key, grouper: Keyz, as: Key): Objs =
    flatMapToStreamer { o => o.opt(groupee).map(o.retainOpt(grouper) -> _) }
       // TODO: if empty? wrap runtime error
      .groupByKey
      .map { case (keyObjOpt, values) =>
        val nums = values.asInstanceOf[Seq[Number]].map(_.doubleValue).toList

        val stats = // TODO: t210115140107 - use dedicated lib? or just wrap commons'?
          obj(
              //FIXME: t210115141937 - more (size, uniq, skewness, kurtosis, min, max, mode, IQR, percentiles, ...) + check for/handle NaNs (see t210115144940)
              gallia._mean   -> nums.mean,
              gallia._stdev  -> nums.stdev)
        tmp(keyObjOpt).put(as, stats) }
      .thn(Objs.build)
}


// ===========================================================================
