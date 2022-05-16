package gallia
package inferring.table

import aptus.Seq_

import reflect.BasicType
import reflect.Container
import meta.Fld
import meta.Ofni
import io.CellConf

// ===========================================================================
object TableSchemaInferrer {

  def fullInferring(conf: CellConf, keys: Seq[Key])(z: Objs): Cls =
      ofniLookup(conf)(
            keySet  = keys.toSet,
            mutable = new MutableValuesSubset(keys, max = 3 /* enough for boolean detection at least */))(
          z)
        .pipe { lookup =>
          keys
            .map { key => Fld(key, lookup(key)) }
            .pipe(Cls.apply) }

    // ---------------------------------------------------------------------------
    private def ofniLookup(conf: CellConf)(keySet: Set[Key], mutable: MutableValuesSubset)(z: Objs): Map[Key, Ofni] =
      z .consume
        .foldLeft(Set[(Key, Ofni)]()) { (curr, o) =>
          tmp(conf, keySet, mutable)(curr, o) }
        .toSeq
        .groupByKey
        .view.mapValues(combineOfnisAll)
        .map { case (key, ofni) =>
          key -> mutable.potentiallyUpdateOfni(key, ofni) }
        .toMap

      // ---------------------------------------------------------------------------
      private def tmp(conf: CellConf, keySet: Set[Key], mutable: MutableValuesSubset)(curr: Set[(Key, Ofni)], o: Obj): Set[(Key, Ofni)] = {
        curr ++
          keySet
            .map { key =>
              val value = o.string(key) // guaranteed present by 201215141231
                .tap {
                  conf
                    .valueSet(_)
                    .pipe(mutable.addValues(key, _)) }

              key -> conf.inferOfni(value) } }

  // ===========================================================================
  def stringsOnly(conf: CellConf, keys: Seq[Key])(z: Objs): Cls =
    stringsOnlyOfniLookup(conf)(keys.toSet)(z)
      .pipe { lookup =>
        keys
          .map { key => Fld(key, lookup(key)) }
          .pipe(Cls.apply) }

  // ---------------------------------------------------------------------------
  private def stringsOnlyOfniLookup(conf: CellConf)(keySet: Set[Key])(z: Objs): Map[Key, Ofni] =
    z .consume
      .foldLeft(Set[(Key, Ofni)]()) { (curr, o) =>
        curr ++
          keySet
            .map { key =>
              val value = o.string(key) // guaranteed present by 201215141231
              val container = conf.inferContainerOnly(value)
              key -> container.ofni(BasicType._String) } }
      .toSeq
      .groupByKey
      .view.mapValues(combineOfnisString)
      .toMap

  // ===========================================================================
  private def combineOfnisAll   (values: Seq[Ofni]): Ofni = _combineOfnis(values)(_.map(_.forceBasicType).pipe(reflect.BasicTypeUtils.combine))
  private def combineOfnisString(values: Seq[Ofni]): Ofni = _combineOfnis(values)(_ => BasicType._String)

    // ---------------------------------------------------------------------------
    private def _combineOfnis(values: Seq[Ofni])(f: Seq[Ofni] => BasicType): Ofni =
      values
        .map(_.container1)
        .reduceLeft(Container.combine)
        .ofni(f(values))
}

// ===========================================================================
