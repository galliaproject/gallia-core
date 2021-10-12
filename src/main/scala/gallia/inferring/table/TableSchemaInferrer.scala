package gallia
package inferring.table

import aptus.Seq_

import reflect.BasicType
import meta.Fld
import meta.Info
import reflect.Container
import io.CellConf

// ===========================================================================
object TableSchemaInferrer {

  def fullInferring(conf: CellConf, keys: Seq[Key])(z: Objs): Cls =
      infoLookup(conf)(
            keySet  = keys.toSet,
            mutable = new MutableValuesSubset(keys, max = 3 /* enough for boolean detection at least */))(
          z)
        .pipe { lookup =>
          keys
            .map { key => Fld(key, lookup(key)) }
            .pipe(Cls.apply) }

    // ---------------------------------------------------------------------------
    private def infoLookup(conf: CellConf)(keySet: Set[Key], mutable: MutableValuesSubset)(z: Objs): Map[Key, Info] =
      z .consume
        .foldLeft(Set[(Key, Info)]()) { (curr, o) =>
          curr ++
            keySet
              .map { key =>
                val value = o.string(key) // guaranteed present by 201215141231
                  .tap {
                    conf
                      .valueSet(_)
                      .pipe(mutable.addValues(key, _)) }

                key -> conf.inferInfo(value)
              } }
        .toSeq
        .groupByKey
        .mapValues(combineInfos)
        .map { case (key, info) =>
          key -> mutable.potentiallyUpdateInfo(key, info) }
        .toMap

  // ===========================================================================
  def stringsOnly(conf: CellConf, keys: Seq[Key])(z: Objs): Cls =
      stringsOnlyInfoLookup(conf)(keys.toSet)(z)
        .pipe { lookup =>
          keys
            .map { key => Fld(key, lookup(key)) }
            .pipe(Cls.apply) }

    // ---------------------------------------------------------------------------
    private def stringsOnlyInfoLookup(conf: CellConf)(keySet: Set[Key])(z: Objs): Map[Key, Info] =
      z .consume
        .foldLeft(Set[(Key, Info)]()) { (curr, o) =>
          curr ++
            keySet
              .map { key =>
                val value = o.string(key) // guaranteed present by 201215141231
                key -> Info(conf.inferContainerOnly(value), BasicType._String) } }
        .toSeq
        .groupByKey
        .mapValues(combineContainers)
        .toMap

  // ===========================================================================
  private def combineInfos(values: Seq[Info]): Info =
      Info(
        values.map(_.container)     .reduceLeft(Container.combine),
        values.map(_.forceBasicType).pipe       (BasicType.combine) )

  // ---------------------------------------------------------------------------
  private def combineContainers(values: Seq[Info]): Info =
      Info(
        values.map(_.container).reduceLeft(Container.combine),
        BasicType._String)

}

// ===========================================================================
