package gallia
package meta

import aptus.Anything_

// ===========================================================================
object ClsUtils {

  def valuesToObjOpt(dis: Cls)(itr: Iterator[AnyValue]): Option[Obj] =
    dis.fields
      .map { field =>
        field.key ->
          field.info.potentiallyProcessNesting(
            value = itr.next()) }
      .flatMap(data.single.ObjIn.normalizeEntry)
      .in.noneIf(_.isEmpty)
      .map(obj)
      .tap(_ => assert(itr.isEmpty, dis /* TODO: pass original value? */))

}

// ===========================================================================
