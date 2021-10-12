package gallia.meta

import gallia._

// ===========================================================================
object ClsUtils {

  def valuesToObj(dis: Cls)(itr: Iterator[AnyValue]): Obj =
    dis.fields
      .map { field =>
        field.key ->
          field.info.potentiallyProcessNesting(
            value = itr.next()) }
      .pipe(gallia.obj)
      .tap(_ => assert(itr.isEmpty, dis /* TODO: pass original value? */))

}

// ===========================================================================
