package gallia.meta

import aptus.Anything_

import gallia._

// ===========================================================================
object ClsUtils {

  def valuesToObj(dis: Cls)(itr: Iterator[AnyValue]): Obj =
    dis.fields
      .map { field =>
        field.key ->
          field.info.potentiallyProcessNesting(
            value = itr.next()) }
      .thn(gallia.obj)
      .sideEffect(_ => assert(itr.isEmpty, dis /* TODO: pass original value? */))

}

// ===========================================================================
