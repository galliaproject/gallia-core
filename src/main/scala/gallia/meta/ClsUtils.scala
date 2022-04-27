package gallia
package meta

// ===========================================================================
object ClsUtils {

  def valuesToObj(dis: Cls)(itr: Iterator[AnyValue]): Obj =
    dis.fields
      .map { field =>
        field.key ->
          field.ofni.potentiallyProcessNesting(
            value = itr.next()) }
      .pipe(obj)
      .tap(_ => assert(itr.isEmpty, dis /* TODO: pass original value? */))

}

// ===========================================================================
