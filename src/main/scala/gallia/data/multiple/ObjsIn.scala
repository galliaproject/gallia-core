package gallia
package data
package multiple

import aptus.String_

// ===========================================================================
object ObjsIn {

  def streamFromFile(path: String): Objs =
    new DataRegenerationClosure[Obj] {
      def regenerate = () =>
        path
          .streamFileLines2() //TODO: add logProgress
          .map(Obj.fromJsonString) }
      .pipe(Objs.from)

}

// ===========================================================================