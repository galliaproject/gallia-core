package gallia
package reflect
package lowlevel

import scala.reflect.runtime.{universe => RuntimeUniverse}
import scala.reflect.internal.Symbols

// ===========================================================================
private object CompanionReflection {
  
  private val RuntimeMirror  = RuntimeUniverse.runtimeMirror(getClass.getClassLoader)
  private val RuntimeMirror2 = RuntimeMirror.asInstanceOf[{ def methodToJava(sym: Symbols#MethodSymbol): java.lang.reflect.Method }] // see https://stackoverflow.com/a/16791962/4228079

  // ---------------------------------------------------------------------------
  def apply[T : RuntimeUniverse.WeakTypeTag](methodName: String)(args: Object*): T = {
    val tpe = RuntimeUniverse.weakTypeTag[T].tpe

    tpe
      .companion
      .member(RuntimeUniverse.TermName(methodName))
      .asInstanceOf[Symbols#MethodSymbol]
      .pipe(RuntimeMirror2.methodToJava)
      .invoke(
          companionObjectInstance[T](tpe),
            args:_*)
      .asInstanceOf[T]
  }
  
  // ===========================================================================
  // inspired by https://stackoverflow.com/a/17473794/4228079
  private def companionObjectInstance[T: RuntimeUniverse.WeakTypeTag](tpe: RuntimeUniverse.Type) =
    tpe
      .typeSymbol
      .companion
      .asModule
      .pipe(RuntimeMirror.reflectModule)
      .instance
}


// ===========================================================================
