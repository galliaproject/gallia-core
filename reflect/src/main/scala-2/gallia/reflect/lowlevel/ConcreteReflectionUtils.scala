package gallia
package reflect
package lowlevel

// ===========================================================================
// until scala 2 macros are ready (t23112114209)
object ConcreteReflectionUtils extends ReflectionTypesAbstraction {
  // see https://stackoverflow.com/questions/18729321/how-to-get-classtag-form-typetag-or-both-at-same-time
  def ctag[T : WTT]: scala.reflect.ClassTag[T] = { val tag = runiverse.weakTypeTag[T]
    scala.reflect.ClassTag[T](tag.mirror.runtimeClass(tag.tpe)) } }

// ===========================================================================
