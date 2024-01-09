package galliatesting
package suites
package misc

// ===========================================================================
object ReflectionTest extends utest.TestSuite { import utest._
  private val JavaInt     = "java.lang.Integer"
  private val JavaString  = "java.lang.String"

  // ===========================================================================
  val tests = Tests {

    // ---------------------------------------------------------------------------
    test("basic") {
      test("integer") {
        val jv : java.lang.Integer = 1
        val scl: scala.Int         = jv
        val any: Any               = jv

        test(checkClassName(jv .getClass.getName, JavaInt))
        test(checkClassName(scl.getClass.getName, "int"))
        test(checkClassName(any.getClass.getName, JavaInt))

        test(checkClassName(generic(jv ), JavaInt))
        test(checkClassName(generic(scl), JavaInt))
        test(checkClassName(generic(any), JavaInt))

        test(checkClassName(scala.Int.box  (scl).getClass.getName, JavaInt))
        test(checkClassName(scala.Int.unbox(jv) .getClass.getName, "int")) }

      // ---------------------------------------------------------------------------
      test("string") {
        val jv : java.lang.String    = "foo"
        val scl: scala.Predef.String = jv
        val any: Any                 = jv

        test(checkClassName(jv .getClass.getName, JavaString))
        test(checkClassName(scl.getClass.getName, JavaString))
        test(checkClassName(any.getClass.getName, JavaString))

        test(checkClassName(generic(jv ), JavaString))
        test(checkClassName(generic(scl), JavaString))
        test(checkClassName(generic(any), JavaString)) } }

    // ---------------------------------------------------------------------------
    // FIXME: t231229155632 - scala 3 migration
    /*test("if applicable") { // for union types
      val node = gallia.typeNode[Int]

      assert(node.ifApplicable(_ + 1).apply("foo") == "foo") // ignored
      assert(node.ifApplicable(_ + 1).apply(1)     == 2) }*/ }

  // ===========================================================================
  private def checkClassName(actual: String, expected: String): Unit = Predef.assert(actual == expected, actual -> expected)

  // ---------------------------------------------------------------------------
  private def generic[T](t: T): String = t.getClass.getName
}

// ===========================================================================
