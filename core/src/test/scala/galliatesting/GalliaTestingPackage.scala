// ===========================================================================
package object galliatesting {
  type GalliaTestSuite = gallia.testing.Suite

  // ---------------------------------------------------------------------------
  @deprecated type _Error = gallia.vldt._Error
  @deprecated val  _Error = gallia.vldt._Error

  @deprecated val ErrorId = gallia.vldt.ErrorId

  // ===========================================================================
  private[galliatesting] val f  = "f"
  private[galliatesting] val F  = "F"
  private[galliatesting] val f1 = "f1"
  private[galliatesting] val f2 = "f2"
  private[galliatesting] val f3 = "f3"
  private[galliatesting] val g  = "g"
  private[galliatesting] val g1 = "g1"
  private[galliatesting] val g2 = "g2"
  private[galliatesting] val h  = "h"
  private[galliatesting] val p  = "p"
  private[galliatesting] val z  = "z"
  private[galliatesting] val e  = "e"

  // ---------------------------------------------------------------------------
  private[galliatesting] val _t  = true
  private[galliatesting] val _f  = false

  // ---------------------------------------------------------------------------
  private[galliatesting] val foo = "foo"
  private[galliatesting] val FOO = "FOO"

    private[galliatesting] val foo1 = "foo1"
    private[galliatesting] val foo2 = "foo2"
    private[galliatesting] val foo3 = "foo3"
    private[galliatesting] val foo4 = "foo4"

  private[galliatesting] val bar = "bar"
  private[galliatesting] val baz = "baz"
  private[galliatesting] val qux = "qux"

  // ===========================================================================
  private[galliatesting] implicit class GalliaTestingAnything1_[A](value: A) { // so as to not import chaining._ everywhere
    def pipe[B](f: A => B)   : B =   f(value)
    def pype[B](f: A => B)   : B =   f(value)
    def tap    (f: A => Unit): A = { f(value); value }

    def p___                  : A = new aptus.Anything_(value).p__
    def p_                    : A = new aptus.Anything_(value).p
    def i_(f: A => Any)       : A = new aptus.Anything_(value).i(f)

    def __p                   : A = new aptus.Anything_(value).p__
    def  _p                   : A = new aptus.Anything_(value).p
    def  _i(f: A => Any)      : A = new aptus.Anything_(value).i(f) } }

// ===========================================================================
