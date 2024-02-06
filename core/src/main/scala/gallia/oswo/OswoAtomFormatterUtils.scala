package gallia
package oswo

import aptus._

// ===========================================================================
private object OswoAtomFormatterUtils {

  private[oswo] implicit class ANI_(id: ANI) {

    // eg: "N000000000_0" -> "0", "N000000001_0" -> "1", "N000000001_1" -> "1_1"
    def norm =
      id
        .replace("-", "_")
        .replaceAll("N0+", "")
        .replaceAll("_0$", "")
        .in.noneIf(_.isEmpty)
        .getOrElse("0")

    def method  (name: String) = "m_"  + norm + "_" + name
    def cc      (name: String) = "cc_" + norm
    def instance(name: String) = "i_"  + norm }

  // ===========================================================================
  def className[T](t: T) = t.getClass.getSimpleName.stripPrefixGuaranteed("_")

  // ---------------------------------------------------------------------------
  // val i_N000000001_1: cc_N000000001_1 = m_N000000001_1(i_N000000001_0)

  def _call0           (id: ANI)(last: IsLast)(name: String) = (if (last) "" else s"""val ${id.instance(name)}: ${id.cc(name)} = """) + s"""${id.method(name)}()"""
  def _call1(from: ANI, id: ANI)(last: IsLast)(name: String) = (if (last) "" else s"""val ${id.instance(name)}: ${id.cc(name)} = """) + s"""${id.method(name)}(${from.instance(name)})"""

  // ---------------------------------------------------------------------------
  def equals = "// ".append("=" * 75)
  def dashes = "// ".append("-" * 75)

  // ---------------------------------------------------------------------------
  def format(cc: Seq[String], method: String, call: String): String =
    equals
        .newline(cc.joinln)
      .newline
      .newline(dashes)
        .newline(method)
      .newline(dashes)
        .newline(call)
      .newline.newline

  // ===========================================================================
  // TODO: common keys
  def commonAdd(y: AtomOswo)(id: ANI) = common3(y)(id) { (c1, c2) => (c1.size + 1 == c2.size) ||
(c1.size == c2.size) }
    // TODO: if replace...
  def commonRemove(y: AtomOswo)(id: ANI) = common3(y)(id) { (c1, c2) => c1.size - 1 == c2.size } // TODO: common keys

  def common2[T](y: AtomOswo)(id: ANI)(f: Cls => T) = common3[T](y)(id) { (c1, c2) => c1.pipe(f) == c2.pipe(f) }

  def common3[T](atom: AtomOswo)(id: ANI)(f: (Cls, Cls) => Boolean): Name = {
    assert(atom._metaIO != null)

    assert(f(atom._metaIO.in, atom._metaIO.out), atom._metaIO)

    className(atom)  }

}

// ===========================================================================
