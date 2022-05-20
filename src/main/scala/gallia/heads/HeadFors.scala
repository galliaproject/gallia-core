package gallia
package heads

import actions.ActionsFor._
import selection.untyped.UtsBoilerplate._

// ===========================================================================
trait HeadUFors { ignored: HeadU =>
    /* TODO: t201007093053 - forKeyPair, for group(_.firstKey).by(_.lastKey) */

    // ---------------------------------------------------------------------------
    def forKey(f  : KeyW           ): _ForKey = forKey(_.explicit(f.value)) // can also be used to chain conveniently
    def forKey(sel: ForKey.Selector): _ForKey = new _ForKey(sel)

      // ---------------------------------------------------------------------------
      class _ForKey(sel: ForKey.Selector) { import ForKey._
        def thn        (f: (Self, Key) => Self)                            : Self     = uu(ForPathUU   (resolve(sel), (u, p) => f(u, p.key)))
        def thn[V: WTT](f: (Self, Key) => HeadV[V])(implicit d: DI)        : HeadV[V] = uv(ForPathUV[V](resolve(sel), (u, p) => f(u, p.key)))
        def thn        (f: (Self, Key) => HeadZ   )(implicit d: DI, d2: DI): HeadZ    = uz(ForPathUZ   (resolve(sel), (u, p) => f(u, p.key))) }

    // ===========================================================================
    def forPath(sel: ForPath.Selector) = new { import ForPath._
      def thn(f: (Self, KPath) => Self): Self =
        self ::+ ForPathUU(resolve(sel), f) }

    // ===========================================================================
    def forEachKey(x1: KeyW)                       : _ForEachKey = new _ForEachKey(_.explicit(KeyWz.from(x1))) //TODO: keep?
    def forEachKey(x1: KeyW, x2: KeyW, more: KeyW*): _ForEachKey = new _ForEachKey(_.explicit(x1, x2, more:_*))
    def forEachKey(xs: KeyWz)                      : _ForEachKey = new _ForEachKey(_.explicit(xs))
    def forEachKey(sel: ForEachKey.Selector)       : _ForEachKey = new _ForEachKey(sel)

      // ---------------------------------------------------------------------------
      class _ForEachKey(sel: ForEachKey.Selector) {
        def thn(f: (Self, Key) => Self): Self = self ::+
          ForPathsUU(ForEachKey.resolve(sel), (u, p) => f(u, p.key /* ok by design */)) }

    // ===========================================================================
    def forEachPath(x1: KPathW)                           : _ForEachPath = new _ForEachPath(_.explicit(KPathWz.from(x1))) //TODO: keep?
    def forEachPath(x1: KPathW, x2: KPathW, more: KPathW*): _ForEachPath = new _ForEachPath(_.explicit(x1, x2, more:_*))
    def forEachPath(xs: KPathWz)                          : _ForEachPath = new _ForEachPath(_.explicit(xs))
    def forEachPath(sel: ForEachPath.Selector)            : _ForEachPath = new _ForEachPath(sel)

      // ---------------------------------------------------------------------------
      class _ForEachPath(sel: ForEachPath.Selector) {
        def thn(f: (Self, KPath) => Self): Self = self ::+
          ForPathsUU(ForEachPath.resolve(sel), (u, p) => f(u, p.value)) }

    // ===========================================================================
    // common
    def forAllKeys  (f: (Self, Key)   => Self): Self = forEachKey (_. allKeys) .thn(f)
    def forLeafPaths(f: (Self, KPath) => Self): Self = forEachPath(_.leafPaths).thn(f)
    def forAllPaths (f: (Self, KPath) => Self): Self = forEachPath(_. allPaths).thn(f)

    def forKeysMatching (pred: SKey  => Boolean) = forEachKey (_.filterKeys (pred))
    def forPathsMatching(pred: KPath => Boolean) = forEachPath(_.filterPaths(pred))
    //TODO: def unquoteValueIfApplicable = forAllKeys(_ unquoteValueIfApplicable _) // common enough?
  }

  // ===========================================================================
  trait HeadZFors { ignored: HeadZ =>
    /* TODO: t201007093053 - forKeyPair, for group(_.firstKey).by(_.lastKey) */

    // ---------------------------------------------------------------------------
    def forKey(f  : KeyW)           : _ForKey = forKey(_.explicit(f.value)) // can also be used to chain conveniently
    def forKey(sel: ForKey.Selector): _ForKey = new _ForKey(sel)

      // ---------------------------------------------------------------------------
      class _ForKey(sel: ForKey.Selector) { import ForKey._
        def thn        (f: (Self, Key) => Self)                            : Self     = zz(ForPathZZ   (resolve(sel), (x, p) => f(x, p.key)))
        def thn[V: WTT](f: (Self, Key) => HeadV[V])(implicit d: DI)        : HeadV[V] = zv(ForPathZV[V](resolve(sel), (x, p) => f(x, p.key)))
        def thn        (f: (Self, Key) => HeadU   )(implicit d: DI, d2: DI): HeadU    = zu(ForPathZU   (resolve(sel), (x, p) => f(x, p.key))) }

    // ===========================================================================
    def forPath(sel: ForPath.Selector) = new { import ForPath._
      def thn(f: (Self, KPath) => Self): Self = zz(
          ForPathZZ(resolve(sel), f)) } // TODO: test ok?

    // ===========================================================================
    def forEachKey(x1: KeyW)                       : _ForEachKey = new _ForEachKey(_.explicit(KeyWz.from(x1))) //TODO: keep?
    def forEachKey(x1: KeyW, x2: KeyW, more: KeyW*): _ForEachKey = new _ForEachKey(_.explicit(x1, x2, more:_*))
    def forEachKey(xs: KeyWz)                      : _ForEachKey = new _ForEachKey(_.explicit(xs))
    def forEachKey(sel: ForEachKey.Selector)       : _ForEachKey = new _ForEachKey(sel)

      // ---------------------------------------------------------------------------
      class _ForEachKey(sel: ForEachKey.Selector) {
        def thn(f: (Self, Key) => Self): Self = zz(
          ForPathsZZ(ForEachKey.resolve(sel), (z, p) => f(z, p.key /* ok by design */))) }

    // ===========================================================================
    def forEachPath(x1: KPathW)                           : _ForEachPath = new _ForEachPath(_.explicit(KPathWz.from(x1))) //TODO: keep?
    def forEachPath(x1: KPathW, x2: KPathW, more: KPathW*): _ForEachPath = new _ForEachPath(_.explicit(x1, x2, more:_*))
    def forEachPath(xs: KPathWz)                          : _ForEachPath = new _ForEachPath(_.explicit(xs))
    def forEachPath(sel: ForEachPath.Selector)            : _ForEachPath = new _ForEachPath(sel)

      // ---------------------------------------------------------------------------
      class _ForEachPath(sel: ForEachPath.Selector) {
          def thn(f: (Self, KPath) => Self): Self = zz(
            ForPathsZZ(ForEachPath.resolve(sel), (z, p) => f(z, p.value))) }

    // ===========================================================================
    // common

    def forAllKeys  (f: (Self, Key)   => Self): Self = forEachKey (_. allKeys) .thn(f)
    def forLeafPaths(f: (Self, KPath) => Self): Self = forEachPath(_.leafPaths).thn(f)
    def forAllPaths (f: (Self, KPath) => Self): Self = forEachPath(_. allPaths).thn(f)

    def forKeysMatching (pred: SKey  => Boolean) = forEachKey (_.filterKeys (pred))
    def forPathsMatching(pred: KPath => Boolean) = forEachPath(_.filterPaths(pred))
    //TODO: def unquoteValueIfApplicable = forAllKeys(_ unquoteValueIfApplicable _) // common enough?
  }

// ===========================================================================
