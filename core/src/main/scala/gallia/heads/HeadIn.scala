package gallia
package heads

import aptus.Nes
import aptus.Separator

// ===========================================================================
trait HeadUIn { // TODO: t210114201159 - implement missing convenience creation methods below
  def content(value: Any): HeadU = ???

  // ---------------------------------------------------------------------------
  def from[T         ](a: Key                )(value:  T          ): HeadU = ???
  def from[T1, T2    ](a: Key, b: Key        )(tuple: (T1, T2    )): HeadU = ???
  def from[T1, T2, T3](a: Key, b: Key, c: Key)(tuple: (T1, T2, T3)): HeadU = ???

  def from[T]             (keys: Nes[Key])            (values: Nes[T]): HeadU = ???
  def from(sep: Separator)(a: Key, b: Key, more: Key*)(value: String /* eg "foo:3:true" */): HeadU = ???
}

// ===========================================================================
trait HeadZIn { // TODO: t210114201159 - implement missing convenience creation methods below
  def lines(value1: AnyValue, more: AnyValue*): HeadZ = ???

  // ---------------------------------------------------------------------------
  def from[T: WTT](key: Key, values: Iterable[T]): HeadZ = ??? // TODO: from init: .union(Objs.from('symbol, values = a ++ b))

  /** must all be non-empty, and same size or size 1 (will repeat the value) */
  def from(entry1: (Key, Seq[AnyValue]), more: (Key, Seq[AnyValue])*): HeadZ = ???
  def from(key1: Key, more: Key*)(values: Seq[Any]*)                 : HeadZ = ???

  def from(keys: Seq[Key], values: Seq[Seq[AnyValue]]): HeadZ = ???
  def from(entries: Seq[(Key, Seq[AnyValue])])        : HeadZ = ???
}

// ===========================================================================
