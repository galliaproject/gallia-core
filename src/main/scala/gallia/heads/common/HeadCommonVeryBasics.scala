package gallia
package heads
package common

import aptus.String_

import domain._
import actions.common.ActionsCommonVeryBasics._

// ===========================================================================
trait HeadCommonVeryBasics[F <: HeadCommon[F]] { ignored: HeadCommon[F] =>

  // ===========================================================================
  // keys reordering

  def reorderKeys           (f: Seq[SKey] => Seq[SKey]): Self2 = self2 :+ ReorderKeys(f, recursively = false)
  def reorderKeysRecursively(f: Seq[SKey] => Seq[SKey]): Self2 = self2 :+ ReorderKeys(f, recursively = true )

  // ---------------------------------------------------------------------------
  def reorderAsFirstKeys(target1: KeyW, more: KeyW*): Self2 = reorderAsFirstKeys(_.explicit(target1 -> more)) 
  def reorderAsFirstKeys(targets: KeyWz)            : Self2 = reorderAsFirstKeys(_.explicit(targets))
  def reorderAsFirstKeys(selector: SEL.ReorderAsX.Selector): Self2 = 
    self2 :+ ReorderSelectedKeys(SEL.ReorderAsX.resolve(selector), f = (allKeys, targetKeys) => targetKeys ++ allKeys.diff(targetKeys))

  // ---------------------------------------------------------------------------
  def reorderAsLastKeys(target1: KeyW, more: KeyW*): Self2 = reorderAsLastKeys(_.explicit(target1 -> more)) 
  def reorderAsLastKeys(targets: KeyWz)            : Self2 = reorderAsLastKeys(_.explicit(targets))    
  def reorderAsLastKeys(selector: SEL.ReorderAsX.Selector): Self2 = 
    self2 :+ ReorderSelectedKeys(SEL.ReorderAsX.resolve(selector), f = (allKeys, targetKeys) => allKeys.diff(targetKeys) ++ targetKeys.reverse)

  // ===========================================================================
  // rename (explicitly, aot dynamically); selection: use forX

  def rename(x: KPathW) = new _Rename(x) // 2-step version

    // ---------------------------------------------------------------------------
    class _Rename (x: KPathW) {
      def using(f: SKey => SKey): Self2 = rename(x).to(x.value.skey.pipe(f).symbol)
      def to   (y: KeyW)        : Self2 = self2 :+ new Rename(RPathz(Seq(x.rpath(y)))) }

  // ---------------------------------------------------------------------------
  def rename(x: ActualRPathW )                                       : Self2 = self2 :+ new Rename(x.rpathz)
  def rename(x: ActualRPathWz)                                       : Self2 = self2 :+ new Rename(RPathz(x.values.map(_.value)))
  def rename(x1: ActualRPathW, x2: ActualRPathW, more: ActualRPathW*): Self2 = self2 :+ new Rename(RPathz((Seq(x1, x2) ++ more).map(_.value)))

  // ---------------------------------------------------------------------------
  // TODO: validate no duplicates...
  def rename(mapping : Map[KeyW, KeyW])                 : Self2 = self2 :+ new Rename(toRPathz(mapping))
  def rename(mapping : Map[SKey, SKey])(implicit di: DI): Self2 = self2 :+ new Rename(toRPathz(mapping)) // to prevent going to the function counterpart

    // ---------------------------------------------------------------------------
    //TODO: to utils?
    private def toRPathz(mapping : Map[KeyW, KeyW])                   : RPathz = RPathz(mapping.toSeq.map(x => RPath.from(x._1.value, x._2.value)))
    private def toRPathz(mapping : Map[SKey , SKey ])(implicit di: DI): RPathz = RPathz(mapping.toSeq.map(x => RPath.from(x._1.symbol, x._2.symbol)))

  // ---------------------------------------------------------------------------
  def rename           (modifier: SKey => SKey): Self2 = self2 :+ new RenameDynamically(modifier, recursively = false)
  def renameRecursively(modifier: SKey => SKey): Self2 = self2 :+ new RenameDynamically(modifier, recursively = true )

  // ===========================================================================
  // remove

  def remove(target1: KPathW, more: KPathW*): Self2 = remove(_.explicit(target1 -> more))
  def remove(targets: KPathWz)              : Self2 = remove(_.explicit(targets))
  def remove(selector: SEL.Remove.Selector) : Self2 = self2 :+ new Remove(SEL.Remove.resolve(selector))
  
  // ===========================================================================
  // retain

  def retain(target1: RPathW, more: RPathW*): Self2 = retain(_.explicit(target1 -> more))
  def retain(targets: RPathWz)              : Self2 = retain(_.explicit(targets))
  def retain(selector: SEL.Retain.Selector) : Self2 = self2 :+ new Retain(SEL.Retain.resolve(selector))

  // ===========================================================================
  // add

  // - t210111113206 - support adding path, eg: .add('p |> 'f -> "foo")
  // - t210127194716 - p2 - also support append/prepend field(s); maybe generalized as insertion index (including negative)?
  // - t210125111338 - investigate union types to restrict T at compile-time (coming in scala 3?)

  def add[T1: WTT](e1: KVE): Self2 = self2 :+ new Add(e1)

    def add[T1: WTT, T2: WTT                           ](e1: KVE, e2: KVE                           ): Self2 = self2 :+ new Add(e1, e2)
    def add[T1: WTT, T2: WTT, T3: WTT                  ](e1: KVE, e2: KVE, e3: KVE                  ): Self2 = self2 :+ new Add(e1, e2, e3)
    def add[T1: WTT, T2: WTT, T3: WTT, T4: WTT         ](e1: KVE, e2: KVE, e3: KVE, e4: KVE         ): Self2 = self2 :+ new Add(e1, e2, e3, e4)
    def add[T1: WTT, T2: WTT, T3: WTT, T4: WTT, T5: WTT](e1: KVE, e2: KVE, e3: KVE, e4: KVE, e5: KVE): Self2 = self2 :+ new Add(e1, e2, e3, e4, e5)

  // ===========================================================================
  // replace
  // TODO:
  // - selection too (t210110094731)?
  // - t210111113206 - support adding path, eg: .replace('p |> 'f -> "foo")
  // - t210125111338 - investigate union types to restrict T at compile-time (coming in scala 3?)

  def replace[T1: WTT](e1: RVE): Self2 = self2 :+ new Replace(e1)

    def replace[T1: WTT, T2: WTT                           ](e1: RVE, e2: RVE                           ): Self2 = self2 :+ new Replace(e1, e2)
    def replace[T1: WTT, T2: WTT, T3: WTT                  ](e1: RVE, e2: RVE, e3: RVE                  ): Self2 = self2 :+ new Replace(e1, e2, e3)
    def replace[T1: WTT, T2: WTT, T3: WTT, T4: WTT         ](e1: RVE, e2: RVE, e3: RVE, e4: RVE         ): Self2 = self2 :+ new Replace(e1, e2, e3, e4)
    def replace[T1: WTT, T2: WTT, T3: WTT, T4: WTT, T5: WTT](e1: RVE, e2: RVE, e3: RVE, e4: RVE, e5: RVE): Self2 = self2 :+ new Replace(e1, e2, e3, e4, e5)

  // ===========================================================================
  // shorthands

  def reverseKeyOrder           : Self2 = reorderKeys           (_.reverse)
  def reverseKeyOrderRecursively: Self2 = reorderKeysRecursively(_.reverse)
  
  // ---------------------------------------------------------------------------
  def renameToUpperCase(x: KPathW): Self2 = rename(x).using(_.toUpperCase)
  def renameToLowerCase(x: KPathW): Self2 = rename(x).using(_.toLowerCase)
  def renameToSnakeCase(x: KPathW): Self2 = rename(x).using(_.camelCaseToSnake)
  def renameToCamelCase(x: KPathW): Self2 = rename(x).using(_.snakeToCamelCase)

  def renameToDoubleQuoted(x: KPathW): Self2 = ???
  def renameToUnquoted    (x: KPathW): Self2 = ???

  // ---------------------------------------------------------------------------
  def addId    [T: WTT](value: T): Self2 = add    (_id -> value) // TODO: t210408130938 - as first key rather
  def replaceId[T: WTT](value: T): Self2 = replace(_id -> value)
  // TODO: offert shofthand for the convert to double/int since so common?
}

// ===========================================================================
