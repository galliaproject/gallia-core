package gallia
package meta

import aptus.{Anything_, Seq_}

// ===========================================================================
trait ClsHelper { ignored: Cls =>
  def field (ren  : Ren  ): Fld = field_(ren ).getOrElse(aptus.illegalState(ren             , keys.#@@))
  def field (path : RPath): Fld = field_(path).getOrElse(aptus.illegalState(path            , keys.#@@))
  def field (key  : Key  ): Fld = field_(key ).getOrElse(aptus.illegalState(key.name        , keys.#@@))
  def field (path : KPath): Fld = field_(path).getOrElse(aptus.illegalState(path.formatDebug, keys.#@@))

  // ---------------------------------------------------------------------------
  def field_(key  : Key  ): Option[Fld] = fields.find(_.key == key)
  def field_(ren  : Ren  ): Option[Fld] = fields.find(_.key == ren.from).map(_.updateKey(ren.to))

  // ---------------------------------------------------------------------------
  def field_(path : KPath): Option[Fld] =
    path.tailPair match {
      case (leaf  , None      ) => field_(leaf)
      case (parent, Some(tail)) => field_(parent).flatMap(_.nestedClassOpt).flatMap(_.field_(tail)) }

  def field_(path : RPath): Option[Fld] =
      path.tailPair match {
        case Left ( leaf         ) => field_(leaf)
        case Right((parent, tail)) => field_(parent).flatMap(_.nestedClassOpt).flatMap(_.field_(tail)) }

  // ===========================================================================
  def keyPair (key: Key): domain.KPair    = domain.KPair        (key, field(key).isOptional)
  def pathPair(key: Key): domain.PathPair = domain.PathPair.from(key, field(key).isOptional)
  // TODO: more alternatives

  // ===========================================================================
  def _leafPaths(parent: Seq[Key]): Seq[Seq[Key]] =
      fields
        .flatMap { f =>
          (parent :+ f.key)
            .pipe { current =>
              f.nestedClassOpt
                .map(_._leafPaths(current))
                .getOrElse(current.in.seq) } }

    // ---------------------------------------------------------------------------
    def _allPaths(parent: Seq[Key]): Seq[Seq[Key]] =
      fields
        .flatMap { f =>
          (parent :+ f.key)
            .pipe { current =>
              f.nestedClassOpt
                .map(_._allPaths(current))
                .getOrElse(Nil) ++
                Seq(current) /* add parent AFTER children */ } }

    // ---------------------------------------------------------------------------
    def _filter(parent: Seq[Key], pred: Fld => Boolean): Seq[Seq[Key]] =
      fields
        .flatMap { f =>
          (parent :+ f.key)
            .pipe { current =>
              f.nestedClassOpt match {
                case None    => if (pred(f)) Seq(current) else Nil
                case Some(c) => c._filter(current, pred) } } }

    // ---------------------------------------------------------------------------
    def _filterPNF(parent: Seq[Key], pred: PNF => Boolean): Seq[Seq[Key]] =
      fields
        .flatMap { f =>
          (parent :+ f.key)
            .pipe { current =>
              f.nestedClassOpt match {
                case None    => if (pred(f.toPNF(parent))) Seq(current) else Nil
                case Some(c) => c._filterPNF(current, pred) } } }

  // ===========================================================================
  def _rename(from: Key, to: Key): Cls = {
    requireNewKey(to)

    fields
      .mapIf(_.key == from) { _.updateKey(to) } // not worth checking if they're different (TODO: unless keeping track of actualy changes?)
      .pipe(rewrap)
  }

  // ---------------------------------------------------------------------------
  def _replace(key: Ren, info: Info): Cls = {
    requireKnownKey(key.from)

    fields
      .map { field =>
        if (field.key == key.from) Fld(key.to, info)
        else                       field }
      .pipe(rewrap)
  }

  // ---------------------------------------------------------------------------
  def _transformField(key: RenW)(f: Fld => Fld): Cls = {
    requireRenamingKey(key)

    fields
      .map { field =>
        if (field.key == key.from) f(field)
        else                       field }
      .pipe(rewrap)
  }

}

// ===========================================================================
