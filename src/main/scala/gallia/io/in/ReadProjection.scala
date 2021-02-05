package gallia.io.in

import aptus.Anything_
import aptus.MirrorIndex

import gallia._

// ===========================================================================
sealed trait ReadProjection {
    def resolve(allKeys: Keyz): Renz
  }

  // ---------------------------------------------------------------------------
  object ReadProjection {
    case class Retainees1(targets: RPathz) extends ReadProjection {
      def resolve(ignored: Keyz): Renz = targets.forceRenzFX
    }

    // ---------------------------------------------------------------------------
    case class Removees1 (targets: KPathz) extends ReadProjection {
      def resolve(allKeys: Keyz): Renz = targets.forceKeyzFX.values.thn(allKeys.values.diff).thn(Keyz.apply).renz
    }

    // ---------------------------------------------------------------------------
    case class Retainees2(targets: MIndexEntries) extends ReadProjection {
      def resolve(allKeys: Keyz): Renz = targets.values.map(_.resolve(allKeys)).thn(Renz.apply)
    }

    // ---------------------------------------------------------------------------
    case class Removees2 (targets: Seq[MirrorIndex]) extends ReadProjection {
      def resolve(allKeys: Keyz): Renz = targets.map(allKeys.values.apply).thn(allKeys.values.diff).thn(Keyz.apply).renz
    }
  }

// ===========================================================================
