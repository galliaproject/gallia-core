package gallia
package whatever

import java.time.{LocalDate, LocalDateTime}

// ===========================================================================
object WhateverImplicits {

  @TypeMatching
  private[gallia] implicit class Any_(u: Any) {    
    def number         = u match { case x: Number         => x; case _ => dataError(error(u, "210113130852", classOf[Number])) }

    // ---------------------------------------------------------------------------
    def boolean        = u match { case x: Boolean        => x; case _ => dataError(error(u, "210113130850", classOf[Boolean])) }
    def string         = u match { case x: String         => x; case _ => dataError(error(u, "210113130851", classOf[String]))  }
    def int            = u match { case x: Int            => x; case _ => dataError(error(u, "210112140145", classOf[Int]))    }
    def long           = u match { case x: Long           => x; case _ => dataError(error(u, "210113130857", classOf[Long]))  }
    def double         = u match { case x: Double         => x; case _ => dataError(error(u, "210113130854", classOf[Double])) }
    // note: no support for byte/short/float (see 220318114510 whatever.md@docs)

    // ---------------------------------------------------------------------------
    def bigInt         = u match { case x: BigInt         => x; case _ => dataError(error(u, "210113130859", classOf[BigInt]))  }
    def bigDec         = u match { case x: BigDec         => x; case _ => dataError(error(u, "210113130900", classOf[BigDecimal])) }

    // ---------------------------------------------------------------------------
    def localDate      = u match { case x: LocalDate      => x; case _ => dataError(error(u, "210113130901", classOf[LocalDate]))  }
    def localTime      = u match { case x: LocalTime      => x; case _ => dataError(error(u, "210113130903", classOf[LocalTime])) }
    def localDateTime  = u match { case x: LocalDateTime  => x; case _ => dataError(error(u, "210113130902", classOf[LocalDateTime])) }
    def offsetDateTime = u match { case x: OffsetDateTime => x; case _ => dataError(error(u, "210113130904", classOf[LocalDateTime])) }
    def zonedDateTime  = u match { case x: ZonedDateTime  => x; case _ => dataError(error(u, "210113130905", classOf[LocalDateTime])) }
    def instant        = u match { case x: Instant        => x; case _ => dataError(error(u, "210113130906", classOf[LocalDateTime])) }

    // ---------------------------------------------------------------------------
    def binary         = u match { case x: ByteBuffer     => x; case _ => dataError(error(u, "210113130907", classOf[ByteBuffer])) }
    
    // ---------------------------------------------------------------------------
    def enm            = u match { case x: EnumValue      => x; case _ => dataError(error(u, "210113130908", classOf[EnumValue])) }

    // ---------------------------------------------------------------------------
    def seq            = u match { case x: Seq[_]         => x; case _ => dataError(error(u, "210113130909", classOf[Seq[_]])) }
  }

  // ===========================================================================
  private def error(value: Any, id: String, klass: Class[_]) = s"TODO:expected ${klass}:${id}:${value}:${value.getClass}:${value}"  
}

// ===========================================================================
