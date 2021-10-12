package gallia
package whatever

import java.time.{LocalDate, LocalDateTime}

// ===========================================================================
object WhateverImplicits {

  @TypeMatching
  private[gallia] implicit class Any_(u: Any) {    
    def number     = u match { case x: Number        => x; case _ => dataError(error(u, "210113130852", classOf[Number])) }

    // ---------------------------------------------------------------------------
    def boolean    = u match { case x: Boolean       => x; case _ => dataError(error(u, "210113130850", classOf[Boolean])) }
    def string     = u match { case x: String        => x; case _ => dataError(error(u, "210113130851", classOf[String]))  }

    def int        = u match { case x: Int           => x; case _ => dataError(error(u, "210112140145", classOf[Int]))    }
    def double     = u match { case x: Double        => x; case _ => dataError(error(u, "210113130854", classOf[Double])) }

    def byte       = u match { case x: Byte          => x; case _ => dataError(error(u, "210113130855", classOf[Byte]))  }
    def short      = u match { case x: Short         => x; case _ => dataError(error(u, "210113130856", classOf[Short])) }
    def long       = u match { case x: Long          => x; case _ => dataError(error(u, "210113130857", classOf[Long]))  }
    def float      = u match { case x: Float         => x; case _ => dataError(error(u, "210113130858", classOf[Float])) }

    def bigInt     = u match { case x: BigInt        => x; case _ => dataError(error(u, "210113130859", classOf[BigInt]))  }
    def bigDecimal = u match { case x: BigDecimal    => x; case _ => dataError(error(u, "210113130900", classOf[BigDecimal])) }

    def date       = u match { case x: LocalDate     => x; case _ => dataError(error(u, "210113130901", classOf[LocalDate]))  }
    def dateTime   = u match { case x: LocalDateTime => x; case _ => dataError(error(u, "210113130902", classOf[LocalDateTime])) }

    // TODO: enum - t210201095414
  }

  // ---------------------------------------------------------------------------
  private def error(value: Any, id: String, klass: Class[_]) = s"TODO:expected ${klass}:${id}:${value}:${value.getClass}"  
}

// ===========================================================================
