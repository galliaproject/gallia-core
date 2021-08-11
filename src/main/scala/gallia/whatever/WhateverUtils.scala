package gallia.whatever

import aptus.Seq_
import aptus.Int_
import aptus.Double_

// ===========================================================================
object WhateverUtils {
  
  private[whatever] def formatDefault(value: Any): String =
      value match {

        // legit?
        case None           => "None"
        case Nil            => "Nil"

        case Some(value)    => s"Some(${formatIndividualValue(value)})"
        case values: Seq[_] => s"Seq(${values.map(formatIndividualValue).join(",")})"

        case value          => formatIndividualValue(value) }

      // ---------------------------------------------------------------------------
      private def formatIndividualValue(value: Any) = value match {
          case x: Int    => x.formatExplicit
          case x: Long   => x.formatExplicit
          case x: Double => x.formatExplicit
          //TODO: t210202160709 - more
          case x         => x.toString }
      
  // ===========================================================================
  private[whatever] def size(value: Any): Int =
    value match {
      case x: Whatever => size(x.any)      
      case None | Nil  => 0
      case y: Some[_]  => y.size
      case y: Seq [_]  => y.size
      case y           => 1 }

}

// ===========================================================================
