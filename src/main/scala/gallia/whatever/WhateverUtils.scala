package gallia
package whatever

import aptus.Seq_

// ===========================================================================
object WhateverUtils {
  
  private[whatever] def formatDefault(value: Any): String =
    value match {

      // legit?
      case None           => "None"
      case Nil            => "Nil"

      case Some(value)    => s"Some(${          data.DataDynamicFormatting.formatBasicValue(value)})"
      case values: Seq[_] => s"Seq(${values.map(data.DataDynamicFormatting.formatBasicValue).join(",")})"

      case value          =>                    data.DataDynamicFormatting.formatBasicValue(value) }

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
