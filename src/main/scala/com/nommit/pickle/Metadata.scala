package com.nommit.pickle

import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.combinator.PackratParsers


object Metadata {
  val Empty = Metadata(Nil)
  def apply(m: Tagged*): Metadata = Metadata(m.toList)
}

case class Metadata private[pickle] (elements: List[Tagged])














