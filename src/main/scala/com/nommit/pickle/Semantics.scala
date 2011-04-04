package com.nommit.pickle

import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.combinator.PackratParsers


sealed trait Semantics {
  def metadata: Metadata
}

case class Tag(ident: String, metadata: Metadata = Metadata.Empty) extends Semantics
case class MetaTag(ident: Doc,  metadata: Metadata = Metadata.Empty) extends Semantics






