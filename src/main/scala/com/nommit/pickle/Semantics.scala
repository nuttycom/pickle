package com.nommit.pickle

sealed trait Semantics {
  def metadata: Metadata
}

case class Tag(ident: String, metadata: Metadata = Metadata.Empty) extends Semantics

case class MetaTag[+S <: Section](ident: Doc[S], metadata: Metadata = Metadata.Empty) extends Semantics






