package com.nommit.pickle

object Doc {
  val Empty = Doc(Nil)
  def text(s: String) = Doc(List(Text(s)))
  def tagged(t: Semantics, d: Doc) = Doc(List(Tagged(t, d)))
  def apply(s: Section*): Doc = Doc(s.toList)
}

case class Doc private[pickle] (sections: List[Section])
















