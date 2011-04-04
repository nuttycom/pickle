package com.nommit.pickle

import collection.{IndexedSeqLike, TraversableLike}
import collection.immutable.{IndexedSeq, Vector}
import collection.generic.{SeqFactory, CanBuildFrom, HasNewBuilder}

object Doc {
  val Empty = new Doc(Vector.empty)
  def text(s: String) = new Doc(Vector(Text(s)))
  def tagged(t: Semantics, d: Doc) = new Doc(Vector(Tagged(t, d)))
  def apply(s: Section*): Doc = new Doc(Vector(s: _*))
  def unapplySeq(doc: Doc) = Some(doc.sections)
  def newBuilder = Vector.newBuilder[Section].mapResult(new Doc(_))
}

class Doc private[pickle] (private[pickle] val sections: Vector[Section]) extends IndexedSeq[Section] with IndexedSeqLike[Section, Doc] {
  override protected[this] def newBuilder = Doc.newBuilder

  override def apply(idx: Int) = sections(idx)

  override def length = sections.length
}
















