package com.nommit.pickle

import scala.collection.IndexedSeqLike
import scala.collection.generic.SeqFactory
import scala.collection.immutable.Vector

object Metadata {
  val Empty = new Metadata(Vector.empty)
  def apply(m: Tagged*): Metadata = new Metadata(Vector(m: _*))
  def unapplySeq(m: Metadata) = Some(m.elems)
  def newBuilder = Vector.newBuilder[Tagged].mapResult(new Metadata(_))
}

class Metadata private[pickle] (private[pickle] val elems: Vector[Tagged]) extends IndexedSeq[Tagged] with IndexedSeqLike[Tagged, Metadata] {
  override protected[this] def newBuilder = Metadata.newBuilder

  override def apply(idx: Int) = elems(idx)

  override def length = elems.length
}














