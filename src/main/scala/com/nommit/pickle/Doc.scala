package com.nommit.pickle

import collection.{IndexedSeqLike, TraversableLike}
import collection.immutable.{IndexedSeq, Vector}
import collection.generic.{SeqFactory, CanBuildFrom, HasNewBuilder}
import scalaz._
import annotation.tailrec

object Doc {
  def empty[S <: Section] = new Doc[S](Vector.empty)

  def text(s: String): Doc[Primitive] = new Doc(Vector(Primitive(s)))

  def tagged[S <: Section](s: Semantics, d: Doc[S]): Doc[Complex[S]] = new Doc(Vector(Complex(s, d)))

  def apply[S <: Section](s: S*): Doc[S] = new Doc(Vector(s: _*))

  def unapplySeq[S <: Section](doc: Doc[S]) = Some(doc.sections)

  def newBuilder[S <: Section] = Vector.newBuilder[S].mapResult(new Doc(_))

  implicit def canBuildFrom[S <: Section]: CanBuildFrom[Traversable[S], S, Doc[S]] = new CanBuildFrom[Traversable[S], S, Doc[S]] {
    override def apply() = newBuilder
    override def apply(from: Traversable[S]) = newBuilder ++= from
  }
}

class Doc[+S <: Section] private[pickle] (private[pickle] val sections: Vector[S]) extends IndexedSeq[S] with IndexedSeqLike[S, Doc[S]] {
  override protected[this] def newBuilder = Doc.newBuilder[S]

  def +:[SS >: S <: Section](s: SS): Doc[SS] = new Doc(s +: sections)
  def :+[SS >: S <: Section](s: SS): Doc[SS] = new Doc(sections :+ s)
  def ++[SS >: S <: Section](d: Doc[SS]): Doc[SS] = new Doc(sections ++ d.sections)
  def updated[SS >: S <: Section](idx: Int, s: SS): Doc[SS] = new Doc(sections.updated(idx, s))

  override def apply(idx: Int) = sections(idx)
  override def length = sections.length

  override def drop(n: Int) = new Doc(sections drop n)
  override def dropRight(n: Int) = new Doc(sections dropRight n)

  override def take(n: Int) = new Doc(sections take n)
  override def takeRight(n: Int) = new Doc(sections takeRight n)

  override def head = sections.head
  override def tail = new Doc(sections.tail)

  override def init = new Doc(sections.init)
  override def last = sections.last

  override def iterator = sections.iterator
  override def reverseIterator = sections.reverseIterator

  override def lengthCompare(len: Int) = sections lengthCompare len
  override def slice(from: Int, until: Int) = new Doc(sections.slice(from, until))

  override def splitAt(n: Int) = {
    val (left, right) = sections splitAt n
    (new Doc(left), new Doc(right))
  }

  //def split: Vector[Doc[S]] = {
  //  @tailrec def inSplit(lb: List[A], acc: Vector[Vector[B]]): Vector[Vector[B]] = lb match {
  //    case x :: xs => inSplit(xs, f(x).map(v => acc.init :+ (acc.last :+ v)).getOrElse(acc :+ Vector.empty))
  //    case Nil     => acc
  //  }

  //  inSplit(l, Vector.empty)
  //}
  def toMetadata(implicit ev: S <:< Complex[Section]) = new Metadata(sections.map(ev))
}


object Metadata {
  val Empty = new Metadata(Vector.empty)
  def apply[S <: Section](s: Complex[S]*): Metadata = new Metadata(Vector(s: _*))
  def unapplySeq(m: Metadata) = Doc.unapplySeq(m)
  def newBuilder = Vector.newBuilder[Complex[Section]].mapResult(new Metadata(_))
  implicit def canBuildFrom[S <: Section]: CanBuildFrom[Traversable[Complex[S]], Complex[S], Metadata] = new CanBuildFrom[Traversable[Complex[S]], Complex[S], Metadata] {
    override def apply() = newBuilder
    override def apply(from: Traversable[Complex[S]]) = newBuilder ++= from
  }
}

class Metadata private[pickle](private[pickle] override val sections: Vector[Complex[Section]]) extends Doc[Complex[Section]](sections) {
  override protected[this] def newBuilder = Metadata.newBuilder
  override def equals(other: Any) = other.isInstanceOf[Metadata] && super.equals(other)
}
