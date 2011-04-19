package com.nommit.pickle

import collection.{IndexedSeqLike, TraversableLike}
import collection.immutable.{IndexedSeq, Vector}
import collection.generic.{SeqFactory, CanBuildFrom, HasNewBuilder}
import scalaz._
import annotation.tailrec
import collection.mutable.Builder

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

  override def drop(n: Int) = new Doc[S](sections drop n)
  override def dropRight(n: Int) = new Doc[S](sections dropRight n)

  override def take(n: Int) = new Doc[S](sections take n)
  override def takeRight(n: Int) = new Doc[S](sections takeRight n)

  override def head = sections.head
  override def tail = new Doc(sections.tail)

  override def init = new Doc(sections.init)
  override def last = sections.last

  override def iterator = sections.iterator
  override def reverseIterator = sections.reverseIterator

  override def lengthCompare(len: Int) = sections lengthCompare len
  override def slice(from: Int, until: Int) = new Doc[S](sections.slice(from, until))

  override def splitAt(n: Int) = {
    val (left, right) = sections splitAt n
    (new Doc(left), new Doc(right))
  }

  def split(removeEmpty: Boolean = true): Vector[Doc[S]] = {
    @tailrec def _split(s: Vector[S], acc: Vector[Vector[S]]): Vector[Vector[S]] =  {
      if (s.isEmpty) acc
      else if (s.head == Separator) _split(s.tail, acc :+ Vector.empty[S])
      else _split(s.tail, acc.init :+ (acc.last :+ s.head))
    }

    _split(sections, Vector(Vector.empty[S])).filter(!_.isEmpty && removeEmpty).map(new Doc(_))
  }

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

trait Zipper[+S <: Section] extends Doc[S] { outer =>
  case class Edit(from: Int, to: Int, f: Doc[Section] => Section)

  /**
   * Vector of edits to the parent that must be applied to traverse upward
   */
  protected def edits:  Vector[Edit]
  protected def parent: Zipper[Section]

  def trim: Doc[S] = new Doc(sections)

  def up: Zipper[S]

  override def map[B, That](f: S => B)(implicit cbf: CanBuildFrom[Doc[S], B, That]) = cbf match {
    case cbf: ZipperCBF[Doc[Section], B, That] => {
      val builder = cbf.builder(parent, edits)
      builder ++= (sections.map(f))
      builder.result
    }

    case _ => super.map(f)(cbf)
  }

  override def updated[B >: S <: Section](index: Int, section: B) = {
    new Doc[B](super.sections.updated(index, section)) with Zipper[B] {
      override val edits = outer.edits
      val parent = outer.parent
    }
  }
}

trait ZipperCBF[-From, -Elem, To] extends CanBuildFrom[From, Elem, To] {
  def builder(from: From, edits: Vector[Zipper[_]#Edit]): Builder[Elem, To]
}

