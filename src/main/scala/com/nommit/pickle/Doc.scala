package com.nommit.pickle

import collection.{IndexedSeqLike, TraversableLike}
import collection.generic.{SeqFactory, CanBuildFrom, HasNewBuilder}
import scalaz._
import annotation.tailrec
import util.BloomFilter
import collection.immutable.{VectorBuilder, IndexedSeq, Vector}
import collection.mutable.{ArrayBuffer, Builder}

object Doc {
  def empty[S <: Section] = new Doc[S](Vector.empty)

  def text(s: String): Doc[Primitive] = new Doc(Vector(Primitive(s)))

  def tagged[S <: Section](s: Semantics, d: Doc[S]): Doc[Complex[S]] = new Doc(Vector(Complex(s, d)))

  def apply[S <: Section](s: S*): Doc[S] = new Doc(Vector(s: _*))

  def unapplySeq[S <: Section](doc: Doc[S]) = Some(doc.sections)

  def newBuilder[S <: Section] = Vector.newBuilder[S].mapResult(new Doc(_))

  implicit def zipperCBF[S <: Section]: ZipperCBF[Traversable[_], S, Zipper[S]] = new ZipperCBF[Traversable[_], S, Zipper[S]] {
    override def builder(from: Traversable[_], priorEdits: => Vector[Zipper.Edit]): Builder[S, Zipper[S]] = Vector.newBuilder[S].mapResult {
      sections => new Doc(sections) with Zipper[S] {
        lazy val edits = priorEdits

        lazy val parent = from match {
          case doc: Doc[Section] => doc.zipper
          case _ => error("No zipper context available")
        }
      }
    }

    override def builder(priorEdits: => Vector[Zipper.Edit]): Builder[S, Zipper[S]] =  Vector.newBuilder[S].mapResult {
      sections => new Doc(sections) with Zipper[S] {
        lazy val edits = priorEdits
        def parent = error("No zipper context available")
      }
    }
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

  private lazy val bloomFilter: BloomFilter = sections.foldLeft(BloomFilter.empty) {
    case (bf, Complex(Tag(ident, metadata), doc)) => bf.append(doc.bloomFilter, ident) ++ metadata.bloomFilter
    case (bf, Complex(MetaTag(ident, metadata), doc)) => bf ++ ident.bloomFilter ++ metadata.bloomFilter ++ doc.bloomFilter
    case (bf, _) => bf
  }

  def matches(s: Selector[_, _]) = s.characteristic.forall(bloomFilter.contains)

	private def

	def |[B, That <: Traversable[B]](selector: Selector[B, That])(implicit cbf: ZipperCBF[Zipper[S], B, That]): That = {
	}

  def >[B, That <: Traversable[B]](selector: Selector[B, That])(implicit cbf: ZipperCBF[Zipper[S], B, That]): That = {
    import Zipper._
		val selected = new VectorBuilder[B]
		val chunks   = new VectorBuilder[Int]
		val editors  = new VectorBuilder[Doc[Section] => Section]

		sections.foreach {
			case section @ Complex(_, child) => {
				val selectedChildOffsets = new ArrayBuffer[Int](child.length)
				var selectedCount = 0

				var childIndex = 0
				for (section <- child) {
					if (selector isDefinedAt section) {
						selected += selector(section)
						selectedCount += 1
						selectedChildOffsets += childIndex
					}
					childIndex += 1
				}

				chunks   += selectedCount
				editors += (
					(replacements: Doc[Section]) => complex.copy(
						doc = Doc(
							(selectedChildOffsets zip replacements).foldLeft(child.sections) {
								case (vec, (i, r)) => vec.updated(i, r)
							}: _*
						)
					)
				)
			}

			case _ =>
		}

		lazy val (_, edits) = {
			(chunks.result zip editors.result).foldLeft((0, Vector[Edit]())) {
				case ((i, acc), (length, f)) if length != 0 => (i + length, acc :+ Edit(i, i + length, f))
				case ((i, acc), _)                          => (i, acc)
			}
		}

		val builder = cbf.builder(zipper, edits)
		builder ++= selected.result
		builder.result
  }

  protected def zipper: Zipper[S] = this match {
		case z: Zipper[S] => z
		case _ => new Doc(sections) with Zipper[S] {
			override val edits = Vector()
			override def parent = error("Attempted to move up at root of the tree")
		}
	}
}

trait Selector[+A, +Coll <: Traversable[A]] extends PartialFunction[Section, A] {
  def characteristic: Option[String]
}

object Selector {
  implicit def sym(s: Symbol): Selector[Section, Zipper[Section]] = str(s.name)

  implicit def str(ident: String): Selector[Section, Zipper[Section]] = new Selector[Section, Zipper[Section]] {
    val tagMatch: PartialFunction[Section, Section] = {
      case c @ Complex(Tag(`ident`, _), _) => c
    }

    override def characteristic = Some(ident)
    override def apply(s: Section) = tagMatch(s)
    override def isDefinedAt(s: Section) = tagMatch.isDefinedAt(s)
  }
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

object Zipper {
  case class Edit(from: Int, to: Int, rebuild: Doc[Section] => Section)
}

trait Zipper[+S <: Section] extends Doc[S] { outer =>
  import Zipper._

  /**
   * Vector of edits to the parent that must be applied to traverse upward
   */
  protected def edits:  Vector[Edit]
  protected def parent: Zipper[Section]

  def trim: Doc[S] = new Doc(sections)

  def up: Zipper[S] = {
    // fold each edit into its location in the parent sections. 
    val (_, sections) = edits.foldLeft((0, parent.sections)) {
      case ((i, replacements), Edit(from, to, rebuild)) =>
        (i + 1, if (replacements(i).isInstanceOf[Section]) replacements.updated(i, rebuild(outer.slice(from, to))) else replacements)
    }

    new Doc(sections) with Zipper[S] {
      override val edits  = outer.parent.edits
      override val parent = outer.parent.parent
    }
  }

  override def map[B, That](f: S => B)(implicit cbf: CanBuildFrom[Doc[S], B, That]) = cbf match {
    case cbf: ZipperCBF[Doc[Section], B, That] => {
      val builder = cbf.builder(parent, edits)
      builder ++= (sections.map(f))
      builder.result
    }

    case _ => super.map(f)(cbf)
  }

  override def updated[B >: S <: Section](index: Int, section: B) = {
    new Doc[B](sections.updated(index, section)) with Zipper[B] {
      override val edits = outer.edits
      override val parent = outer.parent
    }
  }
}

trait ZipperCBF[-From, -Elem, To] extends CanBuildFrom[From, Elem, To] {
  def builder(from: From, edits: => Vector[Zipper.Edit]): Builder[Elem, To]
  override def apply(from: From): Builder[Elem, To] = builder(from, Vector.empty[Zipper.Edit])

  def builder(edits: => Vector[Zipper.Edit]): Builder[Elem, To]
  override def apply(): Builder[Elem, To] = builder(Vector.empty[Zipper.Edit])
}

object ZipperCBF {
  implicit def identity[From, Elem, To](implicit cbf: CanBuildFrom[From, Elem, To]): ZipperCBF[From, Elem, To] = new ZipperCBF[From, Elem, To] {
    override def builder(from: From, edits: => Vector[Zipper.Edit]) = cbf(from)
    override def builder(edits: => Vector[Zipper.Edit]) = cbf()
  }
}
