package com.nommit.pickle

import util.BloomFilter

import collection.{IndexedSeqLike, TraversableLike}
import collection.generic.{SeqFactory, CanBuildFrom, HasNewBuilder}
import scalaz._
import Scalaz._
import annotation.tailrec
import collection.immutable.{VectorBuilder, IndexedSeq, Vector}
import collection.mutable.{ArrayBuffer, Builder}


object Doc {
  def empty[S <: Section] = new Doc[S](Vector.empty)

  def text(s: String*): Doc[Primitive] = new Doc(Vector(s.map(Primitive(_)): _*))

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

  implicit def DocMonoid[S <: Section]: Monoid[Doc[S]] = new Monoid[Doc[S]] {
    val zero: Doc[S] = Doc.empty[S]
    def append(s1: Doc[S], s2: => Doc[S]): Doc[S] = s1 ++ s2
  }

  implicit def DocReducer[S <: Section]: Reducer[S, Doc[S]] = new Reducer[S, Doc[S]] {
    override def cons(s: S, d: Doc[S]) = s +: d
    override def unit(s: S) = Doc(s)
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

  private[pickle] lazy val bloomFilters: Vector[BloomFilter] = sections.foldLeft(Vector.empty[BloomFilter]) {
    (filters, section) => filters.zipAll(section.bloomFilters, BloomFilter.Empty, BloomFilter.Empty).map {
      case (f1, f2) => f1 ++ f2
    }
  }

  def matches(s: Selector) = (s.characteristics zip bloomFilters).forall {
    case (v, filter) => filter.contains(v)
  }

  def select(traverse: Traverse) = traverse(this)

  def zipper: Zipper[S] = this match {
		case z: Zipper[S] => z
		case _ => new Doc(sections) with Zipper[S] {
			override val edits = Vector()
			override def parent = error("Attempted to move up at root of the tree")
		}
	}

  def complexity: Int = sections.map(_.complexity).sum
  def inline = complexity <= 10

  def prettyPrint(depth: Int = 0): String = {
    val prefix = " " * (depth * 2)
    def mdoc(m: Metadata) = if (m.isEmpty) "" else " # " + m.prettyPrint(depth + 1)

    val subdocs = split(true).map{
      subdoc =>
        def line(s: String) = if (subdoc.inline) s else prefix + s
        (
          subdoc.inline,
          subdoc.sections.map {
            case Primitive(s) => line(s)
            case s: Separator.type => line(if (inline) "|" else "@@")
            case complex @ Complex(Tag(ident, metadata), doc) =>
              if (complex.complexity <= 10) {
                line("@" + ident + "[" + doc.prettyPrint(depth + 1) + mdoc(metadata) + "]")
              } else {
                line("@[" + ident + mdoc(metadata) + "]\n" + doc.prettyPrint(depth + 1) + "\n@/")
              }
          }
        )
    }

    subdocs.map{ case (inline, d) => d.mkString(if (inline) "" else "\n")}.mkString(if (inline) "" else "\n")
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
  /**
   * @from the index of the zipper to slice from
   * @to the index of the zipper to slice to
   * @rebuild a function from the slice of child nodes in the zipper to the rebuilt parent element
   * @childMap a map from index of the element that spawned the slice of child nodes to the indices within that
   *           slice of children to use in the reconstruction of the parent
   */
  case class Edit(from: Int, to: Int, rebuild: (Doc[Section], Map[Int, Set[Int]]) => Doc[Section], childMap: Map[Int, Set[Int]])
}

trait Zipper[+S <: Section] extends Doc[S] { outer =>
  import Zipper._

  /**
   * Vector of edits to the parent that must be applied to traverse upward
   */
  protected def edits:  Vector[Edit]
  protected def parent: Zipper[Section]

  def trim: Doc[S] = new Doc(sections)

  def unselect: Zipper[S] = {
    // fold each edit into its location in the parent sections. 
    val sections = (edits zip parent.sections).foldLeft(Vector.empty[Section]) {
      case (acc, (Edit(from, to, _, _), _: Complex[Section])) if from == to => acc
      case (acc, (Edit(from, to, rebuild, childMap), _: Complex[Section]))  => acc ++ rebuild(this.slice(from, to), childMap)
      case (acc, (_, s))                                                    => acc :+ s
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
  def apply(from: From): Builder[Elem, To] = builder(from, Vector.empty[Zipper.Edit])

  def builder(edits: => Vector[Zipper.Edit]): Builder[Elem, To]
  def apply(): Builder[Elem, To] = builder(Vector.empty[Zipper.Edit])
}

object ZipperCBF {
  implicit def identity[From, Elem, To](implicit cbf: CanBuildFrom[From, Elem, To]): ZipperCBF[From, Elem, To] = new ZipperCBF[From, Elem, To] {
    override def builder(from: From, edits: => Vector[Zipper.Edit]) = cbf(from)
    override def builder(edits: => Vector[Zipper.Edit]) = cbf()
  }
}

object Selector {
  implicit def sym(s: Symbol): Selector = str(s.name)

  implicit def str(ident: String): Selector = new Selector {
    override def characteristics = Vector(ident)
    override def apply(s: Section) = s match {
      case Complex(Tag(`ident`, _), _) => true
      case _ => false
    }
  }
}

trait Selector extends (Section => Boolean) {
  /**
   * A vector of values to be checked against the bloom filters of a document, where
   * each successive value in the vector corresponds to another layer of metadata. Hence,
   * the 0th element of the vector is the identifier of the node that this selector is to select;
   * the 1st element is the identifier of a metadata element which
   */
  def characteristics: Vector[Any]
}

object Traverse {
  import Zipper.Edit

  implicit def sym(s: Symbol): Traverse = filter(Selector.sym(s))
  implicit def str(s: String): Traverse = filter(Selector.str(s))

  def filter(selector: Selector)(implicit cbf: ZipperCBF[Zipper[Section], Section, Zipper[Section]]): Traverse = new Traverse {
    override def apply(doc: Doc[Section]) = {
      if (doc.matches(selector)) {
        val selected = new VectorBuilder[Section]
        val chunks   = new VectorBuilder[Int]
        val editors  = new VectorBuilder[(Doc[Section], Map[Int, Set[Int]]) => Doc[Section]]

        doc.sections.zipWithIndex.foreach {
          case (section: Complex[Section], i) if selector(section) =>
            selected += section
            chunks += 1
            editors += ((replacements: Doc[Section], map: Map[Int, Set[Int]]) => replacements)

          case (_, i) => chunks += 0
        }

        lazy val (_, edits) = {
          (chunks.result zip editors.result).foldLeft((0, Vector[Edit]())) {
            // i:       index of an element in the rebuilt group
            // acc:     the vector of contexts that will be used to construct the new parent element on unselect
            // length:  the number of children retained in the resulting zipper from the indexed element
            // f:       the function that will rebuild the indexed element on unselect
            case ((i, acc), (length, f)) if length > 0 => (i + length, acc :+ Edit(i, i + length, f, Map.empty))
            case ((i, acc), _)                         => (i, acc)
          }
        }

        val builder = cbf.builder(doc.zipper, edits)
        builder ++= selected.result
        builder.result
      } else {
        cbf.builder(Vector.empty).result
      }
    }
  }
}

trait Traverse extends (Doc[Section] => Zipper[Section]) { parent =>
  import Zipper.Edit
  import Traverse._

  /**
   * A filtering operation that does not descend into the document tree, but instead just selects some
   * members of the parent document.
   */
  def % (selector: Selector)(implicit cbf: ZipperCBF[Zipper[Section], Section, Zipper[Section]]): Traverse = new Traverse {
    override def apply(doc: Doc[Section]) = filter(selector).apply(parent(doc))
  }

  def > (selector: Selector)(implicit cbf: ZipperCBF[Zipper[Section], Section, Zipper[Section]]): Traverse = new Traverse {
    override def apply(doc: Doc[Section]) = {
      val operand = parent(doc)

      if (operand.matches(selector)) {
        val selected = new VectorBuilder[Section]
        val chunks   = new VectorBuilder[Int]
        val editors  = new VectorBuilder[(Doc[Section], Map[Int, Set[Int]]) => Doc[Section]]
        val childMaps = new VectorBuilder[Map[Int, Set[Int]]]

        operand.foreach {
          case section @ Complex(_, children) => {
            var childMap = Map.empty[Int, Set[Int]]
            val selectedChildOffsets = new ArrayBuffer[Int](children.length)

            val selectedCount = children.zipWithIndex.foldLeft(0) {
              case (count, (child, i)) if selector(child) =>
                selected += child
                selectedChildOffsets += i
                childMap += (i -> Set(count))
                count + 1

              case (count, _) => count
            }

            chunks += selectedCount
            childMaps += childMap
            editors += (
              (replacements: Doc[Section], map: Map[Int, Set[Int]]) => Doc(
                section.copy(
                  doc = Doc(
                    children.zipWithIndex.foldLeft(Vector.empty[Section]) {
                      case (result, (_, i)) if map.contains(i) => map(i).foldLeft(result) { _ :+ replacements(_) }
                      case (result, (s, _))                    => result :+ s
                    }: _*
                  )
                )
              )
            )
          }

          case _ =>
        }

        lazy val (_, edits) = {
          (chunks.result zip editors.result zip childMaps.result).foldLeft((0, Vector[Edit]())) {
            // i:       index of an element in the rebuilt group
            // acc:     the vector of contexts that will be used to construct the new parent element on unselect
            // length:  the number of children retained in the resulting zipper from the indexed element
            // f:       the function that will rebuild the indexed element on unselect
            case ((i, acc), ((length, f), childMap)) if length > 0 => (i + length, acc :+ Edit(i, i + length, f, childMap))
            case ((i, acc), _)                                     => (i, acc)
          }
        }

        val builder = cbf.builder(operand.zipper, edits)
        builder ++= selected.result
        builder.result
      } else {
        cbf.builder(Vector.empty).result
      }
    }
  }

  def #%(s: Selector): Traverse = new Traverse {
    override def apply(doc: Doc[Section]) = error("todo")
  }

  def #>(s: Selector): Traverse = new Traverse {
    override def apply(doc: Doc[Section]) = error("todo")
  }
}

