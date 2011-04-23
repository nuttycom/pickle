package com.nommit.pickle
package extractors

import scalaz._
import Scalaz._

/** A simple case class to make pattern matching nicer looking than it would with raw tuples */
case class ~[+A, +B](a: A, b: B) {
  def tuple = (a, b)
}

object ~ {
  implicit def tuple[A, B](t: A ~ B): (A, B) = t.tuple
}

trait DocParsers[E] {
  sealed trait Result[+A] {
    def map[B](f: A => B): Result[B] = this match {
      case Success(a, rest) => Success(f(a), rest)
      case f : Failure => f
    }
  }

  case class Success[+A](value: A, rest: Doc[Section]) extends Result[A]
  case class Failure(error: E, rest: Doc[Section]) extends Result[Nothing]


  def failure(msg: String, rest: Doc[Section]): Failure

  trait DocParser[+A] extends Function1[Doc[Section], Result[A]] { outer =>
    def ~[B](next: DocParser[B]): DocParser[A ~ B] = new DocParser[A ~ B] {
      override def apply(doc: Doc[Section]): Result[A ~ B] = outer(doc) match {
        case Success(a, rest) => next(rest).map(b => new ~(a, b))
        case f : Failure => f
      }
    }

    def ~[B](next: SectionParser[B]): DocParser[A ~ B] = new DocParser[A ~ B] {
      override def apply(doc: Doc[Section]) = outer(doc) match {
        case Success(a, rest) => rest.headOption.map(s => next(s)).getOrElse(failure("Document empty", Doc.empty)) match {
          case Success(b, _) => Success(new ~(a, b), rest.tail)
          case f : Failure => f
        }

        case f : Failure => f
      }
    }

    def map[B](f: A => B): DocParser[B] = new DocParser[B] {
      override def apply(doc: Doc[Section]) = outer(doc).map(f)
    }

    def flatMap[B](f: A => DocParser[B]): DocParser[B] = new DocParser[B] {
      override def apply(doc: Doc[Section]) = outer(doc) match {
        case Success(a, rest) => f(a)(rest)
        case f : Failure => f
      }
    }
  }

  trait SectionParser[+A] extends Function1[Section, Result[A]] { outer =>
    def ~[B](next: DocParser[B]): DocParser[A ~ B] = new DocParser[A ~ B] {
      override def apply(doc: Doc[Section]): Result[A ~ B] = doc.headOption.map(s => outer(s)).getOrElse(failure("Document empty", Doc.empty)) match {
        case Success(a, _) => next(doc.tail).map(b => new ~(a, b))
        case f : Failure => f
      }
    }

    def ~[B](next: SectionParser[B]): DocParser[A ~ B] = new DocParser[A ~ B] {
      override def apply(doc: Doc[Section]) = doc.headOption.map(s => outer(s)).getOrElse(failure("Document empty", Doc.empty)) match {
        case Success(a, _) => doc.tail.headOption.map(next).getOrElse(failure("Document empty", Doc.empty)) match {
          case Success(b, _) => Success(new ~(a, b), doc.tail.tail)
          case f : Failure => f
        }

        case f : Failure => f
      }
    }

    def map[B](f: A => B): SectionParser[B] = new SectionParser[B] {
      override def apply(s: Section) = outer(s).map(f)
    }
  }

  sealed trait SemanticsHandler[-T, M, +A] extends Function2[T, M, Validation[E, A]]{
    def metadataExtractor: DocParser[M]
  }

  trait TagHandler[M, +A] extends SemanticsHandler[String, M, A] { outer =>
    def extractor[B](d: DocParser[B]) = new SectionParser[A ~ B] {
      override def apply(section: Section) = section match {
        case Complex(Tag(ident, metadata), doc) => failure("todo", Doc.empty)
        case Complex(MetaTag(_, _), doc) => failure("Found a metatag instead of a tag", doc)
        case _ => failure("Not a tag", Doc.empty)
      }
    }
  }

  trait MetaTagHandler[T, M, +A] extends SemanticsHandler[T, M, A] {
    def metaTagExtractor: DocParser[T]
  }
}