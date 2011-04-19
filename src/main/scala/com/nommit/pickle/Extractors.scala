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


trait Extractors[+E] {
  def error[X](msg: String): Validation[E, X]

  trait DocExtractor[S <: Section, +A] extends Function1[Doc[S], Validation[E, A]] { outer =>
    def ~[T >: S <: Section, B](s: DocExtractor[T, B]): DocExtractor[T, A ~ B] = new DocExtractor[T, A ~ B] {
      override def apply(s1: Doc[S]) = (outer(s1) <**> s()
    }

    def ~[T >: S <: Section, B](s: SectionExtractor[T, B]): DocExtractor[T, A ~ B] = error[A ~ B]("todo")

    def map[B](f: A => B): DocExtractor[S, B] =
    def flatMap[T >: S <: Section, B](f: A => DocExtractor[T, B]): DocExtractor[T, B] = error("todo")
  }

  trait SectionExtractor[S <: Section, +A] extends Function1[S, Validation[E, A]] {
    def ~[T >: S <: Section, B](e: DocExtractor[T, B]): DocExtractor[T, A ~ B] = error("todo")
    def ~[T >: S <: Section, B](s: SectionExtractor[T, B]): DocExtractor[T, A ~ B] = error("todo")

    def map[B](f: A => B): SectionExtractor[S, B] = error("todo")

    def flatMap[T >: S <: Section, B](f: A => SectionExtractor[T, B]): SectionExtractor[T, B] = error("todo")
  }

  sealed trait SemanticsHandler[-T, M, +A] extends Function2[T, M, Validation[E, A]]{
    def metadataExtractor: DocExtractor[Complex[Section], M] = error("todo")
  }

  trait TagHandler[M, +A] extends SemanticsHandler[String, M, A] { self =>
    def extractor[B](d: DocExtractor[Section, B]) = new SectionExtractor[Complex[Section], A ~ B] {
      override def apply(section: Complex[Section]) = section.tag match {
        case Tag(ident, metadata) =>
          (self.metadataExtractor(metadata).flatMap(self(ident, _)) <**> d(section.doc))(_ ~ _)

        case metatag => 
          error("Expected a tag, but got " + metatag)
      }
    }
  }

  trait MetaTagHandler[T, M, +A] extends SemanticsHandler[T, M, A] {
    def metaTagExtractor: DocExtractor[Section, T]
  }
}