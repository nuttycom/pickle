package com.nommit.pickle
package extractors

case class ~[+A, +B](a: A, b: B)

trait DocExtractor[S <: Section, +A] extends Function1[Doc[S], A] {
  def ~[T >: S <: Section, B](s: DocExtractor[T, B]): DocExtractor[T, A ~ B]
  def ~[T >: S <: Section, B](s: SectionExtractor[T, B]): DocExtractor[T, A ~ B]

  def map[B](f: A => B): DocExtractor[S, B]
  def flatMap[T >: S <: Section, B](f: A => DocExtractor[T, B]): DocExtractor[T, B]
}

trait SectionExtractor[S <: Section, +A] extends Function1[S, A] {
  def ~[T >: S <: Section, B](e: DocExtractor[T, B]): DocExtractor[T, A ~ B]
  def ~[T >: S <: Section, B](s: SectionExtractor[T, B]): DocExtractor[T, A ~ B]

  def map[B](f: A => B): SectionExtractor[S, B]

  def flatMap[T >: S <: Section, B](f: A => SectionExtractor[T, B]): SectionExtractor[T, B]
}

sealed trait SemanticsHandler[-T, M, +A] extends Function2[T, M, A]{
  def metadataExtractor: DocExtractor[Complex[Section], M]
}

trait TagHandler[M, +A] extends SemanticsHandler[String, M, A]

trait MetaTagHandler[T, M, +A] extends SemanticsHandler[T, M, A] {
  def metaTagExtractor: DocExtractor[Section, T]
}

trait Extractors {


}