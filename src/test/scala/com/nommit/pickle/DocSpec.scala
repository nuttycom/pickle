package com.nommit.pickle

import org.specs._

class DocSpec extends Specification {

  "a primitive document" should {
    "not be convertible to metadata" in {
      // this shouldn't compile
      //Doc(Primitive("a")).toMetadata must beEmpty
    }
  }

  "a complex document" should {
    "be convertible to metadata" in {
      "when the doc has just one level of complexity" >> {
        val doc: Doc[Complex[Primitive]] = Doc(Complex(Tag("a"), Doc.empty[Primitive]))
        doc.toMetadata must_== Metadata(Complex(Tag("a"), Doc.empty[Primitive]))
      }

      "when the doc has nested complex structure" >> {
        val doc: Doc[Complex[Complex[Primitive]]] = Doc(Complex(Tag("a"), Doc.empty[Complex[Primitive]]))
        doc.toMetadata must_== Metadata(Complex(Tag("a"), Doc.empty[Complex[Primitive]]))
      }
    }

    //"pretty print correctly" in {
    //  val pancakes = PickleParser.unsafeParse(SampleDocs.pancakes)
    //
    //}
  }

  "a metadata document" should {
    "be appendable as metadata" in {
      val metadata: Metadata = Metadata.Empty :+ Complex(Tag("a"), Doc.empty[Primitive])
      metadata must_== Metadata(Complex(Tag("a"), Doc.empty[Primitive]))
    }
  }

  "a multipart document" should {
    "split into its component pieces correctly" in {
      val result = Doc(Primitive("a"), Primitive("b"), Separator, Complex(Tag("x"), Doc.empty[Primitive]), Primitive("c"), Separator, Primitive("d")).split(true)
      result must_== Vector(
        Doc(Primitive("a"), Primitive("b")),
        Doc(Complex(Tag("x"), Doc.empty[Primitive]), Primitive("c")) ,
        Doc(Primitive("d"))
      )
    }

    "split correctly with leading separators" in {
      val result = Doc(Separator, Complex(Tag("x"), Doc.empty[Primitive]), Primitive("c"), Separator, Primitive("d")).split(true)
      result must_== Vector(
        Doc(Complex(Tag("x"), Doc.empty[Primitive]), Primitive("c")) ,
        Doc(Primitive("d"))
      )
    }

    "split correctly with trailing separators" in {
      val result = Doc(Primitive("a"), Primitive("b"), Separator, Complex(Tag("x"), Doc.empty[Primitive]), Primitive("c"), Separator).split(true)
      result must_== Vector(
        Doc(Primitive("a"), Primitive("b")),
        Doc(Complex(Tag("x"), Doc.empty[Primitive]), Primitive("c"))
      )
    }
  }

  "selecting parts of a document" should {
    val pancakes = PickleParser.unsafeParse(SampleDocs.pancakes)

    "filter" in {
      val doc = PickleParser.unsafeParse("@a[@b[c] @b[c] @b[c] d] d")
      val as = doc.select('a)
      val b = Complex(Tag("b"), Doc(Primitive("c")))
      as must_== Doc(Complex(Tag("a"), Doc(b, b, b, Primitive("d"))))
    }

    "descend" in {
      import Traverse._
      val doc = PickleParser.unsafeParse("@a[@b[c] @b[c] @b[c] d] d")
      val as = doc.select('a > 'b)
      val b = Complex(Tag("b"), Doc(Primitive("c")))
      as must_== Doc(b, b, b)
    }


      //as.unselect must_== Doc(Complex(Tag("a"), Doc(b, b, b, Primitive("d"))), Primitive("d"))
  }
}