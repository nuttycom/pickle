package com.nommit.pickle

import org.specs._

class DocSpec extends Specification {

  "a primitive document" should {
    "not be convertible to metadata" in {
      // this won't compile
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
  }

  "a metadata document" should {
    "be appendable as metadata" in {
      val metadata: Metadata = Metadata.Empty :+ Complex(Tag("a"), Doc.empty[Primitive])
      metadata must_== Metadata(Complex(Tag("a"), Doc.empty[Primitive]))
    }
  }
}