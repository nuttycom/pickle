package com.nommit.pickle

import org.specs._

class DocSpec extends Specification {

  "a primitive document" should {
    "not be convertible to metadata" in {
      // this won't compile
      //Doc(Primitive("a")).toMetadata must beEmpty
    }
  }

  "a document composed nested complex sections" should {
    val doc: Doc[Complex[Complex[Primitive]]] = Doc(Complex(Tag("a"), Doc.empty[Complex[Primitive]]))

    "be convertible to metadata" in {
      doc.toMetadata must_== Metadata(Complex(Tag("a"), Doc.empty[Complex[Primitive]]))
    }
  }
}