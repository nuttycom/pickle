package com.nommit.pickle

import org.specs._
import scalaz._

class ValidatorSpec extends Specification {
  import Validators.Strict._

  "a document validator" should {
    def doc(markup: String) = PickleParser.parse(markup) match {
      case PickleParser.Success(v, _) => v
    }

    def ahref(href: => DocValidator[NonEmptyList[String]], body: => DocValidator[NonEmptyList[String]]) = ident('a) {
      metadata { ident('href) { noMetadata ~ href } } ~ body
    }

    val validator = ahref(
      text("google.com"),
      ahref(text("google.com"), text("foo")) ~
      ident('label) { metadata { ident('ref) { noMetadata ~ text("null") } } ~ Validators.Strict.empty } ~
      ahref(ident('ref) { noMetadata ~ text("null") }, text("foo"))
    )

    "verify the structure of a document" in {
      val d = doc(
        """
          @[a # @href[google.com]]
            @a[foo # @href[google.com]]
            @label[#@ref[null]]
            @a[foo # @href[@ref[null]]]
          @/
        """
      )

      println(validator.validate(d))
      validator.validate(d) must_== Success(
        Doc(
          Complex(
            Tag("a",Metadata(Complex(Tag("href",Metadata()),Doc(Primitive("google.com"))))),
            Doc(
              Complex(
                Tag("a",Metadata(Complex(Tag("href",Metadata()),Doc(Primitive("google.com"))))),
                Doc(Primitive("foo"))
              ),
              Complex(Tag("label",Metadata(Complex(Tag("ref",Metadata()),Doc(Primitive("null"))))),Doc()),
              Complex(
                Tag("a",Metadata(Complex(Tag("href",Metadata()),Doc(Complex(Tag("ref",Metadata()),Doc(Primitive("null"))))))),
                Doc(Primitive("foo"))
              )
            )
          )
        )
      )
    }

    "reject a document that does not comply with the validator's expected structure" in {
      val d = doc(
        """
          @[a # @href[foogle.com]]
            @a[foo # @href[google.com]]
            @a[foo # @href[@ref[null]]]
          @/
        """
      )

      val result = validator.validate(d)
      //println(result)
      result.isSuccess must beFalse
    }

    "fail to compile in the presence of validation structure errors " in {
      //val fail1 = ahref(
      //  primitive(_ == "google.com"),
      //  ahref(text, text) ~
      //  ident('label) {
      //    Validators.Strict.empty ~
      //    metadata {
      //      ident('ref) { noMetadata ~ text }
      //    }
      //  } ~
      //  ahref(text, ident('ref) { noMetadata ~ text })
      //)

      //val fail2 = ahref(
      //  primitive(_ == "google.com"),
      //  ahref(text, text) ~
      //    ident('label) {
      //      metadata {
      //        ident('ref) { noMetadata ~ text }
      //      } ~
      //      Validators.Strict.empty ~
      //      metadata {
      //        ident('ref) { noMetadata ~ text }
      //      } ~
      //    } ~
      //    ahref(text, ident('ref) { noMetadata ~ text })
      //)
    }
  }
}