package com.nommit.pickle

import org.specs._

class ParserSpec extends Specification {
  def matchPickleParse[S <: Section](expected: Doc[S]) = beLike[PickleParser.ParseResult[Doc[S]]] {
    case PickleParser.Success(`expected`, reader) => reader.atEnd
  }

  "the pickle parser" should {
    "parse a lone short form tag without metadata" in {
      PickleParser.parse("@a[arbitrary text]") must matchPickleParse(
        Doc.tagged(Tag("a"), Doc.text("arbitrary text"))
      )
    }

    "parse a lone short form tag with metadata" in {
      PickleParser.parse("@a[arbitrary text # @b[foo]]") must matchPickleParse(
        Doc.tagged(
          Tag("a", Metadata(Complex(Tag("b"), Doc.text("foo")))),
          Doc.text("arbitrary text")
        )
      )
    }

    "parse compound data with a short form tag" in {
      PickleParser.parse("Hi there @a[Joe # @href[http://www.joe.com]]") must matchPickleParse(
        Doc(
          Primitive("Hi there "),
          Complex(
            Tag("a", Metadata(Complex(Tag("href"), Doc.text("http://www.joe.com")))),
            Doc.text("Joe")
          )
        )
      )
    }

    "parse long form data with a single tag using a short-form identifier" in {
      PickleParser.parse("@[a]arbitrary text@/") must matchPickleParse(
        Doc.tagged(Tag("a"), Doc.text("arbitrary text"))
      )
    }

    "parse long form data with a single tag using a short-form identifier with metadata" in {
      PickleParser.parse("@[a # @b[foo]]arbitrary text@/") must matchPickleParse(
        Doc.tagged(
          Tag("a", Metadata(Complex(Tag("b"), Doc.text("foo")))),
          Doc.text("arbitrary text")
        )
      )
    }

    "parse long form data with a single tag using a long-form identifier" in {
      PickleParser.parse("@[@ref[foobar]]arbitrary text@/") must matchPickleParse(
        Doc.tagged(
          MetaTag(Doc.tagged(Tag("ref"), Doc.text("foobar"))),
          Doc.text("arbitrary text")
        )
      )
    }

    "parse long form data using a long-form closing tag" in {
      PickleParser.parse("@[label]arbitrary text@[/label]") must matchPickleParse(
        Doc.tagged(Tag("label"), Doc.text("arbitrary text"))
      )
    }

    "fail to parse long form data using a mismatched long-form closing tag" in {
      PickleParser.parse("@[label]arbitrary text@[/labex]") must beLike {
        case PickleParser.Failure(msg, next) => msg.startsWith("Unable to match metatag closing tag")
      }
    }

    "parse nested short-form tags" in {
      PickleParser.parse("@a[arbitrary text # @b[foo # @c[#@bar[ok]]]]") must  matchPickleParse (
        Doc.tagged(
          Tag(
            "a",
            Metadata(
              Complex(
                Tag(
                  "b",
                  Metadata(Complex(Tag("c", Metadata(Complex(Tag("bar"), Doc.text("ok")))), Doc.empty[Section]))
                ),
                Doc.text("foo")
              )
            )
          ),
          Doc.text("arbitrary text")
        )
      )
    }

    "parse nested long-form tags" in {
      val sample = """
        @[label # @ref[b]]
          some text
          @[@ref[a] # @b[foo]]
            arbitrary text
          @/
          more text
        @[/label]
      """

      PickleParser.parse(sample) must matchPickleParse(
        Doc.tagged(
          Tag(
            "label",
            Metadata(Complex(Tag("ref"), Doc.text("b")))
          ),
          Doc(
            Primitive("some text\n          "),
            Complex(
              MetaTag(
                Doc.tagged(Tag("ref"), Doc.text("a")),
                Metadata(Complex(Tag("b"), Doc.text("foo")))
              ),
              Doc.text("arbitrary text")
            ),
            Primitive("more text")
          )
        )
      )
    }

    "parse a complex document" in {
      val sample = """
        @a[arbitrary text # @b[foo # @c[#@bar[ok]]]]

        @[label # @ref[b]]
          @[@ref[a] # @b[foo]]
            as;ldkjal;dsj
          @/
        @[/label]

        Some More Arbitrary Text

        @[label # @ref[a]]
          @ref[b]
        @/

        @a[Hi there     \ # @href[stuff]]
        @a[Hi there \| here's a pipe symbol \|\ # @href[google.com]]

        @[a # @href[google.com]]
          @a[foo # @href[google.com]]
          @label[#@ref[x]]
          @a[foo # @href[@ref[x]]]
        @/
      """

      val result = PickleParser.parse(sample)

      result must haveClass[PickleParser.Success[Doc[Section]]]
      result match {
        case PickleParser.Success(doc, _) =>
          doc.sections must haveSize(7)
          doc.sections.last must_== Complex(
            Tag("a", Metadata(Complex(Tag("href"), Doc.text("google.com")))),
            Doc(
              Complex(Tag("a", Metadata(Complex(Tag("href"), Doc.text("google.com")))), Doc.text("foo")),
              Complex(Tag("label", Metadata(Complex(Tag("ref"), Doc.text("x")))), Doc.empty[Section]),
              Complex(
                Tag("a", Metadata(Complex(Tag("href"), Doc.tagged(Tag("ref"), Doc.text("x"))))),
                Doc.text("foo")
              )
            )
          )
      }
    }

    "parse a document of just primitives" in {
      val sample = "123 45th St. @@ 678 90th St."
      PickleParser.parse(sample) must matchPickleParse(
        Doc(
          Primitive("123 45th St."),
          Separator,
          Primitive("678 90th St.")
        )
      )
    }

    "parse a document of primitives with some escaping" in {
      val sample = "@list[123 45th St. \\@ \\| | 678 90th St.]"
      PickleParser.parse(sample) must matchPickleParse(
        Doc(
          Complex(
            Tag("list"),
            Doc(Primitive("123 45th St. @ |"), Separator, Primitive("678 90th St."))
          )
        )
      )
    }

    "parse a mixed document" in {
      val sample = "123 45th St. @@ 678 90th St. @a[Link # @href[google.com]]"
      PickleParser.parse(sample) must matchPickleParse(
        Doc(
          Primitive("123 45th St."),
          Separator,
          Primitive("678 90th St. "),
          Complex(
            Tag("a", Metadata(Complex(Tag("href"), Doc.text("google.com")))),
            Doc.text("Link")
          )
        )
      )
    }
  }
}


// vim: set ts=4 sw=4 et:

