package com.nommit.pickle

import org.specs._
import org.specs.matcher.Matcher

/**
 * Created by IntelliJ IDEA.
 * User: nuttycom
 * Date: 4/1/11
 * Time: 9:55 AM
 * To change this template use File | Settings | File Templates.
 */

class ParserSpec extends Specification {
  def matchPickleParse(expected: Doc) = beLike[PickleParser.ParseResult[Doc]] {
    case PickleParser.Success(`expected`, reader) => reader.atEnd
  }

  "the pickle parser" should {
    "parse a lone short form tag without metadata" in {
      PickleParser.parse("@a[arbitrary text]") must matchPickleParse(
        Doc.tagged(ShortForm("a"), Doc.text("arbitrary text"))
      )
    }

    "parse a lone short form tag with metadata" in {
      PickleParser.parse("@a[arbitrary text | @b[foo]]") must matchPickleParse(
        Doc.tagged(
          ShortForm("a", List(Tagged(ShortForm("b"), Doc.text("foo")))),
          Doc.text("arbitrary text")
        )
      )
    }

    "parse compound data with a short form tag" in {
      PickleParser.parse("Hi there @a[Joe | @href[http://www.joe.com]]") must matchPickleParse(
        Doc(
          List(
            Text("Hi there "),
            Tagged(
              ShortForm("a", List(Tagged(ShortForm("href"), Doc.text("http://www.joe.com")))),
              Doc.text("Joe")
            )
          )
        )
      )
    }

    "parse long form data with a single tag using a short-form identifier" in {
      PickleParser.parse("@[a]arbitrary text@/") must matchPickleParse(
        Doc.tagged(ShortForm("a"), Doc.text("arbitrary text"))
      )
    }

    "parse long form data with a single tag using a short-form identifier with metadata" in {
      PickleParser.parse("@[a | @b[foo]]arbitrary text@/") must matchPickleParse(
        Doc.tagged(
          ShortForm("a", List(Tagged(ShortForm("b"), Doc.text("foo")))),
          Doc.text("arbitrary text")
        )
      )
    }

    "parse long form data with a single tag using a long-form identifier" in {
      PickleParser.parse("@[@ref[foobar]]arbitrary text@/") must matchPickleParse(
        Doc.tagged(
          LongForm(Doc.tagged(ShortForm("ref"), Doc.text("foobar"))),
          Doc.text("arbitrary text")
        )
      )
    }

    "parse long form data using a long-form closing tag" in {
      PickleParser.parse("@[label]arbitrary text@[/label]") must matchPickleParse(
        Doc.tagged(ShortForm("label"), Doc.text("arbitrary text"))
      )
    }

    "fail to parse long form data using a mismatched long-form closing tag" in {
      PickleParser.parse("@[label]arbitrary text@[/labex]") must beLike {
        case PickleParser.Failure(msg, next) => msg.startsWith("Unable to match long form closing tag")
      }
    }

    "parse nested short-form tags" in {
      PickleParser.parse("@a[arbitrary text | @b[foo | @c[|@bar[ok]]]]") must  matchPickleParse (
        Doc.tagged(
          ShortForm(
            "a",
            List(
              Tagged(
                ShortForm(
                  "b",
                  List(Tagged(ShortForm("c", List(Tagged(ShortForm("bar"), Doc.text("ok")))), Doc.Empty))
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
        @[label | @ref[b]]
          some text
          @[@ref[a] | @b[foo]]
            arbitrary text
          @/
          more text
        @[/label]
      """

      PickleParser.parse(sample) must matchPickleParse(
        Doc.tagged(
          ShortForm(
            "label",
            List(Tagged(ShortForm("ref"), Doc.text("b")))
          ),
          Doc(
            List(
              Text("some text\n          "),
              Tagged(
                LongForm(
                  Doc.tagged(ShortForm("ref"), Doc.text("a")),
                  List(Tagged(ShortForm("b"), Doc.text("foo")))
                ),
                Doc.text("arbitrary text\n          ")
              ),
              Text("more text\n        ")
            )
          )
        )
      )
    }

    "parse a complex document" in {
      val sample = """
        @a[arbitrary text | @b[foo | @c[|@bar[ok]]]]

        @[label | @ref[b]]
          @[@ref[a] | @b[foo]]
            as;ldkjal;dsj
          @/
        @[/label]

        Some More Arbitrary Text

        @[label | @ref[a]]
          @ref[b]
        @/

        @a[Hi there     \ | @href[stuff]]
        @a[Hi there \| here's a pipe symbol \|\ | @href[google.com]]

        @[a | @href[google.com]]
          @a[foo | @href[google.com]]
          @label[|@ref[null]]
          @a[foo | @href[@ref[null]]]
        @/
      """

      val result = PickleParser.parse(sample)

      result must haveClass[PickleParser.Success[Doc]]
      result match {
        case PickleParser.Success(doc, _) =>
          doc.sections must haveSize(7)
          doc.sections.last must_== Tagged(
            ShortForm("a", List(Tagged(ShortForm("href"), Doc.text("google.com")))),
            Doc(
              List(
                Tagged(ShortForm("a", List(Tagged(ShortForm("href"), Doc.text("google.com")))), Doc.text("foo")),
                Tagged(ShortForm("label", List(Tagged(ShortForm("ref"), Doc.text("null")))), Doc.Empty),
                Tagged(
                  ShortForm("a", List(Tagged(ShortForm("href"), Doc.tagged(ShortForm("ref"), Doc.text("null"))))),
                  Doc.text("foo")
                )
              )
            )
          )
      }
    }
  }

}

// vim: set ts=4 sw=4 et:

