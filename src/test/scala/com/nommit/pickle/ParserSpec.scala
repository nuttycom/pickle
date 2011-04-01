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

  def matchPickleParse(expected: Data) = beLike[PickleParser.ParseResult[Data]] {
    case PickleParser.Success(`expected`, reader) => reader.atEnd
  }

  "the pickle parser" should {
    "parse a lone short form tag without metadata" in {
      PickleParser.parse("@a[arbitrary text]") must matchPickleParse(
        Data.tagged(ShortForm("a"), Data.str("arbitrary text"))
      )
    }

    "parse a lone short form tag with metadata" in {
      PickleParser.parse("@a[arbitrary text | @b[foo]]") must matchPickleParse(
        Data.tagged(
          ShortForm("a", List(Tagged(ShortForm("b"), Data.str("foo")))),
          Data.str("arbitrary text")
        )
      )
    }

    "parse compound data with a short form tag" in {
      PickleParser.parse("Hi there @a[Joe | @href[http://www.joe.com]]") must matchPickleParse(
        Data(
          List(
            Left("Hi there "),
            Right(
              Tagged(
                ShortForm("a", List(Tagged(ShortForm("href"), Data.str("http://www.joe.com")))),
                Data.str("Joe")
              )
            )
          )
        )
      )
    }

    "parse long form data with a single tag using a short-form identifier" in {
      PickleParser.parse("@[a]arbitrary text@/") must matchPickleParse(
        Data.tagged(ShortForm("a"), Data.str("arbitrary text"))
      )
    }

    "parse long form data with a single tag using a short-form identifier with metadata" in {
      PickleParser.parse("@[a | @b[foo]]arbitrary text@/") must matchPickleParse(
        Data.tagged(
          ShortForm("a", List(Tagged(ShortForm("b"), Data.str("foo")))),
          Data.str("arbitrary text")
        )
      )
    }

    "parse long form data with a single tag using a long-form identifier" in {
      PickleParser.parse("@[@ref[foobar]]arbitrary text@/") must matchPickleParse(
        Data.tagged(
          LongForm(Data.tagged(ShortForm("ref"), Data.str("foobar"))),
          Data.str("arbitrary text")
        )
      )
    }

    "parse long form data using a long-form closing tag" in {
      PickleParser.parse("@[label]arbitrary text@[/label]") must matchPickleParse(
        Data.tagged(ShortForm("label"), Data.str("arbitrary text"))
      )
    }

    "fail to parse long form data using a mismatched long-form closing tag" in {
      PickleParser.parse("@[label]arbitrary text@[/labex]") must beLike {
        case PickleParser.Failure(msg, next) => msg.startsWith("Unable to match long form closing tag")
      }
    }

    "parse nested short-form tags" in {
      PickleParser.parse("@a[arbitrary text | @b[foo | @c[|@bar[ok]]]]") must  matchPickleParse (
        Data.tagged(
          ShortForm(
            "a",
            List(
              Tagged(
                ShortForm(
                  "b",
                  List(Tagged(ShortForm("c", List(Tagged(ShortForm("bar"), Data.str("ok")))), Data.Empty))
                ),
                Data.str("foo")
              )
            )
          ),
          Data.str("arbitrary text")
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
        Data.tagged(
          ShortForm(
            "label",
            List(Tagged(ShortForm("ref"), Data.str("b")))
          ),
          Data(
            List(
              Left("some text\n          "),
              Right(
                Tagged(
                  LongForm(
                    Data.tagged(ShortForm("ref"), Data.str("a")),
                    List(Tagged(ShortForm("b"), Data.str("foo")))
                  ),
                  Data.str("arbitrary text\n          ")
                )
              ),
              Left("more text\n        ")
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

      result must haveClass[PickleParser.Success[Data]]
      result match {
        case PickleParser.Success(data, _) =>
          data.data must haveSize(7)
          data.data.last.right.get must_== Tagged(
            ShortForm("a", List(Tagged(ShortForm("href"), Data.str("google.com")))),
            Data(
              List(
                Right(Tagged(ShortForm("a", List(Tagged(ShortForm("href"), Data.str("google.com")))), Data.str("foo"))),
                Right(Tagged(ShortForm("label", List(Tagged(ShortForm("ref"), Data.str("null")))), Data.Empty)),
                Right(
                  Tagged(
                    ShortForm("a", List(Tagged(ShortForm("href"), Data.tagged(ShortForm("ref"), Data.str("null"))))),
                    Data.str("foo")
                  )
                )
              )
            )
          )
      }
    }
  }

}

// vim: set ts=4 sw=4 et:

