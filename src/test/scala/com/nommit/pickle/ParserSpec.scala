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
        Data.tagged(ShortForm("a", Nil), Data.str("arbitrary text"))
      )
    }

    "parse a lone short form tag with metadata" in {
      PickleParser.parse("@a[arbitrary text | @b[foo]]") must matchPickleParse(
        Data.tagged(
          ShortForm("a", List(Tagged(ShortForm("b", Nil), Data.str("foo")))),
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
                ShortForm("a", List(Tagged(ShortForm("href", Nil), Data.str("http://www.joe.com")))),
                Data.str("Joe")
              )
            )
          )
        )
      )
    }

    "parse long form data with a single tag using a short-form identifier" in {
      PickleParser.parse("@[a]arbitrary text@/") must matchPickleParse(
        Data.tagged(ShortForm("a", Nil), Data.str("arbitrary text"))
      )
    }

    "parse long form data with a single tag using a short-form identifier with metadata" in {
      PickleParser.parse("@[a | @b[foo]]arbitrary text@/") must matchPickleParse(
        Data.tagged(
          ShortForm("a", List(Tagged(ShortForm("b", Nil), Data.str("foo")))),
          Data.str("arbitrary text")
        )
      )
    }

    "parse long form data with a single tag using a long-form identifier" in {
      PickleParser.parse("@[@ref[foobar]]arbitrary text@/") must matchPickleParse(
        Data.tagged(
          LongForm(Data.tagged(ShortForm("ref", Nil), Data.str("foobar")), Nil),
          Data.str("arbitrary text")
        )
      )
    }

    "parse long form data using a long-form closing tag" in {
      PickleParser.parse("@[label]arbitrary text@[/label]") must matchPickleParse(
        Data.tagged(ShortForm("label", Nil), Data.str("arbitrary text"))
      )
    }

    "fail to parse long form data using a mismatched long-form closing tag" in {
      PickleParser.parse("@[label]arbitrary text@[/labex]") must beLike {
        case PickleParser.Failure(msg, next) => msg.startsWith("Unable to match long form closing tag")
      }
    }
  }

  val sample = """
    @a[arbitrary text | @b[foo]]

    @[label | @ref[b]]
      @[@ref[a] | @b[foo]]
        as;ldkjal;dsj
      @/
    @[/label]

    @[label | @ref[a]]
      @ref[b]
    @/

    @a[Hi there     \ | @href[stuff]]
    @a[Hi there \| here's a pipe symbol \|\ | @href[google.com]]

    @[a | @href[]]

    @a[foo | @href[]]

    @label[|@ref[null]]
    @a[foo | @href[@ref[null]]]
  """
}

// vim: set ts=4 sw=4 et:

