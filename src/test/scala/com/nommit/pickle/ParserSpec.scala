package com.nommit.pickle

import org.specs._

/**
 * Created by IntelliJ IDEA.
 * User: nuttycom
 * Date: 4/1/11
 * Time: 9:55 AM
 * To change this template use File | Settings | File Templates.
 */

class ParserSpec extends Specification {

  "the pickle parser" should {
    "parse a lone short form tag without metadata" in {
      val expected = Data.tagged(ShortForm("a", Nil), Data.str("asdljadlf;j;ljk"))
      PickleParser.parse("@a[asdljadlf;j;ljk]") must beLike {
        case PickleParser.Success(`expected`, reader) => reader.atEnd
      }
    }

    "parse a lone short form tag with metadata" in {
      val expected =  Data.tagged(
        ShortForm("a", List(Tagged(ShortForm("b", Nil), Data.str("foo")))), Data.str("asdljadlf;j;ljk")
      )

      PickleParser.parse("@a[asdljadlf;j;ljk | @b[foo]]") must beLike {
        case PickleParser.Success(`expected`, reader) => reader.atEnd
      }
    }

    "parse compound data with a short form tag" in {
      val expected = Data(
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

      PickleParser.parse("Hi there @a[Joe | @href[http://www.joe.com]]") must beLike {
        case PickleParser.Success(`expected`, reader) => reader.atEnd
      }
    }

    "parse long form data with a single tag using a short-form identifier" in {
      val expected = Data.tagged(ShortForm("a", Nil), Data.str("asdljadlf;j;ljk"))
      PickleParser.parse("@[a]asdljadlf;j;ljk@/") must beLike {
        case PickleParser.Success(`expected`, reader) => reader.atEnd
      }
    }

    "parse long form data with a single tag using a short-form identifier with metadata" in {
      val expected =  Data.tagged(
        ShortForm("a", List(Tagged(ShortForm("b", Nil), Data.str("foo")))), Data.str("asdljadlf;j;ljk")
      )

      PickleParser.parse("@[a | @b[foo]]asdljadlf;j;ljk@/") must beLike {
        case PickleParser.Success(`expected`, reader) => reader.atEnd
      }
    }
  }

  val sample = """
    @a[asdljadlf;j;ljk | @b[foo]]

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

