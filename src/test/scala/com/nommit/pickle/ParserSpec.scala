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

  "a lone short form tag without metadata" should {
    "parse correctly" in {
      PickleParser.parse("@a[asdljadlf;j;ljk]") must_== Data.tagged(ShortForm("a", Nil), Data.str("asdljadlf;j;ljk"))
    }
  }

  "a lone short form tag with metadata" should {
    "parse correctly" in {
      PickleParser.parse("@a[asdljadlf;j;ljk | @b[foo]]") must_== Data.tagged(
        ShortForm("a", List(Tagged(ShortForm("b", Nil), Data.str("foo")))), Data.str("asdljadlf;j;ljk")
      )
    }
  }

  "simple compound data with a short form tag" should {
    "parse correctly" in {
      PickleParser.parse("Hi there @a[Joe | @href[http://www.joe.com]]") must_== Data(
        List(
          Left("Hi there "),
          Right(
            Tagged(
              ShortForm("a", List(Tagged(ShortForm("href", Nil), Data.str("http://www.joe.com")))),
              Data.str("Joe ")
            )
          )
        )
      )
    }
  }
}

// vim: set ts=4 sw=4 et:

