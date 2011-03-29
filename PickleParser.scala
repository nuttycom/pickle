import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.combinator.PackratParsers

case class Data(data: List[Either[String, Tagged]])

sealed trait Tag
case class ShortForm(ident: String, metadata: List[Tagged]) extends Tag
case class LongForm(data: Data, metadata: List[Tagged]) extends Tag

case class Tagged(tag: Tag, data: Data)

object PickleParser extends RegexParsers with PackratParsers {
  def parse(s: String) = parseAll(doc, s)

  lazy val doc: PackratParser[Data] = rep(tagged.map(Right(_)) | text.map(Left(_))).map(Data(_))

  def text: Parser[String] = """([^@\\]|\\@)+""".r

  def ident: Parser[String] = """[^@\[\]\s\p{Cntrl}]*""".r

  lazy val tagged: PackratParser[Tagged] = (shortForm | longForm)

  lazy val shortForm: PackratParser[Tagged] = "@" ~> ident ~ tagbody ^^ {
    case i ~ b => Tagged(ShortForm(i, b.metadata), b.data)
  }

  lazy val longForm: PackratParser[Tagged]  = "@" ~> tagbody ~ doc <~ "@[]" ^^ {
    case b ~ d => Tagged(b, d)
  }

  lazy val tagbody: PackratParser[LongForm] = "[" ~> ndoc ~ opt("|" ~> rep1sep(shortForm, "\\w+".r)) <~ "]" ^^ {
    case d ~ m => LongForm(d, m.toList.flatten)
  }

  lazy val ndoc: PackratParser[Data] = rep(tagged.map(Right(_)) | ntext.map(Left(_))).map(Data(_))

  def ntext: Parser[String] = """([^|@\\\]\[]|\\@|\\\||\\ )+""".r

}

object ParserTest {
  val sample = """
  @a[asdljadlf;j;ljk | @b[foo]]

  @[label | @ref[b]]
  @[@ref[a] | @b[foo]]

  as;ldkjal;dsj

  @[]
  @[]

  @[label | @ref[a]]

  @ref[b]

  @[]

  @a[Hi there     \ | @href[stuff]]
  @a[Hi there \| here's a pipe symbol \|\ | @href[google.com]]

  @[a | @href[]]

  @a[foo | @href[]]

  @label[|@ref[null]]
  @a[foo | @href[@ref[null]]]
  """

  val simple = "Hi there @a[Joe | @href[http://www.joe.com]]"

  def main(argv: Array[String]) {
    println(PickleParser.parse("@a[asdljadlf;j;ljk]"))
    println(PickleParser.parse("@a[asdljadlf;j;ljk | @b[foo]]"))
    println(PickleParser.parse(simple)) 
    println(PickleParser.parse(sample)) 
  }
}

// vim: set ts=4 sw=4 et:
