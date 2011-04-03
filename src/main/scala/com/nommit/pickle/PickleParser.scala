package com.nommit.pickle

import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.combinator.PackratParsers

case class Doc(sections: List[Section])

object Doc {
  val Empty = Doc(Nil)
  def text(s: String) = Doc(List(Text(s)))
  def tagged(t: Tag, d: Doc) = Doc(List(Tagged(t, d)))
}

sealed trait Tag {
  def metadata: List[Tagged]
}

case class ShortForm(ident: String, metadata: List[Tagged] = Nil) extends Tag
case class LongForm(doc: Doc, metadata: List[Tagged] = Nil) extends Tag

sealed trait Section
case class Tagged(tag: Tag, doc: Doc) extends Section
case class Text(text: String) extends Section

object PickleParser extends RegexParsers with PackratParsers {
  def parse(s: String) = parseAll(doc, s)

  // Just about any string that doesn't include spaces, control characters
  // or would confuse the parser can be used as an identifier. Bring on your Unicode!
  val Ident = """([^@\[\]\s\p{Cntrl}]+)""".r

  lazy val doc: PackratParser[Doc] = rep(tagged | text).map(Doc(_))

  def text: Parser[Text] = """([^@\\]|\\@)+""".r ^^ (s => Text(s))

  lazy val tagged: PackratParser[Tagged] = (shortForm | longForm)

  lazy val shortForm: PackratParser[Tagged] = "@" ~> Ident ~ tagbody ^^ {
    case i ~ b => Tagged(ShortForm(i, b.metadata), b.doc)
  }

  lazy val longForm: PackratParser[Tagged]  = ("@" ~> tagbody ~ doc ~ longFormClose) flatMap {
    // translate long form to short form if the data is in ident form
    case LongForm(Doc(List(Text(Ident(oid)))), metadata) ~ d ~ None => success(Tagged(ShortForm(oid, metadata), d))
    // short close always succeeds
    case b ~ d ~ None => success(Tagged(b, d))
    // long close succeeds if the opening tag body is in ident form and opening id matches closing id
    case LongForm(Doc(List(Text(oid))), metadata) ~ d ~ Some(cid) if oid.trim == cid.trim => success(Tagged(ShortForm(oid, metadata), d))
    // long close cannot be used for doc form
    case b ~ d ~ c => failure("Unable to match long form closing tag for " + b + " at " + c)
  }

  lazy val longFormClose: PackratParser[Option[String]] = ("@/" ^^ (_ => None) | ("@[/" ~> Ident <~ "]").map(Some(_)))

  lazy val tagbody: PackratParser[LongForm] = "[" ~> ndoc ~ opt("|" ~> rep1sep(shortForm, "\\w+".r)) <~ "]" ^^ {
    case d ~ m => LongForm(d, m.toList.flatten)
  }

  def ntext: Parser[Text] = """([^|@\\\]\[]|\\@|\\\||\\ )+""".r ^^ (s => Text(s))

  lazy val ndoc: PackratParser[Doc] = rep(tagged | ntext).map(
    l => Doc(
      l.foldRight(List.empty[Section]) {
        case (Text(s), Nil) =>
          // strip trailing unescaped spaces from the last element
          val ss = s.replaceAll("""(?<!\\)\s+$""", "")
          // if what remains is empty, drop the element
          if (ss.isEmpty) Nil else Text(ss) :: Nil

        case (elem, rest) => elem :: rest
      }
    )
  )
}

