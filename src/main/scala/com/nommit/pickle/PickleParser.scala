package com.nommit.pickle

import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.combinator.PackratParsers

case class Data(data: List[Either[String, Tagged]])

object Data {
  val empty = Data(Nil)
  def str(s: String) = Data(List(Left(s)))
  def tagged(t: Tag, d: Data) = Data(List(Right(Tagged(t, d))))
}

sealed trait Tag
case class ShortForm(ident: String, metadata: List[Tagged]) extends Tag
case class LongForm(data: Data, metadata: List[Tagged]) extends Tag

case class Tagged(tag: Tag, data: Data)

object PickleParser extends RegexParsers with PackratParsers {
  def parse(s: String) = parseAll(doc, s)

  lazy val doc: PackratParser[Data] = rep(tagged.map(Right(_)) | text.map(Left(_))).map(Data(_))

  def text: Parser[String] = """([^@\\]|\\@)+""".r

  def ident: Parser[String] = """[^@\[\]\s\p{Cntrl}]+""".r

  lazy val tagged: PackratParser[Tagged] = (shortForm | longForm)

  lazy val shortForm: PackratParser[Tagged] = "@" ~> ident ~ tagbody ^^ {
    case i ~ b => Tagged(ShortForm(i, b.metadata), b.data)
  }

  lazy val longForm: PackratParser[Tagged]  = ("@" ~> tagbody ~ doc ~ longFormClose) flatMap {
    // short close always succeeds
    case b ~ d ~ None => success(Tagged(b, d))
    // long close succeeds if the opening tag body is in ident form and opening id matches closing id
    case LongForm(Data(List(Left(oid))), metadata) ~ d ~ Some(cid) if oid.trim == cid.trim => success(Tagged(ShortForm(oid, metadata), d))
    // long close cannot be used for doc form
    case b ~ d ~ c => failure("Unable to match long form closing tag for " + b)
  }

  lazy val longFormClose: PackratParser[Option[String]] = ("@/" ^^ (_ => None) | ("@[/" <~ ident ~> "]").map(Some(_)))

  lazy val tagbody: PackratParser[LongForm] = "[" ~> ndoc ~ opt("|" ~> rep1sep(shortForm, "\\w+".r)) <~ "]" ^^ {
    case d ~ m => LongForm(d, m.toList.flatten)
  }

  def ntext: Parser[String] = """([^|@\\\]\[]|\\@|\\\||\\ )+""".r

  lazy val ndoc: PackratParser[Data] = rep(tagged.map(Right(_)) | ntext.map(Left(_))).map(Data(_))
}

