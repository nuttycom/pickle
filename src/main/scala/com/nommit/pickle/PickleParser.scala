package com.nommit.pickle

import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.combinator.PackratParsers

class PickleParser extends RegexParsers with PackratParsers {
  def parse(s: String) = parseAll(doc, s)

  // Just about any string that doesn't include spaces, control characters
  // or would confuse the parser can be used as an identifier. Bring on your Unicode!
  val Ident = """([^@\[\]\s\p{Cntrl}]+)""".r

  lazy val doc: PackratParser[Doc[Section]] = rep(tagged | text | "@@") ^^ {
    values => Doc(fixWhitespace(values.collect { case s: Section => s }) : _*)
  }

  def text: Parser[Primitive] = """([^@\\]|\\@)+""".r ^^ (Primitive compose unescape)

  lazy val tagged: PackratParser[Complex[Section]] = (shortForm | longForm)

  lazy val shortForm: PackratParser[Complex[Section]] = "@" ~> Ident ~ tagbody ^^ {
    case ~(i, (doc, metadata)) => Complex(Tag(i, metadata), doc)
  }

  lazy val longForm: PackratParser[Complex[Section]]  = ("@" ~> tagbody ~ doc ~ longFormClose) flatMap {
    // translate long form to short form if the data is in ident form
    case (Doc(Primitive(Ident(oid))), metadata) ~ doc ~ None => success(Complex(Tag(oid, metadata), doc))
    // short close always succeeds
    case (metadoc, metadata) ~ doc ~ None => success(Complex(MetaTag(metadoc, metadata), doc))
    // long close succeeds if the opening tag body is in ident form and opening id matches closing id
    case (Doc(Primitive(oid)), metadata) ~ doc ~ Some(cid) if oid.trim == cid.trim => success(Complex(Tag(oid, metadata), doc))
    // long close cannot be used for doc form
    case body ~ doc ~ c => failure("Unable to match metatag closing tag for " + body + " at " + c)
  }

  lazy val longFormClose: PackratParser[Option[String]] = ("@/" ^^ (_ => None) | ("@[/" ~> Ident <~ "]").map(Some(_)))

  lazy val tagbody: PackratParser[(Doc[Section], Metadata)] = "[" ~> ndoc ~ opt("\\s*#\\s*".r ~> rep1sep(shortForm, "\\w+".r)) <~ "]" ^^ {
    case d ~ m => (d, Metadata(m.flatten.toSeq: _*))
  }

  def ntext: Parser[Primitive] = """([^|\[\]\\@#]|\\\||\\\[|\\\]|\\@|\\#|\\ )+""".r ^^ (Primitive compose unescape)

  lazy val ndoc: PackratParser[Doc[Section]] = rep(tagged | ntext | "(@@|\\|)".r) ^^ {
    values => Doc(fixWhitespace(values) : _*)
  }

  private val unescape = (s: String) => s.replaceAll("""\\(?!\\)""", "")

  private def fixWhitespace(values: List[AnyRef]): List[Section] = values.foldRight(List.empty[Section]) {
    case (Primitive(s1), Primitive(s2) :: rest) =>
      val s1s = s1.replaceAll("""(?<!\\)\s+$""", "")
      val s2s = s2.replaceAll("""^\s+""", "")

      if (s1s.isEmpty && s2s.isEmpty)          rest
      else if (s1s.isEmpty)  Primitive(s2s) :: rest
      else if (s2s.isEmpty)  Primitive(s1s) :: rest
      else Primitive(s1s) :: Primitive(s2s) :: rest

    case (Primitive(s), Nil) =>
      // strip trailing unescaped spaces from the last element; if what remains is empty, drop the element
      val ss = s.replaceAll("""(?<!\\)\s+$""", "")
      if (ss.isEmpty) Nil else Primitive(ss) :: Nil

    case (elem: Section, rest) => elem :: rest
    case (_, rest) => rest
  }
}

object PickleParser extends PickleParser

