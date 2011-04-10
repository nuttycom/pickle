package com.nommit.pickle

import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.combinator.PackratParsers

object PickleParser extends RegexParsers with PackratParsers {
  def parse(s: String) = parseAll(doc, s)

  // Just about any string that doesn't include spaces, control characters
  // or would confuse the parser can be used as an identifier. Bring on your Unicode!
  val Ident = """([^@\[\]\s\p{Cntrl}]+)""".r

  lazy val doc: PackratParser[Doc[Section]] = primitives | rep(tagged | ptext).map(Doc(_: _*))

  def primitives: PackratParser[Doc[Primitive]] = "#[" ~> repsep(ntext, "\\s*\\|\\s*".r) <~ "]" ^^ {
    values => Doc(values.map(v => Primitive(v.trim)): _*)
  }

  def text: Parser[String] = """([^@\\]|\\@)+""".r ^^ unescape
  def ptext: Parser[Primitive] = text.map(Primitive(_))

  def ntext: Parser[String] = """([^|@\\\]\[]|\\@|\\\||\\ )+""".r ^^ unescape
  def pntext: Parser[Primitive] = ntext.map(Primitive(_))

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

  lazy val tagbody: PackratParser[(Doc[Section], Metadata)] = "[" ~> ndoc ~ opt("|" ~> rep1sep(shortForm, "\\w+".r)) <~ "]" ^^ {
    case d ~ m => (d, Metadata(m.flatten.toSeq: _*))
  }


  def unescape(s: String) = s.replaceAll("""\\(?!\\)""", "")

  lazy val ndoc: PackratParser[Doc[Section]] = rep(tagged | pntext).map(
    l =>  Doc(
      l.foldRight(List.empty[Section]) {
        case (Primitive(s), Nil) =>
          // strip trailing unescaped spaces from the last element
          val ss = s.replaceAll("""(?<!\\)\s+$""", "")
          // if what remains is empty, drop the element
          if (ss.isEmpty) Nil else Primitive(ss) :: Nil

        case (elem, rest) => elem :: rest
      } : _*
    )
  )
}

