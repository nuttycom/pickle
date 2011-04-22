package com.nommit.pickle

import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.combinator.PackratParsers
import annotation.tailrec

class PickleParser extends RegexParsers with PackratParsers {
  import PickleParser.{unescape, fixWhitespace}

  def parse(s: String) = parseAll(doc, s)

  def unsafeParse(s: String) = parse(s) match {
    case Success(doc, rest) if rest.atEnd => doc
    case error => Predef.error("Shouldn't have used unsafeParse: " + error)
  }

  // Just about any string that doesn't include spaces, control characters
  // or would confuse the parser can be used as an identifier. Bring on your Unicode!
  val Ident = """([^@\[\]\s\p{Cntrl}]+)""".r

  lazy val doc: PackratParser[Doc[Section]] = rep(tagged | text | (literal("@@").map(_ => Separator))) ^^ {
    values => Doc(fixWhitespace(values): _*)
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

  lazy val ndoc: PackratParser[Doc[Section]] = rep(tagged | ntext | (regex("(@@|\\|)".r).map(_ => Separator))) ^^ {
    values => Doc(fixWhitespace(values) : _*)
  }
}

object PickleParser extends PickleParser {
  val unescape = (s: String) => s.replaceAll("""\\(?!\\)""", "")

  def fixWhitespace(values: List[Section]): List[Section] = {
    def trimRight(s: String) = s.replaceAll("""(?<!\\)\s+$""", "")
    def trimLeft(s: String)  = s.replaceAll("""^\s+""", "")
    def primitives(l: String*): List[Primitive] = l.filter(!_.isEmpty).map(Primitive(_)).toList

    val tail = values.foldRight(List.empty[Section]) {
      case (p1 @ Primitive(s1), rest) => rest match {
        case Separator :: Primitive(s2) :: xs => primitives(trimRight(s1)) ::: (Separator :: (primitives(trimLeft(s2)) ::: xs))
        case Primitive(s2) :: xs              => primitives(trimRight(s1), trimLeft(s2)) ::: xs
        case Nil                              => primitives(trimRight(s1))
        case xs                               => p1 :: xs
      }

      case (s, rest) => s :: rest
    }

    tail.headOption.map({ case Primitive(s) => primitives(trimLeft(s)) ::: tail.tail; case x => x :: tail.tail }).getOrElse(Nil)
  }
}

