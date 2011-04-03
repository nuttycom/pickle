package com.nommit.pickle

import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.combinator.PackratParsers

case class Doc private[pickle] (sections: List[Section])

object Doc {
  val Empty = Doc(Nil)
  def text(s: String) = Doc(List(Text(s)))
  def tagged(t: Semantics, d: Doc) = Doc(List(Tagged(t, d)))
  def apply(s: Section*): Doc = Doc(s.toList)
}

case class Metadata private[pickle] (elements: List[Tagged])

object Metadata {
  val Empty = Metadata(Nil)
  def apply(m: Tagged*): Metadata = Metadata(m.toList)
}

sealed trait Semantics {
  def metadata: Metadata
}

case class Tag(ident: String, metadata: Metadata = Metadata.Empty) extends Semantics
case class MetaTag(ident: Doc,  metadata: Metadata = Metadata.Empty) extends Semantics

sealed trait Section
case class Tagged(tag: Semantics, doc: Doc) extends Section
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
    case ~(i, (doc, metadata)) => Tagged(Tag(i, metadata), doc)
  }

  lazy val longForm: PackratParser[Tagged]  = ("@" ~> tagbody ~ doc ~ longFormClose) flatMap {
    // translate long form to short form if the data is in ident form
    case (Doc(List(Text(Ident(oid)))), metadata) ~ doc ~ None => success(Tagged(Tag(oid, metadata), doc))
    // short close always succeeds
    case (metadoc, metadata) ~ doc ~ None => success(Tagged(MetaTag(metadoc, metadata), doc))
    // long close succeeds if the opening tag body is in ident form and opening id matches closing id
    case (Doc(List(Text(oid))), metadata) ~ doc ~ Some(cid) if oid.trim == cid.trim => success(Tagged(Tag(oid, metadata), doc))
    // long close cannot be used for doc form
    case body ~ doc ~ c => failure("Unable to match metatag closing tag for " + body + " at " + c)
  }

  lazy val longFormClose: PackratParser[Option[String]] = ("@/" ^^ (_ => None) | ("@[/" ~> Ident <~ "]").map(Some(_)))

  lazy val tagbody: PackratParser[(Doc, Metadata)] = "[" ~> ndoc ~ opt("|" ~> rep1sep(shortForm, "\\w+".r)) <~ "]" ^^ {
    case d ~ m => (d, Metadata(m.toList.flatten))
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

