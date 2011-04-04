package com.nommit.pickle

sealed trait Section

case class Tagged(tag: Semantics, doc: Doc) extends Section
case class Text(text: String) extends Section










