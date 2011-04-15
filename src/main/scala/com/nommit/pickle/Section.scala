package com.nommit.pickle

sealed trait Section

case class  Complex[+S <: Section](tag: Semantics, doc: Doc[S]) extends Section
case class  Primitive(value: String) extends Section
case object Separator extends Section









