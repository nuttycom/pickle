package com.nommit.pickle

sealed trait Section

case class Complex[+S <: Section](tag: Semantics, value: Doc[S]) extends Section
case class Primitive(value: String) extends Section










