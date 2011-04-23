package com.nommit.pickle.util

import scalaz._

class MMA[M[_], N[_ <: A], A](value: M[N[A]]) {
  def intersperse(a: A)(implicit f: Foldable[M], reducer:Reducer[A, N[A]]): N[A] = {
    f.foldLeft(value, reducer.monoid.zero, (into: N[A], v: N[A]) => if (into == reducer.monoid.zero) v else reducer.monoid.append(into, reducer.cons(a, v)))
  }
}

object MMA {
  implicit def mma[M[_], N[_ <: A], A](v: M[N[A]]): MMA[M, N, A] = new MMA[M, N, A](v)
}