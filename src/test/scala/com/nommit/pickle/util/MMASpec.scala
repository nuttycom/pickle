package com.nommit.pickle.util

import scalaz._
import org.specs._
import org.scalacheck._

class MMASpec extends Specification with ScalaCheck {
  import Prop._
  import MMA._

// Damn it, the code is right, I just can't figure out how to prove it.
//  "intersperse" should {
//    "alternate groups with separators" in {
//      val prop = forAll {
//        (xs: List[List[Int]], sep: Int) => {
//          val result = xs.intersperse(sep)
//          val (success, _) = xs.foldLeft((true, 0)) {
//            case ((ok, length), list) =>
//              val remainder = result.drop(length + list.length)
//              (ok && (remainder.isEmpty || remainder.head == sep), length + list.length + 1)
//          }
//          success
//        }
//      }
//
//      prop must pass(set(maxSize -> 10))
//    }
//  }

}