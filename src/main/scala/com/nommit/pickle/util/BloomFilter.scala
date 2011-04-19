package com.nommit.pickle.util

import scala.collection.immutable.BitSet
import scala.util.Random

private[pickle] object BloomFilter {
  import math._
  
  private val ProbableDefaultMAndK = (2363, 2)

  val empty = apply(Nil)()

  def apply(elements: Seq[Any] = Nil)(n: Int = (elements.size + 1) * 2, p: Float = 0.33f): BloomFilter = {
    require(elements != null, "elements must not be null!")

    val (m, k) = optimalMAndK(n, p)
    new BloomFilter(hashes(m, k, elements), n, m, k)
  }

  private def hashes(m: Int, k: Int, elements: Seq[Any]) = BitSet(elements flatMap hash(m, k): _*)

  private def hash(m: Int, k: Int)(element: Any): Seq[Int] = {
    val rnd = new Random(element.hashCode)
    (1 until k) map { _ => abs(rnd.nextInt) % m }
  }

  private def optimalMAndK(n: Int, p: Float): (Int, Int) = {
    if (n == 1024 && p == 0.33f) {    // fast-path for the hard-coded case
      ProbableDefaultMAndK
    } else {
      val m = {
        val m = round((- n * log(p) / pow(log(2), 2)).toFloat)
        if (m > 0) m else 1
      }

      val k = if (n == 0) {
        1
      } else {
        val k = round(log(2).toFloat * m / n)
        if (k > 0) k else 1
      }

      (m, k)
    }
  }
}

private[pickle] class BloomFilter(private val bits: BitSet, private val n: Int, private val m: Int, private val k: Int) {
  import BloomFilter._

  def contains(element: Any): Boolean = hash(m, k)(element) forall bits.contains

  def +(element: Any): BloomFilter = new BloomFilter(this.bits | BitSet(hash(m, k)(element): _*), n, m, k)

  private def ensureCompatibility(that: BloomFilter) {
    require(
      this.n == that.n && this.m == that.m && this.k == that.k,
      "BloomFilter properties must match: got n:" + (n, that.n) + "; m:" + (m, that.m) + "; k:" + (k, that.k)
    )
  }

  def ++(that: BloomFilter): BloomFilter = {
    ensureCompatibility(that)
    new BloomFilter(this.bits | that.bits, n, m, k)
  }

  def append(that: BloomFilter, elements: Any*): BloomFilter = {
    ensureCompatibility(that)
    new BloomFilter(this.bits | that.bits | hashes(m, k, elements), n, m, k)
  }

  override def equals(a: Any) = a match {
    case that: BloomFilter => this.bits == that.bits && this.n == that.n && this.m == that.m && this.k == that.k
    case _ => false
  }
  
  override def hashCode = bits.hashCode ^ n ^ m ^ k
}
