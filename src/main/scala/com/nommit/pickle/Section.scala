package com.nommit.pickle

import com.nommit.pickle.util.BloomFilter

sealed trait Section {
  /**
   * A vector of bloom filters where each filter covers a successive layer of metadata.
   * For example, in the following document:
   *   @a[a' # @b[b' # @c[c']]]
   *
   * bloomFilters will have size 3 where  bloomFilters(0) covers only the document a'; bloomFilters(1)
   * covers the document b', and bloomFilters(2) covers c'
   */
  private[pickle] def bloomFilters = Vector.empty[BloomFilter]

  def complexity: Int
}

case class  Complex[+S <: Section](tag: Semantics, doc: Doc[S]) extends Section {
  private[pickle] override lazy val bloomFilters = tag match {
    case Tag(ident, metadata) => doc.bloomFilters.zipAll((BloomFilter.Empty + ident) +: metadata.bloomFilters, BloomFilter.Empty, BloomFilter.Empty) map {
      case (f1, f2) => f1 ++ f2
    }

    // TODO: what to do with the bloom filter for ident in a metatag?
    case MetaTag(ident, metadata) => doc.bloomFilters.zipAll(BloomFilter.Empty +: metadata.bloomFilters, BloomFilter.Empty, BloomFilter.Empty) map {
      case (f1, f2) => f1 ++ f2
    }
  }

  override def complexity = 1 + tag.metadata.complexity + doc.complexity
}

case class  Primitive(value: String) extends Section {
  override def complexity = value.length / 20
}

case object Separator extends Section {
  override def complexity = 0
}

