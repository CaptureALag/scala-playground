package cf691.d

object SwapLexicographicMaximizer {
  def apply[T, K](swapGraph : Graph, elems : Seq[T])(sortKey: T => K)(implicit ord : Ordering[K]) : Seq[T] = {
    val disjointUnions = DisjointUnionFinder(swapGraph)
    val elemsIndexed = elems.toIndexedSeq
    val sortedUnions = disjointUnions.map(union => union.sorted.zip(union.map(elemsIndexed).sortBy(sortKey)))
    val flattenedUnions = sortedUnions.flatten.sortBy(_._1)
    flattenedUnions.map(_._2)
  }
}
