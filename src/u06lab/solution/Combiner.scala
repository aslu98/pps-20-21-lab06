package u06lab.solution

import scala.Double.MinValue.>

/**
  * 1) Implement trait Functions with an object FunctionsImpl such that the code
  * in TryFunctions works correctly.
 */

trait Functions {
  def sum(a: List[Double]): Double
  def concat(a: Seq[String]): String
  def max(a: List[Int]): Int // gives Int.MinValue if a is empty
}

object FunctionsImpl extends Functions {

  override def sum(a: List[Double]): Double = a.foldRight(0.0)(_+_)

  override def concat(a: Seq[String]): String = a.foldRight("")(_+_)

  override def max(a: List[Int]): Int = a.foldRight(Int.MinValue)((a, b) => if (a > b) a else b)
}


/*
  * 2) To apply DRY principle at the best,
  * note the three methods in Functions do something similar.
  * Use the following approach:
  * - find three implementations of Combiner that tell (for sum,concat and max) how
  *   to combine two elements, and what to return when the input list is empty
  * - implement in FunctionsImpl a single method combiner that, other than
  *   the collection of A, takes a Combiner as input
  * - implement the three methods by simply calling combiner
  *
  * When all works, note we completely avoided duplications..
 */

trait Combiner[A] {
  def unit: A
  def combine(a: A, b: A): A
}

object CombinersFactory {
  def sumCombiner: Combiner[Double] = new Combiner[Double] {
    override def unit = 0.0
    override def combine(a: Double , b: Double) = a + b
  }

  def concatCombiner: Combiner[String] = new Combiner[String] {
    override def unit = ""
    override def combine(a: String , b: String) = a + b
  }

  def maxCombiner: Combiner[Int] = new Combiner[Int] {
    override def unit = Int.MinValue
    override def combine(a: Int , b: Int): Int = if (a > b) a else b
  }
}

object FunctionsCombinerImpl extends Functions {
  private def combine[A](a: List[A], c: Combiner[A]) = a.foldRight(c.unit)(c.combine)

  import CombinersFactory._
  override def sum(a: List[Double]): Double = combine[Double](a, sumCombiner)
  override def concat(a: Seq[String]): String = combine[String](a.toList, concatCombiner)
  override def max(a: List[Int]): Int = combine[Int](a, maxCombiner)
}