package org.kuleuven.mai.vision.filters

/**
 * @author mandar2812
 */
trait Filter[T, U]{
  def evaluate(
      window: T): Array[Int]
}
