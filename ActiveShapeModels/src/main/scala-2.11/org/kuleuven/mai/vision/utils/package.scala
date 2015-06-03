package org.kuleuven.mai.vision

import breeze.linalg.{DenseMatrix, DenseVector}

import scala.annotation.tailrec

/**
 * @author mandar2812
 */
package object utils {
  /**
   * Get the mean and variance of a data set
   * which is a [[List]] of [[DenseVector]].
   *
   * @param data The data set.
   *
   * @return A [[Tuple2]] containing the mean
   *         and variance * n-1.
   *
   * */

  def getStats(data: List[DenseVector[Double]]):
  (DenseVector[Double], DenseMatrix[Double]) = {
    def getStatsRec(d: List[DenseVector[Double]],
                    m: DenseVector[Double],
                    s: DenseMatrix[Double],
                    i: Int):
    (DenseVector[Double], DenseMatrix[Double]) = d match {
      case Nil => {
        m :/= i.toDouble
        s :/= i.toDouble
        //val m1: DenseVector[Double] = m/i.toDouble
        (m, s - (m*m.t))
      }
      case x :: rest => {
        getStatsRec(rest, m + x,
          s + x*x.t,
          i + 1)
      }
    }

    getStatsRec(data.tail, data.head,
      data.head * data.head.t,
      1)
  }
}
