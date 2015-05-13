package org.kuleuven.mai.vision.image

import org.apache.spark.rdd.RDD

/**
 * @author mandar
 */
class SparkHistogram(vals: RDD[Double]) {
  private val _values = vals
  private val hist: (Int) => (Array[Double], Array[Long]) = _values.histogram
  def values = _values
  def +(other: SparkHistogram): SparkHistogram =
    SparkHistogram(this.values union other.values)
}

object SparkHistogram {
  def apply(values: RDD[Double]) = new SparkHistogram(values)
}