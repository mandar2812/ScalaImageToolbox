package org.kuleuven.mai.vision.asm

import breeze.linalg.{norm, sum, DenseVector}

/**
 * @author mandar2812
 */
class ActiveShapeModel(shapes: List[DenseVector[Double]]){
  private val data: List[DenseVector[Double]] =
    shapes.map(v => ActiveShapeModel.centerLandmarks(v))
      .map(v => ActiveShapeModel.scaleLandmarks(v))

  def prettyPrint: Unit = {
    println("*** :Model shapes: ***")
    shapes.foreach{i => println(i)}
  }

  def getRawShapes: List[DenseVector[Double]] = shapes
  def getNormalizedShapes: List[DenseVector[Double]] = this.data
}

object ActiveShapeModel {

  def centerLandmarks(vector: DenseVector[Double]): DenseVector[Double] = {
    val num: Int = vector.length/2
    val x = vector(0 to num - 1)
    val y = vector(num to vector.length - 1)
    val xmean = sum(x)/num
    val ymean = sum(y)/num
    val a = x - xmean
    val b = y - ymean
    DenseVector.vertcat(a,b)
  }

  def scaleLandmarks(vector: DenseVector[Double]): DenseVector[Double]
  = vector /= norm(vector, 2)

  def center(list: List[DenseVector[Double]]): List[DenseVector[Double]] = {
    list.map(v => ActiveShapeModel.centerLandmarks(v))
  }

  def scale(list: List[DenseVector[Double]]): List[DenseVector[Double]] = {
    list.map(v => ActiveShapeModel.scaleLandmarks(v))
  }

}
