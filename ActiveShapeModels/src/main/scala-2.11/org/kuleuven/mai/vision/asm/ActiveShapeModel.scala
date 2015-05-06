package org.kuleuven.mai.vision.asm

import breeze.linalg.{norm, sum, DenseVector}

/**
 * @author mandar2812
 */
class ActiveShapeModel(shapes: List[DenseVector[Double]]){

  def center: Unit = {
    this.shapes.map(v => ActiveShapeModel.centerLandmarks(v))
  }

  def scale: Unit = {
    this.shapes.map(v => ActiveShapeModel.scaleLandmarks(v))
  }

  def prettyPrint: Unit = {
    println("*** :Model shapes: ***")
    shapes.foreach{i => println(i)}
  }

  def getshapes: List[DenseVector[Double]] = this.shapes
}

object ActiveShapeModel {

  def centerLandmarks(vector: DenseVector[Double]): DenseVector[Double] = {
    val num: Int = vector.length/2
    val x = vector(0 to num - 1)
    val y = vector(num to vector.length - 1)
    val xmean = sum(x)/num
    val ymean = sum(y)/num
    x :-= xmean
    y :-= ymean
    DenseVector.vertcat(x,y)
  }

  def scaleLandmarks(vector: DenseVector[Double]): DenseVector[Double]
  = vector /= norm(vector, 2)


}
