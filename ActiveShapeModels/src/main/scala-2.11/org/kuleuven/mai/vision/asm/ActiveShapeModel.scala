package org.kuleuven.mai.vision.asm

import breeze.linalg.{norm, sum, DenseVector}

/**
 * @author mandar2812
 */
class ActiveShapeModel {
  
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
