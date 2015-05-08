package org.kuleuven.mai.vision.asm

import breeze.linalg._
import breeze.linalg.svd.DenseSVD

import scala.collection.mutable.{MutableList => ML}

/**
 * @author mandar2812
 */
class ActiveShapeModel(shapes: List[DenseVector[Double]]){

  private var EPSILON: Double = 0.001

  private val data: ML[DenseVector[Double]] =
    ML(shapes.map(v => ActiveShapeModel.centerLandmarks(v))
      .map(v => ActiveShapeModel.scaleLandmarks(v)):_*)

  def setTolerance(e: Double): this.type = {
    this.EPSILON = e
    this
  }

  def numPoints = this.data.length

  def getTolerance: Double = this.EPSILON

  def prettyPrint: Unit = {
    println("*** :Model shapes: ***")
    shapes.foreach{i => println(i)}
  }

  def getRawShapes: List[DenseVector[Double]] = shapes

  def getNormalizedShapes: ML[DenseVector[Double]] = this.data

  def align(v: DenseVector[Double] = this.data.head): Unit = {
    val calculateRotation = ActiveShapeModel.alignShapes(v) _

    (1 to this.data.length).foreach(i => {
      val vec = this.data(i-1)
      val rotation = calculateRotation(vec)
      this.data(i-1) = rotation * vec
    })
  }

  def meanShape: DenseVector[Double] = {
    val mean = DenseVector.zeros[Double](this.data.head.length)
    this.data.foreach(vec => mean :+= vec)
    mean :/= this.data.length.toDouble
    mean
  }

  def alignShapes: DenseVector[Double] = {
    //Iteratively align the set of shapes
    //and calculate the mean shape resulting
    //from the procedure.
    this.align()
    var oldMean = this.meanShape
    val rotation = ActiveShapeModel.alignShapes(this.data.head) _
    var newMean = rotation(oldMean)*oldMean

    while(norm(newMean-oldMean, 2) >= this.EPSILON) {
      this.align(newMean)
      oldMean = this.meanShape
      newMean = rotation(oldMean)*oldMean
    }
    newMean
  }

  private def pca(data: DenseMatrix[Double], components: Int): DenseMatrix[Double] = {
    val d = zeroMean(data)
    val decomp = svd(d)
    val v = decomp.Vt
    val model = v(0 until components/2, ::) //top 'components' eigenvectors
    val ymodel = v(40 until 40+(components/2), ::)
    data * DenseMatrix.vertcat(model, ymodel).t
  }

  private def mean(v: Vector[Double]) = v.valuesIterator.sum / v.size

  private def zeroMean(m: DenseMatrix[Double]) = {
    val copy = m.copy
    for (c <- 0 until m.cols) {
      val col = copy(::, c)
      val colMean = mean(col)
      col -= colMean
    }
    //    println("data \n" + m)
    //    println("mean \n" + copy)
    copy
  }

  def decomposeShape(n: Int = this.data.head.length): DenseMatrix[Double] = {
    val datamatrix = DenseMatrix.vertcat(this.data.map{_.toDenseMatrix}.toList:_*)
    this.pca(datamatrix, n)
  }
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

  def alignShapes(shape1: DenseVector[Double])(shape2: DenseVector[Double])
  : DenseMatrix[Double] = {
    val num: Int = shape1.length/2
    var numer: Double = 0.0
    var denom: Double = 0.0
    val x1 = shape1(0 to num - 1)
    val y1 = shape1(num to shape1.length - 1)
    val x2 = shape2(0 to num - 1)
    val y2 = shape2(num to shape2.length - 1)

    (0 to num - 1).foreach{i =>
      numer += (x2(i)*y1(i) - x1(i)*y2(i))
      denom += (y2(i)*y1(i) + x1(i)*x2(i))
    }
    val theta = math.atan2(numer,denom)

    val Block1 = diag(DenseVector.fill(num)(math.cos(theta)))
    val Block2 = diag(DenseVector.fill(num)(-1*math.sin(theta)))
    val Block3 = diag(DenseVector.fill(num)(math.sin(theta)))
    val Block4 = diag(DenseVector.fill(num)(math.cos(theta)))

    DenseMatrix.horzcat(
      DenseMatrix.vertcat(Block1, Block3),
      DenseMatrix.vertcat(Block2, Block4)
    )
  }

}
