package org.kuleuven.mai.vision.asm

import java.io.File

import breeze.linalg._
import breeze.linalg.svd.DenseSVD
import org.apache.commons.math3.distribution.MultivariateNormalDistribution
import org.kuleuven.mai.vision.utils

import scala.collection.mutable.{MutableList => ML}

/**
 * @author mandar2812
 */
class ActiveShapeModel(shapes: List[DenseVector[Double]]){

  private var EPSILON: Double = 0.0001

  private var MAX_ITERATIONS = 20

  private val data: ML[DenseVector[Double]] =
    ML(shapes.map(v => ActiveShapeModel.centerLandmarks(v))
      .map(v => ActiveShapeModel.scaleLandmarks(v._1)):_*)

  private val centroids: List[DenseVector[Double]] =
    shapes.map(v => ActiveShapeModel.centerLandmarks(v)._2)

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

  def getNormalizedShapesAsMatrix =
    DenseMatrix.vertcat(this.data.map{_.toDenseMatrix}.toList:_*)

  def align(v: DenseVector[Double] = this.data.head): Unit = {
    val calculateRotation = ActiveShapeModel.alignShapes(v) _

    (1 to this.data.length).foreach(i => {
      val vec = this.data(i-1)
      val rotation = calculateRotation(vec)
      this.data(i-1) = rotation * vec
    })
  }

  def meanShape: DenseVector[Double] = {
    this.data.reduce(_+_) /= this.data.length.toDouble
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

  def pca(components: Int = this.data.head.length): DenseMatrix[Double] = {
    val dataMat = this.getNormalizedShapesAsMatrix
    val d = zeroMean(dataMat)
    val decomp = svd(d)
    val v = decomp.Vt
    val model = v(0 until components/2, ::) //top 'components' eigenvectors
    val ymodel = v(40 until 40+(components/2), ::)
    DenseMatrix.vertcat(model, ymodel).t
  }

  private def mean(v: Vector[Double]) = v.valuesIterator.sum / v.size

  private def zeroMean(m: DenseMatrix[Double]) = {
    val copy = m.copy
    for (c <- 0 until m.cols) {
      val col = copy(::, c)
      val colMean = mean(col)
      col -= colMean
    }
    copy
  }

  def ShapeDistribution(n: Int = this.data.head.length): MultivariateNormalDistribution = {
    val eigenvectors = this.pca(n)
    val mean = this.meanShape
    val shapes = this.data.toList.map(shape => {
      eigenvectors.t * (shape - mean)
    })

    val (meanvec, covariance) = utils.getStats(shapes)

    new MultivariateNormalDistribution(meanvec.toArray,
      Array.tabulate(covariance.rows, covariance.cols){
        (i,j) => covariance(i, j)
      })
  }

  def reconstructData(n: Int = this.data.head.length): List[DenseVector[Double]] = {
    val eigenvectors = this.pca(n)
    val mean = this.meanShape
    this.data.toList.map(shape => {
      val b = eigenvectors.t * (shape - mean)
      mean + (eigenvectors * b)
    })
  }

  def fit(vector: DenseVector[Double])
  :(DenseVector[Double], DenseMatrix[Double],
    Double, DenseVector[Double]) = {

    val (centered_vector, vector_centroid) = ActiveShapeModel.centerLandmarks(vector)

    val scale = norm(centered_vector, 2)
    val norm_vector = ActiveShapeModel.scaleLandmarks(centered_vector)

    var x = this.meanShape
    var b = DenseVector.fill(x.length)(0.0)

    val translation = DenseVector.vertcat(
      DenseVector.fill(x.length/2)(-1*vector_centroid(0)),
      DenseVector.fill(x.length/2)(-1*vector_centroid(1))
    )

    var y = vector
    val rotation = ActiveShapeModel.alignShapes(norm_vector) _
    val eigenvectors = this.pca()
    (1 to this.MAX_ITERATIONS).foreach(i => {
      y = rotation(x)*norm_vector
      b = eigenvectors.t * (y - this.meanShape)
      x = this.meanShape + eigenvectors*b
    })
    (translation, rotation(x), scale, b)
  }

  def CentroidDistribution: MultivariateNormalDistribution = {
    val (mean, covariance) = utils.getStats(this.centroids)
    new MultivariateNormalDistribution(mean.toArray,
      Array.tabulate(covariance.rows, covariance.cols){
        (i,j) => covariance(i,j)
      })
  }

  def sampleCentroid = {
    val centroid_dist = this.CentroidDistribution
    println("Sample Centroid: "+centroid_dist.sample().toList)
    DenseVector(centroid_dist.sample())
  }
}

object ActiveShapeModel {

  def apply(shapes: List[DenseVector[Double]], images: Array[File]): ActiveShapeModel = {

    new ActiveShapeModel(shapes)
  }

  def centerLandmarks(vector: DenseVector[Double]):
  (DenseVector[Double], DenseVector[Double]) = {
    val num: Int = vector.length/2
    val x = vector(0 to num - 1)
    val y = vector(num to vector.length - 1)
    val xmean = sum(x)/num
    val ymean = sum(y)/num
    val a = x - xmean
    val b = y - ymean
    (DenseVector.vertcat(a,b), DenseVector(xmean, ymean))
  }

  def scaleLandmarks(vector: DenseVector[Double]): DenseVector[Double]
  = vector /= norm(vector, 2)

  def center(list: List[DenseVector[Double]]): List[DenseVector[Double]] = {
    list.map(v => ActiveShapeModel.centerLandmarks(v)._1)
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
