package org.kuleuven.mai.vision.asm

import java.io.File

import breeze.linalg._
import com.sksamuel.scrimage.{Image, PixelTools}
import com.sksamuel.scrimage.io.TiffReader
import org.apache.commons.io.FileUtils
import org.apache.commons.math3.distribution.MultivariateNormalDistribution
import org.kuleuven.mai.vision.utils

import scala.collection.mutable.{MutableList => ML}

/**
 * @author mandar2812
 */
class ActiveShapeModel(shapes: Map[Int, DenseVector[Double]],
                       images: List[List[File]],
                       pixel_window: Int = 5){

  private var EPSILON: Double = 0.0001

  private var MAX_ITERATIONS = 1

  private val scale = ActiveShapeModel.scaleLandmarks _

  private val center = ActiveShapeModel.centerLandmarks _

  private val process = ActiveShapeModel.process _

  private def alignment(x: DenseVector[Double])(y: DenseVector[Double]) =
    ActiveShapeModel.alignShapes(x)(y)

  private val (data, scales, centroids) = process(shapes)

  private val dims = shapes.toList.head._2.length

  private var SHAPE_DENSITY_THRESHOLD = 0.5*math.pow(2*math.Pi, -1*dims/2)

  def setTolerance(e: Double): this.type = {
    this.EPSILON = e
    this
  }

  def setMaxIterations(i: Int): this.type = {
    this.MAX_ITERATIONS = i
    this
  }

  def numPoints = this.data.length

  def getTolerance: Double = this.EPSILON

  def prettyPrint: Unit = {
    println("*** :Model shapes: ***")
    shapes.foreach{i => println(i)}
  }

  def getRawShapes: List[DenseVector[Double]] = shapes.toList.map(_._2)

  def getNormalizedShapes: ML[DenseVector[Double]] = this.data

  def getNormalizedShapesAsMatrix =
    DenseMatrix.vertcat(this.data.map{_.toDenseMatrix}.toList:_*)

  def align(v: DenseVector[Double] = this.data.head): Unit = {
    val calculateRotation = alignment(v) _

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
    val rotation = alignment(this.data.head) _
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
    val decomposition = svd(d)
    val v = decomposition.Vt
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
    val shapeslist = this.data.toList.map(shape => {
      eigenvectors.t * (shape - mean)
    })

    val (meanvec, covariance) = utils.getStats(shapeslist)
    val d = det(covariance)
    val adjcov = if(d == 0.0) covariance + DenseMatrix.eye[Double](shapeslist.head.length)
    else covariance
    val da = det(adjcov)
    this.SHAPE_DENSITY_THRESHOLD = 0.5*math.pow(2*math.Pi, -1*dims/2)*math.pow(da, -0.5)
    new MultivariateNormalDistribution(meanvec.toArray,
      Array.tabulate(covariance.rows, covariance.cols){
        (i,j) => adjcov(i, j)
      })
  }

  def reconstructData(n: Int = this.data.head.length): List[DenseVector[Double]] = {
    val eigenvectors = this.pca(n)
    val mean = this.meanShape
    (0 until this.data.length).toList.map(shape => {
      val translation = DenseVector.vertcat(
        DenseVector.fill(dims/2)(centroids(shape)(0)),
        DenseVector.fill(dims/2)(centroids(shape)(1))
      )
      val b = eigenvectors.t * (this.data(shape) - mean)
      val x = mean + (eigenvectors * b)
      (x:*=scales(shape)) + translation
    })
  }

  def fit(vector: DenseVector[Double])
  :(DenseVector[Double], DenseMatrix[Double],
    Double, DenseVector[Double]) = {

    val (centered_vector, vector_centroid) = center(vector)
    val (norm_vector, s) = scale(centered_vector)
    var x = this.meanShape
    var b = DenseVector.fill(x.length)(0.0)

    val translation = DenseVector.vertcat(
      DenseVector.fill(x.length/2)(-1*vector_centroid(0)),
      DenseVector.fill(x.length/2)(-1*vector_centroid(1))
    )

    val bdist = this.ShapeDistribution()
    var y = vector
    val rotation = alignment(norm_vector) _
    val eigenvectors = this.pca()
    (1 to this.MAX_ITERATIONS).foreach(i => {
      y = rotation(x)*norm_vector
      b = eigenvectors.t * (y - this.meanShape)
      val prob = bdist.density(b.toArray)
      x = this.meanShape + eigenvectors*b
    })
    (translation, rotation(x), s, b)
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

  def pixelStructure(level: Int)
  : List[(DenseVector[Double], DenseMatrix[Double])] = {
    val pixelgradients:ML[List[DenseVector[Double]]] =
      ML.fill(shapes.head._2.length/2)(List())

    images(level).foreach(file =>{
      val image_num = if(level == 0) {
        file.getName.replaceAll(".tiff", "").toInt
      } else {
        file.getName.replaceAll("_level_"+level+".png", "").toInt
      }

      println("File Name: "+file.getName)
      println("Image Num "+(file.getName diff "_level_"+level+".png"))
      val image = if(level == 0) TiffReader.read(FileUtils.openInputStream(file))
      else Image(FileUtils.openInputStream(file))

      val v = shapes(image_num)
      val landmarks = List.tabulate(shapes.head._2.length/2){k =>
        (v(k), v(k + shapes.head._2.length/2))
      }

      val slopes = landmarks.sliding(2).map((points) => {
        if(points(1)._2 != points.head._2) {
          -1*(points(1)._1 - points.head._1)/(points(1)._2 - points.head._2)
        } else {
          Double.PositiveInfinity
        }
      }).toList

      (0 until shapes.head._2.length/2).foreach(model_point => {
        //find out the normalized gradient vector for the ith landmark
        //append it to pixelgradients(i)

        //to calculate the gradient, first calculate the
        //normal to the shape profile and sample pixels lying
        //on it (approximately)
        val (x, y) = (landmarks(model_point)._1/math.pow(2, level), landmarks(model_point)._2/math.pow(2, level))

        val center_point = DenseVector(x,y)
        val m = slopes(math.min(model_point, slopes.length-1))
        val slopevec = if(m < Double.PositiveInfinity) DenseVector(1.0, m) else DenseVector(0.0, 1.0)
        val gradients: DenseVector[Double] = DenseVector(List.tabulate(2*pixel_window+1){l =>
          val point: DenseVector[Double] = center_point + slopevec*(l-pixel_window).toDouble

          PixelTools.gray(image.pixel(point(0).toInt, point(1).toInt)).toDouble
        }.sliding(2).toList.map(p => p(1) - p.head).toArray)

        val norm_gradients: DenseVector[Double] = gradients / norm(gradients, 2)

        pixelgradients(model_point) ++= List(norm_gradients)
      })
    })
    pixelgradients.toList map utils.getStats
  }

  def MultiResolutionSearch(image: File): Unit = {

  }
}

object ActiveShapeModel {

  def apply(shapes: Map[Int, DenseVector[Double]],
            images: List[List[File]] = List(),
            k: Int = 5): ActiveShapeModel = {
    new ActiveShapeModel(
      shapes,
      images, k)
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

  def scaleLandmarks(vector: DenseVector[Double]): (DenseVector[Double], Double)
  = (vector /= norm(vector, 2), norm(vector, 2))

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

  def process(shapes: Map[Int, DenseVector[Double]]):
  (ML[DenseVector[Double]], List[Double], List[DenseVector[Double]]) = {
    val l = shapes.map(s => {
      val (cv, c) = centerLandmarks(s._2)
      val (nv, n) = scaleLandmarks(cv)
      (nv, n, c)
    }).unzip3
    (ML(l._1.toList:_*), l._2.toList, l._3.toList)
  }

}
