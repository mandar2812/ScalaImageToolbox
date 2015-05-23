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

  private var MAX_ITERATIONS = 5

  private val scale = ActiveShapeModel.scaleLandmarks _

  private val center = ActiveShapeModel.centerLandmarks _

  private val process = ActiveShapeModel.process _

  private def alignment(x: DenseVector[Double])(y: DenseVector[Double]) =
    ActiveShapeModel.alignShapes(x)(y)

  private val (data, scales, centroids) = process(shapes)

  private val dims = shapes.toList.head._2.length

  private val asPoints = ActiveShapeModel.landmarksAsPoints _

  private val asLandmarks = ActiveShapeModel.pointsAsLandmarks _

  private val slopesOfNormals = ActiveShapeModel.calculateNormalSlopes _

  private val pixelneighborhood = ActiveShapeModel.generatePixelWindow(pixel_window) _

  private val asSlope = ActiveShapeModel.asSlopeVector _

  private val getImage = ActiveShapeModel.getImage _

  private val covAsArray = ActiveShapeModel.covAsArray _

  private val pixelGradients = ActiveShapeModel.getPixelGradients _

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
    this.data.reduce(_+_) /= this.numPoints.toDouble
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

  def pca(components: Int = dims): DenseMatrix[Double] = {
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

  def ShapeDistribution(n: Int = dims): MultivariateNormalDistribution = {
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

  def reconstructData(n: Int = dims): List[DenseVector[Double]] = {
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

  def fit(vector: DenseVector[Double],
          b_estimate: DenseVector[Double] =
          DenseVector.fill(this.meanShape.length)(0.0),
          t: Int = dims)
  :(DenseVector[Double], DenseMatrix[Double],
    Double, DenseVector[Double]) = {

    val (centered_vector, vector_centroid) = center(vector)
    val (norm_vector, s) = scale(centered_vector)
    var x = this.meanShape
    var b = b_estimate

    val translation = DenseVector.vertcat(
      DenseVector.fill(dims/2)(-1*vector_centroid(0)),
      DenseVector.fill(dims/2)(-1*vector_centroid(1))
    )

    val bdist = this.ShapeDistribution(t)
    var y = vector
    val rotation = alignment(norm_vector) _
    val eigenvectors = this.pca(t)
    (1 to this.MAX_ITERATIONS).foreach(i => {
      x = this.meanShape + eigenvectors*b
      y = rotation(x)*norm_vector
      val adjy: DenseVector[Double] = y / (y dot this.meanShape)
      b = eigenvectors.t * (adjy - this.meanShape)
      val prob = bdist.density(b.toArray)

    })
    (translation, rotation(x), s, b)
  }

  def CentroidDistribution: MultivariateNormalDistribution = {
    val (mean, covariance) = utils.getStats(this.centroids)
    new MultivariateNormalDistribution(
      mean.toArray,
      covAsArray(covariance))
  }

  def sampleCentroid = {
    val centroid_dist = this.CentroidDistribution
    println("Sample Centroid: "+centroid_dist.sample().toList)
    DenseVector(centroid_dist.sample())
  }

  def pixelStructure(level: Int)
  : List[(DenseVector[Double], DenseMatrix[Double])] = images(level).map(file =>{

      val adjust = ActiveShapeModel.adjustPointforLevel(level) _
      val (image_num, image) = getImage(level, file)
      val v = shapes(image_num)
      val landmarks = asPoints(v)
      val slopes = slopesOfNormals(landmarks)

      landmarks zip slopes map(model_point => {
        //find out the normalized gradient vector for the ith landmark
        //append it to pixelgradients(i)

        //to calculate the gradient, first calculate the
        //normal to the shape profile and sample pixels lying
        //on it (approximately)
        val (x, y) = adjust(model_point._1)

        val gradients: DenseVector[Double] =
          DenseVector(pixelGradients(
            pixelneighborhood(DenseVector(x,y),
              asSlope(model_point._2),
              image).map(_._2)).toArray)

        val norm_gradients: DenseVector[Double] = gradients / norm(gradients, 2)

        List(norm_gradients)
      })
    }).reduce((l1, l2) => {
        (l1 zip l2) map (c => c._1 ++ c._2)
      }) map utils.getStats

  def updateLandmarks(landmarks: DenseVector[Double],
                      level: Int, image: Image,
                      ns: Int = pixel_window + 5,
                      pixelStruct: List[(DenseVector[Double],
                        DenseMatrix[Double])]) = {
    val points = asPoints(landmarks)
    val slopes = slopesOfNormals(points)
    val newlandmarks = asLandmarks(points.zip(slopes).zip(pixelStruct).map((triple) => {
      val center_point = DenseVector(triple._1._1._1, triple._1._1._2)
      val n = pixelneighborhood(center_point, asSlope(triple._1._2), image)
      val(mean, covariance) = triple._2

      val new_point = n.sliding(pixel_window).reduce((window1, window2) => {
        val grad1 = scale(DenseVector(pixelGradients(window1.map(_._2)).toArray))._1
        val grad2 = scale(DenseVector(pixelGradients(window2.map(_._2)).toArray))._1
        grad1 :-= mean
        grad2 :-= mean

        val p1 = grad1.t*(inv(covariance)*grad1)
        val p2 = grad2.t*(inv(covariance)*grad2)
        val ans = if(p1 < p2) window1 else window2
        ans
      }).map(_._1).reduce(_+_)
      (new_point(0)/pixel_window, new_point(1)/pixel_window)
    }))
    (newlandmarks, norm(newlandmarks - landmarks, 2)/norm(landmarks, 2))
  }

  def MultiResolutionSearch(radiogram_levels: List[File]): Unit = {
    val levels = radiogram_levels.length

    (0 until levels).reverse.foreach{level =>

    }
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

  def getPixelGradients(p: List[Double]) = p.sliding(2)
    .toList
    .map(p =>
    p(1) - p.head)

  def covAsArray(covariance: DenseMatrix[Double]) =
    Array.tabulate(covariance.rows, covariance.cols){
    (i,j) => covariance(i,j)
  }

  def asSlopeVector(m: Double) = if(m < Double.PositiveInfinity)
    DenseVector(1.0, m) else DenseVector(0.0, 1.0)

  def getImage(level: Int, file: File) = {
    val image_num = if(level == 0) {
      file.getName.replaceAll(".tiff", "").toInt
    } else {
      file.getName.replaceAll("_level_"+level+".png", "").toInt
    }

    val image = if(level == 0) TiffReader.read(FileUtils.openInputStream(file))
    else Image(FileUtils.openInputStream(file))
    (image_num, image)
  }

  def generatePixelWindow(pixel_window: Int)
                         (center_point: DenseVector[Double],
                          slopevec: DenseVector[Double],
                          image: Image) =
    List.tabulate(2*pixel_window+1){l =>
      val point: DenseVector[Double] = center_point + slopevec*(l-pixel_window).toDouble
      (point, PixelTools.gray(image.pixel(point(0).toInt, point(1).toInt)).toDouble)
    }

  def adjustPointforLevel(l: Int = 0)(p: (Double, Double)) =
    (p._1/math.pow(2, l), p._2/math.pow(2, l))

  def landmarksAsPoints(v: DenseVector[Double]) = List.tabulate(v.length/2){k =>
    (v(k), v(k + v.length/2))
  }

  def pointsAsLandmarks(v: List[(Double, Double)]): DenseVector[Double] =
    DenseVector.vertcat(
    DenseVector(v.map(_._1).toArray),
    DenseVector(v.map(_._2).toArray))

  def calculateNormalSlopes(landmarks: List[(Double, Double)]) = {
    val slopes = landmarks.sliding(2).map((points) => {
      if(points(1)._2 != points.head._2) {
        -1*(points(1)._1 - points.head._1)/(points(1)._2 - points.head._2)
      } else {
        Double.PositiveInfinity
      }
    }).toList

    slopes ::: List(slopes.last)
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
