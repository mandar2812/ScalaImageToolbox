package org.kuleuven.mai.vision.image

import com.sksamuel.scrimage.{PixelTools, Image}
import org.apache.log4j.Logger
import org.apache.spark.rdd.RDD
import org.kuleuven.mai.vision.filters.Filter

import scala.collection.immutable.HashMap

/**
 * @author mandar2812
 *
 * A class to encapsulate an ARGB image as a
 * Spark [[RDD]]
 */
private[vision] class ImageMatrix(im: HashMap[(Int, Int), Int], w: Int, h: Int)
  extends Serializable{
  private val (_width, _height)= (w, h)

  private val _image = im

  private val logger = Logger.getLogger(this.getClass)

  def width = _width

  def height = _height

  def image = _image

  def neighbourhood(radius: Int)(x: Int, y: Int): Iterable[(Int, Int, Int, Int)] = {
    println("Calculating neighborhood for pixel: "+x+", "+y)
    val neighborhoodKeys = for(
      i <- math.max(0.0, x - radius).toInt to math.min(x + radius, this.width - 1);
      j <- math.max(0.0, y - radius).toInt to math.min(y + radius, this.height - 1)
    ) yield (i,j)

    neighborhoodKeys map (image get) map
      (pixel => {
        (PixelTools.alpha(pixel.get),
          PixelTools.red(pixel.get),
          PixelTools.green(pixel.get),
          PixelTools.blue(pixel.get))
      })
  }

}

object ImageMatrix {
  val logger = Logger.getLogger(this.getClass)
  private[vision] def apply(image: Image, path: String): ImageMatrix = {
    println("Creating HashMap for image")
    val rdd = for(i <- 0 until image.width; j <- 0 until image.height)
      yield ((i,j), image.pixel(i,j))
    println("HashMap created successfully")
    new ImageMatrix(HashMap(rdd:_*), image.width, image.height)
  }

  def applyFilter(image: Image,
                  filter: Filter[Iterable[(Int, Int, Int, Int)],
                    List[List[Int]]],
                  r: Int, path: String): Image = {
    val sparkImage = ImageMatrix(image, path)
    val neighborhoodFunction = sparkImage.neighbourhood(r) _
    image.map((x,y,pixel) => {
      val n = neighborhoodFunction(x,y)
      val vals_argb = filter.evaluate(n)
      PixelTools.argb(vals_argb(0),
        vals_argb(1),
        vals_argb(2),
        vals_argb(3))
    })
  }

  def alpha(image: RDD[((Long, Long), Array[Int])]) =
    image.map{pixel => (pixel._1, pixel._2(0))}

  def red(image: RDD[((Long, Long), Array[Int])]) =
    image.map{pixel => (pixel._1, pixel._2(1))}

  def green(image: RDD[((Long, Long), Array[Int])]) =
    image.map{pixel => (pixel._1, pixel._2(2))}

  def blue(image: RDD[((Long, Long), Array[Int])]) =
    image.map{pixel => (pixel._1, pixel._2(3))}
}
