package org.kuleuven.mai.vision.image

import java.io.File

import com.github.tototoshi.csv.CSVWriter
import com.sksamuel.scrimage.{PixelTools, Image}
import org.apache.log4j.Logger
import org.apache.spark.SparkContext
import org.apache.spark.rdd.RDD
import org.kuleuven.mai.vision.filters.Filter

/**
 * @author mandar2812
 *
 * A class to encapsulate an ARGB image as a
 * Spark [[RDD]]
 */
private[vision] class SparkImage(image: RDD[((Int, Int), Int)], w: Int, h: Int)
  extends Serializable{
  private val (_width, _height)= (w, h)

  private val logger = Logger.getLogger(this.getClass)

  def width = _width

  def height = _height

  def neighbourhood(radius: Int)(x: Int, y: Int): Iterable[(Int, Int, Int, Int)] = {
    this.logger.info("Calculating neighborhood for pixel: "+x+", "+y)
    val neighborhoodKeys = for(
      i <- math.max(0.0, x - radius).toInt to math.min(x + radius, this.width - 1);
      j <- math.max(0.0, y - radius).toInt to math.min(y + radius, this.height - 1)
    ) yield (i,j)

    this.image.filter(pixel =>
      math.abs(pixel._1._1 - x) <= radius &&
        math.abs(pixel._1._2 - y) <= radius)
      .map(pixel => {
      (PixelTools.alpha(pixel._2),
        PixelTools.red(pixel._2),
        PixelTools.green(pixel._2),
        PixelTools.blue(pixel._2))
    }).collect().toIterable
  }

}

object SparkImage {
  val logger = Logger.getLogger(this.getClass)
  private[vision] def apply(image: Image, sc: SparkContext, path: String): SparkImage = {
    logger.info("Creating RDD for image")
    val writer = CSVWriter.open(new File(path.replaceAll("\\.tif", ".csv")), append = false)
    logger.info("Writing image to temp file: "+path.replaceAll("\\.tif", ".csv"))
    image.foreach((x,y,pixel) => {
      writer.writeRow(List(x,y,pixel))
    })
    writer.close()
    val rdd = sc.textFile(path.replaceAll("\\.tif", ".csv")).map{line => {
      val split = line.split(',')
      ((split(0).toInt, split(1).toInt), split(2).toInt)
    }}
    logger.info("RDD created successfully")
    new SparkImage(rdd, image.width, image.height)
  }

  def applyFilter(image: Image,
                  filter: Filter[Iterable[(Int, Int, Int, Int)],
                          List[List[Int]]],
                  sc: SparkContext,
                  r: Int, path: String): Image = {
    val sparkImage = SparkImage(image, sc, path)
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
