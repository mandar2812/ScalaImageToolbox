package org.kuleuven.mai.vision.image

import com.sksamuel.scrimage._
import org.apache.log4j.Logger
import org.apache.spark.rdd.RDD
import org.kuleuven.mai.vision.filters.Filter

import scala.collection.immutable.HashMap
import scala.collection.mutable
import scala.concurrent.Future

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

  def width = _width

  def height = _height

  def image = _image

  def neighbourhood(radius: Int)(x: Int, y: Int): Iterable[(Int, Int, Int, Int)] = {
    //println("Calculating neighborhood for pixel: "+x+", "+y)
    for(
      i <- math.max(0.0, x - radius).toInt to math.min(x + radius, this.width - 1);
      j <- math.max(0.0, y - radius).toInt to math.min(y + radius, this.height - 1)
    ) yield {
        val pixel = image get (i,j)
        (PixelTools.alpha(pixel.get),
        PixelTools.red(pixel.get),
        PixelTools.green(pixel.get),
        PixelTools.blue(pixel.get))
      }
  }

}

object ImageMatrix {
  val logger = Logger.getLogger(this.getClass)
  private[vision] def apply(image: Image): ImageMatrix = {
    println("Creating HashMap for image")
    val rdd = for(i <- 0 until image.width; j <- 0 until image.height)
      yield ((i,j), image.pixel(i,j))
    new ImageMatrix(HashMap(rdd:_*), image.width, image.height)
  }

  def applyFilterhisto(image : Image, r: Int): Image ={
    val im: List[mutable.MutableList[Array[Int]]] = List.tabulate(image.width) { i =>
      mutable.MutableList.tabulate(image.height) {j =>
        image.argb(i, j)
      }}
    val imagechannels = (0 to 3).map{i => new RadioGraph(im.map{_.map{_(i)}},r)}.map(_.denoisingfast3)

    val newimage = image.resizeTo(im.length - (2*r + 1),
      im.head.length - (2*r + 1))
      .map{(x,y,pixel) => PixelTools.argb(imagechannels.head(y)(x),
      imagechannels(1)(y)(x),
      imagechannels(2)(y)(x),
      imagechannels(3)(y)(x))}

    newimage
  }

  def applyFilter(image: Image,
                  filter: Filter[Iterable[(Int, Int, Int, Int)],
                    List[List[Int]]],
                  r: Int): Image = {
    val neighborhoodFunction = ImageMatrix.neighbourhood(image)(r) _
    var flag = false
    var prev = 0.0
    image.map((x,y,pixel) => {
      val n = neighborhoodFunction(x,y)
      val percent = 100.0*x.toFloat/image.width
      val vals_argb = filter.evaluate(n)
      if(math.floor(percent) % 5 < 1 && !flag && percent - prev > 1.0) {
        val progress = if(percent.toInt < 10) "0"+percent.toInt else percent.toInt
        if(percent.toInt % 10 == 0){
          println("Progress : "+progress+"%\t"+"="*(percent/10).toInt)
        } else {
          println("Progress : "+progress+"%")
        }
        prev = percent
        flag = true
      } else {
        flag = false
      }
      PixelTools.argb(vals_argb(0),
        vals_argb(1),
        vals_argb(2),
        vals_argb(3))
    })
  }

  def subsample(image: Image, scale: Double = 0.5): Image =
    image.fit((image.width*scale).toInt,
    (image.height*scale).toInt,
    color = Color.Black,
    scaleMethod = ScaleMethod.Bicubic)

  def neighbourhood(image: Image)(radius: Int)(x: Int, y: Int): Iterable[(Int, Int, Int, Int)] = {
    //println("Calculating neighborhood for pixel: "+x+", "+y)
    val (x1, y1) = (math.max(0.0, x - radius).toInt, math.max(0.0, y - radius).toInt)
    val (x2, y2) = (math.min(x + radius, image.width - 1), math.min(y + radius, image.height - 1))
    image.pixels(x1,y1,x2-x1,y2-y1).map(pixel =>
      (PixelTools.alpha(pixel),
      PixelTools.red(pixel),
      PixelTools.green(pixel),
      PixelTools.blue(pixel)))
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
