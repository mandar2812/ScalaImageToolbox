package org.kuleuven.mai.vision.image

import java.io.File
import com.sksamuel.scrimage.Format.PNG
import com.sksamuel.scrimage.filter.GaussianBlurFilter
import com.sksamuel.scrimage.io.TiffReader
import com.twelvemonkeys.imageio.metadata.exif.TIFF
import org.apache.commons.io.FileUtils

import scala.collection.mutable


/**
 * @author mandar & romain
 */
object SmoothImages {
  def main(args: Array[String]): Unit = {

    val dataRoot = "data/Radiographs/01.tif"

    val image = TiffReader.read(FileUtils.openInputStream(new File(dataRoot)))

    println(s"Width: ${image.width} Height: ${image.height} Ratio: ${image.ratio}")

    val im: List[mutable.MutableList[Int]] = List.tabulate(image.width) { i =>
      mutable.MutableList.tabulate(image.height) {j =>
        image.pixel(i, j)
      }
    }

    val radius = args(1).toInt
    val obj = new RadioGraph(im,radius)
    val newimg = obj.denoising

    val newimage = image.resizeTo(im.length - (2*radius + 1),
      im.head.length - (2*radius + 1))
      .map{(x,y,_) => newimg(y)(x)}

    newimage.write(args(0), PNG)

    val filteredimage = image.filter(new GaussianBlurFilter(radius))
    filteredimage.write(args(2), PNG)

  }
}
