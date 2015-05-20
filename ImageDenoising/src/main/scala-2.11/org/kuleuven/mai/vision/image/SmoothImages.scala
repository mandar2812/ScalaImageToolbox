package org.kuleuven.mai.vision.image

import java.io.File
import com.sksamuel.scrimage.Format.PNG
import com.sksamuel.scrimage.PixelTools
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

    val dataRoot = "data/Radiographs/03.tif"
    val cat="C:\\Users\\Romain\\Desktop\\Lenna.tif"
    val image = TiffReader.read(FileUtils.openInputStream(new File(dataRoot)))

    println(s"Width: ${image.width} Height: ${image.height} Ratio: ${image.ratio}")

    val im: List[mutable.MutableList[Array[Int]]] = List.tabulate(image.width) { i =>
      mutable.MutableList.tabulate(image.height) {j =>
        image.argb(i, j)
      }
    }

    val radius = args(1).toInt

    val imagechannels = (0 to 3).map{i => new RadioGraph(im.map{_.map{_(i)}},radius)}.map(_.denoisingfast3)


    val newimage = image.resizeTo(im.length - (2*radius + 1),
      im.head.length - (2*radius + 1))
      .map{(x,y,pixel) => PixelTools.argb(imagechannels.head(y)(x),
      imagechannels(1)(y)(x),
      imagechannels(2)(y)(x),
      imagechannels(3)(y)(x))}

    newimage.write(args(0), PNG)


  }
}
