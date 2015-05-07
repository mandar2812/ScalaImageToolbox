package org.kuleuven.mai.vision.image

import java.io.File

import com.sksamuel.scrimage.Image
import com.sksamuel.scrimage.io.TiffReader
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
      mutable.MutableList.tabulate(image.height) { j =>
        val pix = image.pixel(i, j)
        pix
      }
    }
    val obj = new RadioGraph(im)
    println(obj.kernel(2))
  }

}

