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
    // example: the kernel is at the top left corner ( so all values are =-789517) the first value of last column is deleted , and the last value is no the shifted value(2000,800 to see the change
    // but it doesnt make sense to shift that way. otherwize we wont see any chage in the data ...
    println(obj.kernelactualize(obj.kernel(0,2),2000,800))
  }

}

