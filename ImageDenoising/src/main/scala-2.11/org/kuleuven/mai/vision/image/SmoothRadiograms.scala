package org.kuleuven.mai.vision.image

import java.io.File
import java.util.Calendar

import com.sksamuel.scrimage.Format.PNG
import com.sksamuel.scrimage.io.TiffReader
import org.apache.commons.io.FileUtils
import org.kuleuven.mai.vision.filters.MedianFilter

/**
 * @author mandar
 */
object SmoothRadiograms {
  def main(args: Array[String]): Unit = {
    /*val start = Calendar.getInstance().getTime()
    println("Reading in image at "+start)
    val image = TiffReader.read(FileUtils.openInputStream(new File(args(0))))
    val new_image = ImageMatrix.applyFilter(image, new MedianFilter(),
      args(2).toInt, args(0))
    val end = Calendar.getInstance().getTime()
    println("writing smoothed image at "+end)
    new_image.write(args(1), PNG)*/

    val dataRoot = args(0)
    val destinationDir = args(1)
    val window = args(2).toInt
    val num_images = args(3).toInt
    val num_levels = args(4).toInt

    (1 to num_images).foreach{i =>
      //for every image, filter then subsample.
      val start = Calendar.getInstance().getTime()
      println("Reading in Radiograph "+i+" at "+start)
      val filename = if(i < 10) "0"+i+".tif" else i+".tif"
      var image = TiffReader.read(FileUtils.openInputStream(new File(dataRoot+filename)))
      (1 to num_levels).foreach{level =>
        println("Applying filter for level: "+level)
        val new_image = ImageMatrix.applyFilter(image, new MedianFilter(),
          window)
        val scaled_image = ImageMatrix.subsample(new_image)
        scaled_image.write(destinationDir+i+"_level_"+level+".png", PNG)
        image = scaled_image
        println("Saved scaled version of image "+i+" for level : "+level)
      }
      val end = Calendar.getInstance().getTime()
      println("Done writing smoothed images for radiogram: "+i+" at "+end)
    }
  }
}
