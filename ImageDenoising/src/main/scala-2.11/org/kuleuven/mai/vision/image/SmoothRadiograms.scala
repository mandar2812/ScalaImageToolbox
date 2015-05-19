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
    val start = Calendar.getInstance().getTime()
    println("Reading in image at "+start)
    val image = TiffReader.read(FileUtils.openInputStream(new File(args(0))))
    val new_image = ImageMatrix.applyFilter(image, new MedianFilter(),
      args(2).toInt, args(0))
    val end = Calendar.getInstance().getTime()
    println("writing smoothed image at "+end)
    new_image.write(args(1), PNG)
  }
}
