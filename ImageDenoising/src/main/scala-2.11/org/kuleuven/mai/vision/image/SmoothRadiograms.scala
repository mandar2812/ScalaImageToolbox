package org.kuleuven.mai.vision.image

import java.io.File

import com.sksamuel.scrimage.Format.PNG
import com.sksamuel.scrimage.io.TiffReader
import org.apache.commons.io.FileUtils
import org.apache.spark.{SparkContext, SparkConf}
import org.kuleuven.mai.vision.filters.MedianFilter

/**
 * @author mandar
 */
object SmoothRadiograms {
  def main(args: Array[String]): Unit = {
    val image = TiffReader.read(FileUtils.openInputStream(new File(args(0))))
    val new_image = ImageMatrix.applyFilter(image, new MedianFilter(),
      args(2).toInt, args(0))
    new_image.write(args(1), PNG)
  }
}
