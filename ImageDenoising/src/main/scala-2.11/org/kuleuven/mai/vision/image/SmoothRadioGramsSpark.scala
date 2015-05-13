package org.kuleuven.mai.vision.image

import java.io.File

import com.sksamuel.scrimage.Format.PNG
import com.sksamuel.scrimage.io.TiffReader
import org.apache.commons.io.FileUtils
import org.apache.spark.{SparkContext, SparkConf}
import org.kuleuven.mai.vision.filters.MedianFilter

/**
 * @author mandar2812
 */
object SmoothRadioGramsSpark {
  def main(args: Array[String]): Unit = {
    val conf = new SparkConf()
      .setAppName("SmoothRadiograms")
      .setMaster("local[4]")
      .setExecutorEnv("spark.executor.memory", "1g")
      .set("spark.storage.memoryFraction", "0.4")
    val sc = new SparkContext(conf)
    val image = TiffReader.read(FileUtils.openInputStream(new File(args(0))))
    val new_image = SparkImage.applyFilter(image,
      new MedianFilter(),
      sc, 5, args(0))
    new_image.write(args(1), PNG)
  }
}
