package org.kuleuven.mai.vision.image

import java.awt.image.BufferedImage
import java.io.{FileOutputStream, PrintWriter, File}
import javax.imageio.ImageIO
import breeze.linalg.DenseVector
import com.github.fommil.netlib.BLAS
import com.github.fommil.netlib.ARPACK
import com.github.fommil.netlib.LAPACK
import com.sksamuel.scrimage.Image
import com.sksamuel.scrimage.io.TiffReader
import org.apache.commons.io.FileUtils
import org.apache.sanselan.formats.tiff.write.TiffImageWriterBase

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


    val obj = new RadioGraph(im,3)
    obj.denoising





// possible command  for writing a file
   /* var imge :BufferedImage = new BufferedImage(
      3023,1500 , BufferedImage.TYPE_INT_RGB )
     imge  = List.tabulate(imge.getWidth) { i =>
      mutable.MutableList.tabulate(imge.getHeight) { j =>
        imge.....(j,i) = obj.denoising(i)(j)
      }
    }
   val  outputfile : File = new File("data/Radiographs/image.tif")
        ImageIO.write(imge ,"tif", outputfile)

 */
  }
}
