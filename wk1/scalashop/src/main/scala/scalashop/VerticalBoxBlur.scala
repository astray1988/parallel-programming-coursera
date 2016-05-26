package scalashop

import org.scalameter._
import common._

object VerticalBoxBlurRunner {

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 5,
    Key.exec.maxWarmupRuns -> 10,
    Key.exec.benchRuns -> 10,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val radius = 3
    val width = 1920
    val height = 1080
    val src = new Img(width, height)
    val dst = new Img(width, height)
    val seqtime = standardConfig measure {
      VerticalBoxBlur.blur(src, dst, 0, width, radius)
    }
    println(s"sequential blur time: $seqtime ms")

    val numTasks = 32
    val partime = standardConfig measure {
      VerticalBoxBlur.parBlur(src, dst, numTasks, radius)
    }
    println(s"fork/join blur time: $partime ms")
    println(s"speedup: ${seqtime / partime}")
  }

}

/** A simple, trivially parallelizable computation. */
object VerticalBoxBlur {

  /** Blurs the columns of the source image `src` into the destination image
   *  `dst`, starting with `from` and ending with `end` (non-inclusive).
   *
   *  Within each column, `blur` traverses the pixels by going from top to
   *  bottom.
   */
  def blur(src: Img, dst: Img, from: Int, end: Int, radius: Int): Unit = {
    // TODO implement this method using the `boxBlurKernel` method
    val clampFrom = clamp(from, 0, dst.width)
    val clampEnd = clamp(end, 0, dst.width)
    for (
      y <- 0 until dst.height;
      x <- clampFrom until clampEnd
    ) {
//      println( "(" + x + "," + y + ")" )
      dst.update(x, y, boxBlurKernel(src, x, y, radius))
    }
  }

  /** Blurs the columns of the source image in parallel using `numTasks` tasks.
   *
   *  Parallelization is done by stripping the source image `src` into
   *  `numTasks` separate strips, where each strip is composed of some number of
   *  columns.
   */
  def parBlur(src: Img, dst: Img, numTasks: Int, radius: Int): Unit =
  {
    // TODO implement using the `task` construct and the `blur` method
    if(numTasks <= 0 ) {
      blur(src, dst, 0, src.width, radius)
    }
    val clampNumTasks = clamp(numTasks, 1, src.width) // numTasks <= width
    val colStep = dst.width / clampNumTasks
    if (dst.width % clampNumTasks == 0) { // even strips
      val range = 0 to dst.width by colStep
      val strips =  range.zip(range.tail) // e.g. Vector((0,2), (2,4))
      // each strip of task calling blur
      for(strip <- strips) {
        task(blur(src, dst, strip._1, strip._2, radius)).join()
      }
    } else {    // odd strips
      val extra = dst.width % clampNumTasks
      val c = (0, extra + colStep)
      val range = (extra + colStep) to dst.width by colStep
      val strips = range.zip(range.tail)
      val extStrips = c +: strips
      for(strip <- extStrips) {
        task(blur(src, dst, strip._1, strip._2, radius)).join()
      }

    }



  }

}
