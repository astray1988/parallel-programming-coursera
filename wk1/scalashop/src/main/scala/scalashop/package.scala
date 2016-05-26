
import java.util

import common._

import scala.collection.mutable.ArrayBuffer

package object scalashop {

  /** The value of every pixel is represented as a 32 bit integer. */
  type RGBA = Int

  /** Returns the red component. */
  def red(c: RGBA): Int = (0xff000000 & c) >>> 24

  /** Returns the green component. */
  def green(c: RGBA): Int = (0x00ff0000 & c) >>> 16

  /** Returns the blue component. */
  def blue(c: RGBA): Int = (0x0000ff00 & c) >>> 8

  /** Returns the alpha component. */
  def alpha(c: RGBA): Int = (0x000000ff & c) >>> 0

  /** Used to create an RGBA value from separate components. */
  def rgba(r: Int, g: Int, b: Int, a: Int): RGBA = {
    (r << 24) | (g << 16) | (b << 8) | (a << 0)
  }

  /** Restricts the integer into the specified range. */
  def clamp(v: Int, min: Int, max: Int): Int = {
    if (v < min) min
    else if (v > max) max
    else v
  }

  /** Image is a two-dimensional matrix of pixel values. */
  class Img(val width: Int, val height: Int, private val data: Array[RGBA]) {
    def this(w: Int, h: Int) = this(w, h, new Array(w * h))
    def apply(x: Int, y: Int): RGBA = data(y * width + x)
    def update(x: Int, y: Int, c: RGBA): Unit = data(y * width + x) = c
  }

  /** Computes the blurred RGBA value of a single pixel of the input image. */
  def boxBlurKernel(src: Img, x: Int, y: Int, radius: Int): RGBA = {
    // TODO implement using while loops
    if(radius == 0) return src(x,y)

    val sumRed = new collection.mutable.ListBuffer[RGBA]
    val sumGreen = new collection.mutable.ListBuffer[RGBA]
    val sumBlue = new collection.mutable.ListBuffer[RGBA]
    val sumAlpha = new collection.mutable.ListBuffer[RGBA]

    val xLower = 0
    val xUpper = src.width - 1
    val yLower = 0
    val yUpper = src.height - 1

    val xBlurStart = clamp(x - radius, xLower, xUpper)
    val xBlurEnd = clamp(x + radius, xLower, xUpper)

    val yBlurStart = clamp(y - radius, yLower, yUpper)
    val yBlurEnd = clamp(y + radius, yLower, yUpper)

    for(xPos <- xBlurStart to xBlurEnd; yPos <- yBlurStart to yBlurEnd) {
      sumRed += red(src(xPos, yPos))
      sumGreen += green(src(xPos, yPos))
      sumBlue += blue(src(xPos, yPos))
      sumAlpha += alpha(src(xPos, yPos))
    }

    rgba(average(sumRed), average(sumGreen), average(sumBlue), average(sumAlpha))
  }

  def average[T](ts: Iterable[T])(implicit num: Numeric[T]) = {
    num.toInt(ts.sum) / ts.size
  }


}
