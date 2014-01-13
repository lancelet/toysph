package toysph

import math._

trait SmoothingKernel {
  /** Radial support of the kernel for scale parameter h. */
  def support(h: Double): Double
  /** Value of the kernel for scale parameter h. */
  def kernel(h: Double): Double => Double
  /** First derivative of the kernel for scale parameter h. */
  def grad1(h: Double): Double => Double
  /** Second derivative of the kernel for scale parameter h. */
  def grad2(h: Double): Double => Double
}

object KernelQuintic extends SmoothingKernel {

  final val Sigma = 7.0 / 478.0 / Pi

  def support(h: Double): Double = 3.0f * h

  def kernel(h: Double): Double => Double = {
    (r: Double) => {
      val q = r / h
      val poly = if (q < 1) {
        pow(3.0 - q, 5) - 6.0 * pow(2.0 - q, 5) + 15.0 * pow(1.0 - q, 5)
      } else if (q < 2) {
        pow(3.0 - q, 5) - 6.0 * pow(2.0 - q, 5)
      } else if (q < 3) {
        pow(3.0 - q, 5)
      } else {
        0.0
      }
      Sigma * poly
    }
  }

  def grad1(h: Double): Double => Double = {
    (r: Double) => {
      val q = r / h
      val poly = if (q < 1) {
        -5.0 * pow(3.0 - q, 4) + 30.0 * pow(2.0 - q, 4) - 75.0 * pow(1.0 - q, 4)
      } else if (q < 2) {
        -5.0 * pow(3.0 - q, 4) + 30.0 * pow(2.0 - q, 4)
      } else if (q < 3) {
        -5.0 * pow(3.0 - q, 4)
      } else {
        0.0
      }
      Sigma * poly
    }
  }

  def grad2(h: Double): Double => Double = {
    (r: Double) => {
      val q = r / h
      val poly = if (q < 1) {
        20.0 * pow(3.0 - q, 3) - 120.0 * pow(2.0 - q, 3) + 300.0 * pow(1.0 - q, 3)
      } else if (q < 2) {
        20.0 * pow(3.0 - q, 3) - 120.0 * pow(2.0 - q, 3)
      } else if (q < 3) {
        20.0 * pow(3.0 - q, 3)
      } else {
        0.0
      }
      Sigma * poly
    }
  }

}