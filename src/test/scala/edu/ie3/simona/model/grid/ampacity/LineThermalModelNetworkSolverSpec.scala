/*
 * © 2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.grid.ampacity

import breeze.linalg.{DenseMatrix, norm}
import edu.ie3.simona.test.common.UnitSpec

class LineThermalModelNetworkSolverSpec extends UnitSpec {

  private val tolerance = 1e-10

  "A LineThermalModelNetworkSolver" should {

    "determine eigenvalues and eigenvectors for a diagonal matrix" in {
      val cases = Table(
        ("matrix", "expectedVal", "expectedVec"),
        (
          DenseMatrix((-2.0, 0.0), (0.0, -5.0)),
          Array(-2.0, -5.0),
          DenseMatrix((1.0, 0.0), (0.0, 1.0)),
        ),
      )

      forAll(cases) {
        (
            matrix: DenseMatrix[Double],
            expectedVal: Array[Double],
            expectedVec: DenseMatrix[Double],
        ) =>

          val (eigenvalues, eigenvectors) =
            LineThermalModelNetworkSolver.determineEigenvaluesAndVectors(matrix)

          eigenvalues.toArray.sorted shouldBe expectedVal.sorted
          eigenvectors shouldBe expectedVec
      }
    }

    "return eigenpairs that satisfy A * v = lambda * v" in {
      val matrix = DenseMatrix((-4.0, -1.0), (-2.0, -3.0))

      val (eigenvalues, eigenvectors) =
        LineThermalModelNetworkSolver.determineEigenvaluesAndVectors(matrix)

      (0 until eigenvalues.length).foreach { idx =>
        val lambda = eigenvalues(idx)
        val eigenvector = eigenvectors(::, idx)

        val left = matrix * eigenvector
        val right = eigenvector * lambda

        norm(left - right) should be <= tolerance
      }
    }

    "throw IllegalStateException for unstable (positive-eigenvalue) networks" in {
      val unstable = DenseMatrix((2.0, 0.0), (0.0, 5.0))

      val ex = intercept[IllegalStateException] {
        LineThermalModelNetworkSolver.determineEigenvaluesAndVectors(unstable)
      }
      ex.getMessage should include("Unstable RC network")
    }
  }
}
