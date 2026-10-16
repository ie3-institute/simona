/*
 * © 2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.grid.ampacity

import org.apache.commons.math3.linear.Array2DRowRealMatrix
import edu.ie3.simona.test.common.UnitSpec

class LineThermalModelNetworkSolverSpec extends UnitSpec {

  private val tolerance = 1e-10

  "A LineThermalModelNetworkSolver" should {

    "determine eigenvalues and eigenvectors for a diagonal matrix" in {
      val matrix = new Array2DRowRealMatrix(
        Array(Array(-2.0, 0.0), Array(0.0, -5.0))
      )

      val (eigenvalues, eigenvectors) =
        LineThermalModelNetworkSolver.determineEigenvaluesAndVectors(matrix)

      eigenvalues.sorted shouldBe Array(-5.0, -2.0)
      eigenvectors.getRowDimension shouldBe 2
      eigenvectors.getColumnDimension shouldBe 2
      (0 until eigenvalues.length).foreach { idx =>
        val eigenvector = eigenvectors.getColumnVector(idx)
        matrix
          .operate(eigenvector)
          .subtract(eigenvector.mapMultiply(eigenvalues(idx)))
          .getNorm should be <= tolerance
      }
    }

    "return eigenpairs that satisfy A * v = lambda * v" in {
      val matrix = new Array2DRowRealMatrix(
        Array(Array(-4.0, -1.0), Array(-2.0, -3.0))
      )

      val (eigenvalues, eigenvectors) =
        LineThermalModelNetworkSolver.determineEigenvaluesAndVectors(matrix)

      (0 until eigenvalues.length).foreach { idx =>
        val eigenvector = eigenvectors.getColumnVector(idx)
        val left = matrix.operate(eigenvector)
        val right = eigenvector.mapMultiply(eigenvalues(idx))

        (left.subtract(right)).getNorm should be <= tolerance
      }
    }

    "throw IllegalStateException for unstable (positive-eigenvalue) networks" in {
      val unstable = new Array2DRowRealMatrix(
        Array(Array(2.0, 0.0), Array(0.0, 5.0))
      )

      val ex = intercept[IllegalStateException] {
        LineThermalModelNetworkSolver.determineEigenvaluesAndVectors(unstable)
      }
      ex.getMessage should include("Unstable RC network")
    }
  }
}
