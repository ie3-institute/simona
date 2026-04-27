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
      val matrix = DenseMatrix((2.0, 0.0), (0.0, 5.0))

      val (eigenvalues, eigenvectors) =
        LineThermalModelNetworkSolver.determineEigenvaluesAndVectors(matrix)

      eigenvalues.length shouldBe 2
      eigenvectors.rows shouldBe 2
      eigenvectors.cols shouldBe 2

      eigenvalues.toArray.sorted shouldBe Array(2.0, 5.0)
    }

    "return eigenpairs that satisfy A * v = lambda * v" in {
      val matrix = DenseMatrix((4.0, 1.0), (2.0, 3.0))

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
  }
}
