/*
 * © 2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.grid.ampacity

import breeze.linalg.{DenseMatrix, DenseVector, eig}

object LineThermalModelNetworkSolver {

  def determineEigenvaluesAndVectors(
      matrix: DenseMatrix[Double]
  ): (DenseVector[Double], DenseMatrix[Double]) = {

    val eigResult = eig(matrix)
    val eigenvalues = eigResult.eigenvalues
    val eigenvectors = eigResult.eigenvectors

    val tolerance = 1e-10

    if eigenvalues.exists(_ > tolerance) then {
      throw new IllegalStateException(
        s"Unstable RC network: positive eigenvalues detected: $eigenvalues"
      )
    }

    (eigenvalues, eigenvectors)
  }
}
