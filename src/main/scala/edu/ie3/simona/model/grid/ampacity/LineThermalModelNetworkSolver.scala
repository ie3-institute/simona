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

    // sanity check
    if eigenvalues.length != matrix.cols ||
      eigenvectors.rows != eigenvalues.length ||
      eigenvectors.cols != eigenvalues.length
    then
      throw new IllegalStateException(
        s"Unexpected number of Eigenvalues or Eigenvectors. Expected are 5 each, Got: Eigenvalues: $eigenvalues, Eigenvectors: $eigenvectors."
      )
    if eigenvalues.exists(_ > tolerance) then {
      throw new IllegalStateException(
        s"Unstable RC network: positive eigenvalues detected: $eigenvalues"
      )
    }

    (eigenvalues, eigenvectors)
  }
}
