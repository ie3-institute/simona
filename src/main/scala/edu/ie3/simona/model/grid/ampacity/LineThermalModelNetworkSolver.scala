/*
 * © 2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.grid.ampacity

import org.apache.commons.math3.linear.{EigenDecomposition, RealMatrix}

object LineThermalModelNetworkSolver {

  def determineEigenvaluesAndVectors(
      matrix: RealMatrix
  ): (Array[Double], RealMatrix) = {

    val eigResult = new EigenDecomposition(matrix)
    val n = matrix.getColumnDimension
    val eigenvalues = Array.tabulate(n)(i => eigResult.getRealEigenvalue(i))
    val eigenvectors = eigResult.getV // RealMatrix

    val tolerance = 1e-10

    // sanity check
    if eigenvalues.length != matrix.getColumnDimension ||
      eigenvectors.getRowDimension != eigenvalues.length ||
      eigenvectors.getColumnDimension != eigenvalues.length
    then
      throw new IllegalStateException(
        s"Unexpected number of Eigenvalues or Eigenvectors. Expected are 5 each, Got: Eigenvalues: ${eigenvalues
            .mkString(",")}, Eigenvectors: ${eigenvectors.toString}."
      )
    if eigenvalues.exists(_ > tolerance) then {
      throw new IllegalStateException(
        s"Unstable RC network: positive eigenvalues detected: ${eigenvalues.mkString(",")}"
      )
    }

    (eigenvalues, eigenvectors)
  }
}
