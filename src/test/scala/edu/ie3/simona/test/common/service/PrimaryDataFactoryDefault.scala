package edu.ie3.simona.test.common.service

import edu.ie3.datamodel.models.value.Value
import edu.ie3.simona.api.data.primarydata.PrimaryDataFactory

class PrimaryDataFactoryDefault extends PrimaryDataFactory {

  /** Should convert an object to an primary data value with a check if the object is primary data */
  override def convertObjectToValue(entity: Any): Value = ???
}
