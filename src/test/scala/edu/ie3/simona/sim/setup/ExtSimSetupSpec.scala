/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.sim.setup

import edu.ie3.simona.api.data.primarydata.ExtPrimaryDataConnection
import edu.ie3.simona.exceptions.ServiceException
import edu.ie3.simona.test.common.UnitSpec

import java.util.UUID
import scala.jdk.CollectionConverters.MapHasAsJava
import scala.util.Try

class ExtSimSetupSpec extends UnitSpec {

  "An ExtSimSetup" should {
    val uuid1 = UUID.fromString("726c40e1-b1cd-4f16-a5b6-3972e852f60b")
    val uuid2 = UUID.fromString("614fa950-53fa-4f5e-8ea1-b51234c4866c")
    val uuid3 = UUID.fromString("7a9cd186-ad23-47b2-912e-1a2c777f46b0")
    val uuid4 = UUID.fromString("044f9398-58f6-44fa-94de-039e0a6856fb")
    val uuid5 = UUID.fromString("ebcefed4-a3e6-4a2a-b4a5-74226d548546")
    val uuid6 = UUID.fromString("4a9c8e14-c0ee-425b-af40-9552b9075414")

    def toMap(uuids: Set[UUID]): Map[String, UUID] = uuids
      .map(uuid => uuid.toString -> uuid)
      .toMap

    "validate primary data connections without duplicates correctly" in {
      val extPrimaryDataConnection: Set[ExtPrimaryDataConnection] = Set(
        new ExtPrimaryDataConnection(toMap(Set(uuid1, uuid2)).asJava),
        new ExtPrimaryDataConnection(toMap(Set(uuid3, uuid4)).asJava),
        new ExtPrimaryDataConnection(toMap(Set(uuid5, uuid6)).asJava),
      )

      Try(
        ExtSimSetup.validatePrimaryData(extPrimaryDataConnection)
      ).isSuccess shouldBe true
    }

    "throw exception while validate primary data connections if duplicates are found" in {
      val extPrimaryDataConnection: Set[ExtPrimaryDataConnection] = Set(
        new ExtPrimaryDataConnection(toMap(Set(uuid1, uuid2)).asJava),
        new ExtPrimaryDataConnection(toMap(Set(uuid3, uuid4)).asJava),
        new ExtPrimaryDataConnection(toMap(Set(uuid4, uuid5, uuid6)).asJava),
        new ExtPrimaryDataConnection(toMap(Set(uuid6)).asJava),
      )

      intercept[ServiceException](
        ExtSimSetup.validatePrimaryData(extPrimaryDataConnection)
      ).getMessage shouldBe s"Multiple data connections provide primary data for assets: $uuid6,$uuid4"
    }

  }
}
