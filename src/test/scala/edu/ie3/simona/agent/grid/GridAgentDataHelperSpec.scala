/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid

import java.util.UUID
import edu.ie3.datamodel.graph.SubGridGate
import edu.ie3.simona.agent.grid.GridAgentDataHelperSpec.TestGridData
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.simona.test.common.model.grid.SubGridGateMokka

object GridAgentDataHelperSpec {

  final case class TestGridData(
      subgridId: Int,
      subgridGates: Vector[SubGridGate],
  ) extends GridAgentDataHelper {

    def getSuperiorGridGates: Vector[SubGridGate] =
      super.superiorGridGates

    def getSuperiorGridIds: Vector[String] =
      super.superiorGridIds

    def getInferiorGridIds: Vector[String] =
      super.inferiorGridIds

    def getSuperiorGridNodeUuids: Vector[UUID] =
      super.superiorGridNodeUuids

    def getInferiorGridNodeUuids: Vector[UUID] =
      super.inferiorGridNodeUuids

    def getInferiorGridGates: Vector[SubGridGate] =
      super.inferiorGridGates
  }
}

class GridAgentDataHelperSpec extends UnitSpec with SubGridGateMokka {
  val superiorSubGridGate1: SubGridGate = build2wSubGridGate(
    UUID.fromString("107a3c34-d890-4f92-95e6-beb10975d9d3"),
    1,
    UUID.fromString("0292fbe9-63b7-4580-9920-2c7c24032abc"),
    100,
  )
  val superiorSubGridGate2: SubGridGate = build2wSubGridGate(
    UUID.fromString("97e957fb-b1c0-4ae0-bfe0-329ed85616b4"),
    2,
    UUID.fromString("f05e40a4-8d48-44c3-bbe9-bebf10022fc0"),
    100,
  )
  val centerSubGridGate1: SubGridGate = build2wSubGridGate(
    UUID.fromString("37ff2b4d-de5c-4036-b0b2-f343f839182c"),
    100,
    UUID.fromString("960deb7c-cf50-45d8-8563-5fdd713d90f2"),
    1000,
  )
  val centerSubGridGate2: SubGridGate = build2wSubGridGate(
    UUID.fromString("73a2ec29-9054-4e8d-8f0d-a945434c89d8"),
    100,
    UUID.fromString("b17a0ec9-9567-42c8-990f-186888c6eb37"),
    2000,
  )
  val centerSubGridGate3: SubGridGate = build2wSubGridGate(
    UUID.fromString("44065395-07bf-48ef-9df4-9e82c705946d"),
    100,
    UUID.fromString("3bcda4b0-2d1a-44f5-95c1-a63ce1d40bed"),
    3000,
  )
  val superiorGridGates: Vector[SubGridGate] = Vector(superiorSubGridGate1)
  val centerGridGates: Vector[SubGridGate] = Vector(
    superiorSubGridGate1,
    superiorSubGridGate2,
    centerSubGridGate1,
    centerSubGridGate2,
    centerSubGridGate3,
  )
  val inferiorGridGates: Vector[SubGridGate] = Vector(centerSubGridGate1)

  val superiorGridId = 1
  val superiorGridAgent: TestGridData =
    TestGridData(superiorGridId, superiorGridGates)

  val centerGridId = 100
  val centerGridAgent: TestGridData =
    TestGridData(centerGridId, centerGridGates)

  val inferiorGridId = 1000
  val inferiorGridAgent: TestGridData =
    TestGridData(inferiorGridId, inferiorGridGates)

  "GridAgentDataHelper" should {

    "provide correct superiorGridNodeUuids for all agents on all hierarchy levels" in {

      superiorGridAgent.getSuperiorGridNodeUuids shouldBe Vector.empty[String]
      centerGridAgent.getSuperiorGridNodeUuids shouldBe Vector(
        UUID.fromString("107a3c34-d890-4f92-95e6-beb10975d9d3"),
        UUID.fromString("97e957fb-b1c0-4ae0-bfe0-329ed85616b4"),
      )
      inferiorGridAgent.getSuperiorGridNodeUuids shouldBe Vector(
        UUID.fromString("37ff2b4d-de5c-4036-b0b2-f343f839182c")
      )
    }

    "provide correct inferiorGridGates for all agents on all hierarchy levels" in {

      superiorGridAgent.getInferiorGridGates shouldBe Vector(
        superiorSubGridGate1
      )
      centerGridAgent.getInferiorGridGates shouldBe Vector(
        centerSubGridGate1,
        centerSubGridGate2,
        centerSubGridGate3,
      )
      inferiorGridAgent.getInferiorGridGates shouldBe Vector.empty[SubGridGate]

    }

    "identify itself as superior or not" in {

      superiorGridAgent.isSuperior shouldBe true
      centerGridAgent.isSuperior shouldBe false
      inferiorGridAgent.isSuperior shouldBe false

    }

    "provide correct superiorGridGates for all agents on all hierarchy levels" in {

      superiorGridAgent.getSuperiorGridGates shouldBe Vector.empty[SubGridGate]
      centerGridAgent.getSuperiorGridGates shouldBe Vector(
        superiorSubGridGate1,
        superiorSubGridGate2,
      )
      inferiorGridAgent.getSuperiorGridGates shouldBe Vector(centerSubGridGate1)
    }

    "provide correct superiorGridIds for all agents on all hierarchy levels" in {
      superiorGridAgent.getSuperiorGridIds shouldBe Vector.empty[String]
      centerGridAgent.getSuperiorGridIds shouldBe Vector("1", "2")
      inferiorGridAgent.getSuperiorGridIds shouldBe Vector("100")
    }

    "provide correct inferiorGridNodeUuids for all agents on all hierarchy levels" in {
      superiorGridAgent.getInferiorGridNodeUuids shouldBe Vector(
        UUID.fromString("0292fbe9-63b7-4580-9920-2c7c24032abc")
      )
      centerGridAgent.getInferiorGridNodeUuids shouldBe Vector(
        UUID.fromString("960deb7c-cf50-45d8-8563-5fdd713d90f2"),
        UUID.fromString("b17a0ec9-9567-42c8-990f-186888c6eb37"),
        UUID.fromString("3bcda4b0-2d1a-44f5-95c1-a63ce1d40bed"),
      )
      inferiorGridAgent.getInferiorGridNodeUuids shouldBe Vector.empty[String]
    }

    "provide correct inferiorGridIds for all agents on all hierarchy levels" in {
      superiorGridAgent.getInferiorGridIds shouldBe Vector("100")
      centerGridAgent.getInferiorGridIds shouldBe Vector("1000", "2000", "3000")
      inferiorGridAgent.getInferiorGridIds shouldBe Vector.empty[String]
    }
  }
}
