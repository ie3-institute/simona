package edu.ie3.simona.model.em
import edu.ie3.datamodel.models.input.AssetInput
import edu.ie3.datamodel.models.input.system.{LoadInput, PvInput, StorageInput, WecInput}
import edu.ie3.simona.ontology.messages.flex.MinMaxFlexibilityMessage
import edu.ie3.simona.ontology.messages.flex.MinMaxFlexibilityMessage.ProvideMinMaxFlexOptions
import edu.ie3.util.scala.quantities.DefaultQuantities.zeroKW
import squants.Power

final case class EmAggregateOpt()
  extends EmAggregateFlex {


  /** Aggregates flex options of connected devices to one flex options object
   * that describes the flexibility of this EmAgent
   *
   * @param flexOptions
   * the flex options of all connected agents
   * @return
   * aggregated reference, minimum and maximum power
   */
  override def aggregateFlexOptions(
                                     flexOptions: Iterable[
                                       (_ <: AssetInput, ProvideMinMaxFlexOptions)
                                     ]): (Power, Power, Power) = {
    val (minSum, maxSum) =
      flexOptions.foldLeft((zeroKW, zeroKW)) {
        case (
          (sumMin, sumMax),
          (_, ProvideMinMaxFlexOptions(_, _, addMin, addMax)),
          ) =>
          (
            sumMin + addMin,
            sumMax + addMax,
          )
      }

    val maxRefSum =
      flexOptions.foldLeft(zeroKW) {
        case (
          maxSumExclReg,
          (inputModel, ProvideMinMaxFlexOptions(_, _, addMin, addMax)),
          ) =>
          inputModel match {
            case _: PvInput | _: WecInput =>
              maxSumExclReg + addMin
            case _ => maxSumExclReg + addMax
          }
      }


    // pRef

    val pRefLoads = flexOptions.foldLeft(zeroKW) {
      case (
        pRefLoad,
        (inputModel, ProvideMinMaxFlexOptions(_, addRef, addMin, addMax)),
        ) =>
        inputModel match {
          case _: LoadInput =>
            pRefLoad + addRef
          case _ => pRefLoad
        }
    }

    val (pMinPvs, pMaxPvs) = flexOptions.foldLeft((zeroKW, zeroKW)) {
      case (
        (pvMin, pvMax),
        (inputModel, ProvideMinMaxFlexOptions(_, _, addMin, addMax)),
        ) =>
        inputModel match {
          case _: PvInput =>
            (pvMin + addMin, pvMax + addMax)
          case _ => (pvMin, pvMax)
        }
    }

    val (pMinStorages, pMaxStorages) = flexOptions.foldLeft((zeroKW, zeroKW)) {
      case (
        (storageMin, storageMax),
        (inputModel, ProvideMinMaxFlexOptions(_, _, addMin, addMax)),
        ) =>
        inputModel match {
          case _: StorageInput =>
            (storageMin + addMin, storageMax + addMax)
          case _ => (storageMin, storageMax)
        }
    }

    // p_S = -p_PV^min - p_L

    var pStorOpt = zeroKW
    var pPvOpt = zeroKW
    if (-pMinPvs-pRefLoads >= pMinStorages & -pMinPvs-pRefLoads <= pMaxStorages) {
      pStorOpt = -pMinPvs-pRefLoads
      pPvOpt = pMinPvs
    } else {
      if (-pMaxStorages - pRefLoads >= pMinPvs & -pMaxStorages - pRefLoads <= pMaxPvs) {
        pStorOpt = pMaxStorages
        pPvOpt = -pMaxStorages - pRefLoads
      } else {
        pStorOpt = pMinStorages
        pPvOpt = pMinPvs
        var minValue = (pStorOpt+pPvOpt+pRefLoads).value*(pStorOpt+pPvOpt+pRefLoads).value
        var pStorCand = pMinStorages
        var pPvCand = pMaxPvs
        if ((pStorCand+pPvCand+pRefLoads).value*(pStorCand+pPvCand+pRefLoads).value <= minValue) {
          minValue = (pStorCand+pPvCand+pRefLoads).value*(pStorCand+pPvCand+pRefLoads).value
          pStorOpt = pStorCand
          pPvOpt = pPvCand
        } else {
          pStorCand = pMaxStorages
          pPvCand = pMinPvs
          if ((pStorCand+pPvCand+pRefLoads).value*(pStorCand+pPvCand+pRefLoads).value <= minValue) {
            minValue = (pStorCand+pPvCand+pRefLoads).value*(pStorCand+pPvCand+pRefLoads).value
            pStorOpt = pStorCand
            pPvOpt = pPvCand
          } else {
            pStorCand = pMaxStorages
            pPvCand = pMaxPvs
            if ((pStorCand+pPvCand+pRefLoads).value*(pStorCand+pPvCand+pRefLoads).value <= minValue) {
              pStorOpt = pStorCand
              pPvOpt = pPvCand
            }
          }
        }
      }
    }

    // take the closest power possible to zero
    val refAgg = pStorOpt + pPvOpt + pRefLoads

    (refAgg, minSum, maxSum)
  }
}
