package rockets.tilelink
import rockets.params.config.Parameters
import spinal.core._
import spinal.lib._

trait CoherencePolicy
    extends HasCustomTileLinkMessageTypes
    with HasClientCoh
    with HasManagerCoh

trait HasCustomTileLinkMessageTypes {
  val AcquireEnum: SpinalEnum
  val ProbeEnum: SpinalEnum
  val ReleaseEnum: SpinalEnum
  val GrantEnum: SpinalEnum

  val nAcquireTypes: Int = AcquireEnum.elements.size
  val nProbeTypes: Int = ProbeEnum.elements.size
  val nReleaseTypes: Int = ReleaseEnum.elements.size
  val nGrantTypes: Int = GrantEnum.elements.size

  val releaseTypesWithData: Vec[UInt]
  val grantTypesWithData: Vec[UInt]
}

trait HasClientCoh {
  val clientEnum: SpinalEnum

  val nClientStates: Int = clientEnum.elements.size

  val clientStatesWithDirtyData: Vec[UInt]

  def requiresReleaseOnCacheControl(cmd: UInt, meta: ClientMetaData): Bool =
    clientStatesWithDirtyData.sExist(_ === meta.state)

  def getReleaseType(probe: Probe, meta: ClientMetaData): UInt

  def clientMetaDataOnProbe(probe: Probe, meta: ClientMetaData)(implicit
      p: Parameters
  ): ClientMetaData
}

trait HasManagerCoh extends HasDirectoryRepresentation {
  val nManagerStates: Int
}

case class MESICoherence(dir: DirectoryRepresentation) extends CoherencePolicy {
  object AcquireEnum extends SpinalEnum {
    val acquireShared, acquireExclusive = newElement()
  }

  object ProbeEnum extends SpinalEnum {
    val probeInvalidate, probeDowngrade, probeCopy = newElement()
  }

  object ReleaseEnum extends SpinalEnum {
    val releaseInvalidateData, releaseDowngradeData, releaseCopyData =
      newElement()

    val releaseInvalidateAck, releaseDowngradeAck, releaseCopyAck = newElement()
  }

  override val releaseTypesWithData: Vec[UInt] = Vec(
    ReleaseEnum.releaseInvalidateData.asUInt,
    ReleaseEnum.releaseDowngradeData.asUInt,
    ReleaseEnum.releaseCopyData.asUInt
  )

  object GrantEnum extends SpinalEnum {
    val grantShared, grantExclusive, grantExclusiveAck = newElement()
  }

  override val grantTypesWithData: Vec[UInt] = Vec(
    GrantEnum.grantShared.asUInt,
    GrantEnum.grantExclusive.asUInt
  )

  object clientEnum extends SpinalEnum {
    val clientInvalid, clientShared, clientExclusiveClean,
        clientExclusiveDirty = newElement()
  }

  override val clientStatesWithDirtyData: Vec[UInt] = Vec(
    clientEnum.clientExclusiveDirty.asUInt
  )

  override val nManagerStates: Int = 0

  override def clientMetaDataOnProbe(probe: Probe, meta: ClientMetaData)(
      implicit p: Parameters
  ): ClientMetaData =
    ClientMetaData(
      probe.typ.mux(
        ProbeEnum.probeInvalidate.asUInt -> clientEnum.clientInvalid.asUInt,
        ProbeEnum.probeDowngrade.asUInt -> clientEnum.clientShared.asUInt,
        ProbeEnum.probeCopy.asUInt -> meta.state,
        default -> meta.state
      )
    )

  override def getReleaseType(probe: Probe, meta: ClientMetaData): UInt = U(0)
}
