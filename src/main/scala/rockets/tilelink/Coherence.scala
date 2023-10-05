package rockets.tilelink

trait CoherencePolicy
    extends HasCustomTileLinkMessageTypes
    with HasClientCoh
    with HasManagerCoh

trait HasCustomTileLinkMessageTypes {
  val nAcquireTypes: Int
  val nProbeTypes: Int
  val nReleaseTypes: Int
  val nGrantTypes: Int
}

trait HasClientCoh {
  val nClientStates: Int
}

trait HasManagerCoh extends HasDirectoryRepresentation {
  val nManagerStates: Int
}

case class MESICoherence(dir: DirectoryRepresentation) extends CoherencePolicy {
  override val nClientStates: Int = 4
  override val nManagerStates: Int = 0
  override val nAcquireTypes: Int = 2
  override val nProbeTypes: Int = 3
  override val nReleaseTypes: Int = 6
  override val nGrantTypes: Int = 3
}