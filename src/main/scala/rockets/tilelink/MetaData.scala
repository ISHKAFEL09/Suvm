package rockets.tilelink

import rockets.params.config.Parameters
import spinal.core._

trait HasCoherenceMetaData extends TLBundle {
  val co = tlCoh
}

case class ClientMetaData()(implicit p: Parameters) extends HasCoherenceMetaData {
  val state = UInt(log2Up(co.nClientStates) bits)
}
