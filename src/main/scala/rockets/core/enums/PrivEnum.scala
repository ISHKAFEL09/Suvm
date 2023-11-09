package rockets.core.enums

import spinal.core._

object PrivEnum extends SpinalEnum(binarySequential) {
  val PRV_U, PRV_S, PRV_H, PRV_M = newElement()
}
