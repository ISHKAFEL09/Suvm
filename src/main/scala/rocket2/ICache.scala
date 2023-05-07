package rocket2

import chisel3._
import chisel3.util._
import rocket2.tilelink._


trait HasL1CacheParams extends HasCacheParams with HasCoreParams {
  val outerDataBeats: Int = p(TLDataBeats)
  val outerDataBits: Int = p(TLDataBits)
  val refillCyclesPerBeat: Int = outerDataBits / rowBits
  val refillCycles: Int = refillCyclesPerBeat * outerDataBeats
}

class Core2IMemIO extends Bundle {
  val req = Flipped(DecoupledIO(UInt(32.W)))
  val resp = ValidIO(UInt(32.W))
}

class ICache {

}
