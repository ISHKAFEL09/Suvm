package rockets.tile

import rockets.params._
import rockets.params.config.Parameters
import rockets.tilelink._
import rockets.utils._

object Configs {
  implicit val SimpleConfig = Parameters((_, _, _) => {
    case TileKey =>
      new TileParams {
        override val core: CoreParams = new CoreParams {
          override val xLen: Int = 32
          override val retireWidth: Int = 0
          override val coreFetchWidth: Int = 0
          override val coreInstBits: Int = 32
          override val coreDCacheRegTagBits: Int = 24
          override val fastLoadByte: Boolean = false
          override val fastLoadWord: Boolean = false
          override val maxHartIdBits: Int = 1
        }
        override val dCache: DCacheParams = new DCacheParams {
          override val nSDQ: Int = 8
          override val nMSHRs: Int = 1
          override val nTLBs: Int = 16
          override val nSets: Int = 8
          override val blockOffBits: Int = 6
          override val nWays: Int = 4
          override val rowBits: Int = 128
          override val code: Option[Code] = None
        }
        override val link: LinkParams = new LinkParams {
          override val pAddrBits: Int = 34
          override val vAddrBits: Int = 32
          override val pgIdxBits: Int = 12
          override val ppnBits: Int = 22
          override val vpnBits: Int = 20
          override val pgLevels: Int = 2
          override val asIdBits: Int = 1
          override val pgLevelBits: Int = 10
          override val TLDataBeats: Int = 4
          override val TLDataBits: Int = 128
          override val coherencePolicy: CoherencePolicy =
            MESICoherence(new DirectoryRepresentation() {
              override val width: Int = 0
            })

          /** unique name per TL network */
          override val TLId: String = "nbdcache"

          /** manager agents number for this network */
          override val TLManagerNum: Int = 1

          /** client agents number for this network */
          override val TLClientNum: Int = 1

          /** number of client agents that cache data */
          override val TLCacheClientNum: Int = 1

          /** number of client agents that do not cache data */
          override val TLNoCacheClientNum: Int = 0

          /** maximum number of outstanding xact per client */
          override val TLMaxClientOst: Int = 1

          /** maximum number of clients multiplexed onto one port */
          override val TLMaxClientsPerPort: Int = 1

          /** maximum number of outstanding xact per manager */
          override val TLMaxManagerOst: Int = 1

          /** width of cache block address */
          override val TLBlockAddrBits: Int = 28

          /** amo alu op size */
          override val AmoOperandBits: Int = 32
        }
      }
    case _ =>
  })
}
