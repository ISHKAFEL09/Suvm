package rockets.tilelink

import rockets.params._
import spinal.core._
import rockets.params.config._

abstract class TLBundle(implicit val p: Parameters)
    extends Bundle
    with HasLinkParams
    with HasCoreParams
    with HasTileParams

abstract class TLComponent(implicit val p: Parameters)
  extends Component
    with HasLinkParams
    with HasCoreParams
    with HasTileParams

class TileLink {}
