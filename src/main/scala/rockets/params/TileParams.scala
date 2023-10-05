package rockets.params

import config._

trait TileParams {
  val core: CoreParams
  val dCache: DCacheParams
  val link: LinkParams
}

object TileKey extends Field[TileParams]

trait HasTileParams {
  implicit val p: Parameters
  lazy val tileParams: TileParams = p(TileKey)
}
