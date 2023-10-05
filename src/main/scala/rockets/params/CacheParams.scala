package rockets.params

import rockets._
import rockets.core.{Code, IdentityCode}
import spinal.core._

trait CacheParams {
  val nSets: Int
  val blockOffBits: Int
  val nWays: Int
  val rowBits: Int
  val code: Option[Code]
}

trait HasCacheParams {
  this: HasLinkParams =>

  val cacheParams: CacheParams

  def nSets: Int = cacheParams.nSets

  def blockOffBits: Int = cacheParams.blockOffBits

  def idxBits: Int = log2Up(cacheParams.nSets)

  def untagBits: Int = blockOffBits + idxBits

  def tagBits: Int = pAddrBits - untagBits

  def nWays: Int = cacheParams.nWays

  def wayBits: Int = log2Up(nWays)

  def isDM: Boolean = nWays == 1

  def rowBits: Int = cacheParams.rowBits

  def rowBytes: Int = rowBits / 8

  def rowOffBits: Int = log2Up(rowBytes)

  def code = cacheParams.code.getOrElse(new IdentityCode)
}
