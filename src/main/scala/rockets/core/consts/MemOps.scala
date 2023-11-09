package rockets.core.consts

import spinal.core._

object MemOps {
  val MT_SZ = 3
  def MT_B  = U"000"
  def MT_H  = U"001"
  def MT_W  = U"010"
  def MT_D  = U"011"
  def MT_BU = U"100"
  def MT_HU = U"101"
  def MT_WU = U"110"
  def MT_Q  = U"111"

  val NUM_XA_OPS = 9
  val M_SZ      = 5
  def M_XRD     = U"00000" // int load
  def M_XWR     = U"00001" // int store
  def M_PFR     = U"00010" // prefetch with intent to read
  def M_PFW     = U"00011" // prefetch with intent to write
  def M_XA_SWAP = U"00100"
  def M_NOP     = U"00101"
  def M_XLR     = U"00110"
  def M_XSC     = U"00111"
  def M_XA_ADD  = U"01000"
  def M_XA_XOR  = U"01001"
  def M_XA_OR   = U"01010"
  def M_XA_AND  = U"01011"
  def M_XA_MIN  = U"01100"
  def M_XA_MAX  = U"01101"
  def M_XA_MINU = U"01110"
  def M_XA_MAXU = U"01111"
  def M_FLUSH   = U"10000" // write back dirty data and cede R/W permissions
  def M_PRODUCE = U"10001" // write back dirty data and cede W permissions
  def M_CLEAN   = U"10011" // write back dirty data and retain R/W permissions

  def isAMO(cmd: UInt) = cmd(3) || cmd === M_XA_SWAP
  def isPrefetch(cmd: UInt) = cmd === M_PFR || cmd === M_PFW
  def isRead(cmd: UInt) = cmd === M_XRD || cmd === M_XLR || isAMO(cmd)
  def isWrite(cmd: UInt) = cmd === M_XWR || cmd === M_XSC || isAMO(cmd)
  def isWriteIntent(cmd: UInt) = isWrite(cmd) || cmd === M_PFW || cmd === M_XLR
}
