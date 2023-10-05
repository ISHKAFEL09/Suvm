package rockets.core

import spinal.core._

object MemOps {
  val MT_SZ = 3
  val MT_X  = M"---"
  val MT_B  = M"000"
  val MT_H  = M"001"
  val MT_W  = M"010"
  val MT_D  = M"011"
  val MT_BU = M"100"
  val MT_HU = M"101"
  val MT_WU = M"110"
  val MT_Q  = M"111"

  val NUM_XA_OPS = 9
  val M_SZ      = 5
  val M_X       = M"-----"
  val M_XRD     = M"00000" // int load
  val M_XWR     = M"00001" // int store
  val M_PFR     = M"00010" // prefetch with intent to read
  val M_PFW     = M"00011" // prefetch with intent to write
  val M_XA_SWAP = M"00100"
  val M_NOP     = M"00101"
  val M_XLR     = M"00110"
  val M_XSC     = M"00111"
  val M_XA_ADD  = M"01000"
  val M_XA_XOR  = M"01001"
  val M_XA_OR   = M"01010"
  val M_XA_AND  = M"01011"
  val M_XA_MIN  = M"01100"
  val M_XA_MAX  = M"01101"
  val M_XA_MINU = M"01110"
  val M_XA_MAXU = M"01111"
  val M_FLUSH   = M"10000" // write back dirty data and cede R/W permissions
  val M_PRODUCE = M"10001" // write back dirty data and cede W permissions
  val M_CLEAN   = M"10011" // write back dirty data and retain R/W permissions

  def isAMO(cmd: UInt) = cmd(3) || cmd === M_XA_SWAP
  def isPrefetch(cmd: UInt) = cmd === M_PFR || cmd === M_PFW
  def isRead(cmd: UInt) = cmd === M_XRD || cmd === M_XLR || isAMO(cmd)
  def isWrite(cmd: UInt) = cmd === M_XWR || cmd === M_XSC || isAMO(cmd)
  def isWriteIntent(cmd: UInt) = isWrite(cmd) || cmd === M_PFW || cmd === M_XLR
}
