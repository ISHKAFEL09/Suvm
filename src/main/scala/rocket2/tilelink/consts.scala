// See LICENSE for license details.

package rocket2.tilelink

import chisel3._
import chisel3.util._
import rocket2.config._

object MemoryOpConstants extends MemoryOpConstants

trait MemoryOpConstants {
  val MT_SZ = 3
  val MT_X  = BitPat("b???")
  val MT_B  = BitPat("b000")
  val MT_H  = BitPat("b001")
  val MT_W  = BitPat("b010")
  val MT_D  = BitPat("b011")
  val MT_BU = BitPat("b100")
  val MT_HU = BitPat("b101")
  val MT_WU = BitPat("b110")
  val MT_Q  = BitPat("b111")

  val NUM_XA_OPS = 9
  val M_SZ      = 5
  val M_X       = BitPat("b?????");
  val M_XRD     = BitPat("b00000"); // int load
  val M_XWR     = BitPat("b00001"); // int store
  val M_PFR     = BitPat("b00010"); // prefetch with intent to read
  val M_PFW     = BitPat("b00011"); // prefetch with intent to write
  val M_XA_SWAP = BitPat("b00100");
  val M_NOP     = BitPat("b00101");
  val M_XLR     = BitPat("b00110");
  val M_XSC     = BitPat("b00111");
  val M_XA_ADD  = BitPat("b01000");
  val M_XA_XOR  = BitPat("b01001");
  val M_XA_OR   = BitPat("b01010");
  val M_XA_AND  = BitPat("b01011");
  val M_XA_MIN  = BitPat("b01100");
  val M_XA_MAX  = BitPat("b01101");
  val M_XA_MINU = BitPat("b01110");
  val M_XA_MAXU = BitPat("b01111");
  val M_FLUSH   = BitPat("b10000") // write back dirty data and cede R/W permissions
  val M_PRODUCE = BitPat("b10001") // write back dirty data and cede W permissions
  val M_CLEAN   = BitPat("b10011") // write back dirty data and retain R/W permissions

  def isAMO(cmd: UInt) = cmd(3) || cmd === M_XA_SWAP
  def isPrefetch(cmd: UInt) = cmd === M_PFR || cmd === M_PFW
  def isRead(cmd: UInt) = cmd === M_XRD || cmd === M_XLR || isAMO(cmd)
  def isWrite(cmd: UInt) = cmd === M_XWR || cmd === M_XSC || isAMO(cmd)
  def isWriteIntent(cmd: UInt) = isWrite(cmd) || cmd === M_PFW || cmd === M_XLR

  object AmoAluOperandBits extends Field[Int]
}
