// See LICENSE for license details.

package rocket2.tilelink

import chisel3._
import chisel3.util._

// This class encapsulates transformations on different directory information
// storage formats
abstract class DirectoryRepresentation(val width: Int) {
  def pop(prev: UInt, id: UInt): UInt
  def push(prev: UInt, id: UInt): UInt
  def flush: UInt
  def none(s: UInt): Bool
  def one(s: UInt): Bool
  def count(s: UInt): UInt
  def next(s: UInt): UInt
  def full(s: UInt): UInt
}

abstract trait HasDirectoryRepresentation {
  val dir: DirectoryRepresentation
}

class NullRepresentation(nClients: Int) extends DirectoryRepresentation(1) {
  def pop(prev: UInt, id: UInt) = 0.U
  def push(prev: UInt, id: UInt) = 0.U
  def flush  = 0.U
  def none(s: UInt) = false.B
  def one(s: UInt) = false.B
  def count(s: UInt) = UInt(nClients.W)
  def next(s: UInt) = 0.U
  def full(s: UInt) = (-1).S(width = nClients.W).asUInt
}

class FullRepresentation(nClients: Int) extends DirectoryRepresentation(nClients) {
  def pop(prev: UInt, id: UInt) =  prev &  (~UIntToOH(id)).asUInt
  def push(prev: UInt, id: UInt) = prev | UIntToOH(id)
  def flush = 0.U(width = width.W)
  def none(s: UInt) = s === 0.U
  def one(s: UInt) = PopCount(s) === 1.U
  def count(s: UInt) = PopCount(s)
  def next(s: UInt) = PriorityEncoder(s)
  def full(s: UInt) = s
}
