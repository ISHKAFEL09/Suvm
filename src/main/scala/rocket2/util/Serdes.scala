package rocket2.util

import chisel3._
import chisel3.util._
import rocket2.tilelink._

class Serializer[T <: HasTileLinkData](gen: T, n: Int) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(DecoupledIO(chiselTypeOf(gen)))
    val out = DecoupledIO(chiselTypeOf(gen))
    val cnt = Output(UInt(log2Up(n).W))
    val done = Output(Bool())
  })

  val narrowWidth = gen.getWidth / n
  require(gen.getWidth % n == 0)

  if (n == 1) {
    io.done := true.B
    io.cnt := 0.U
    io.in <> io.out
  } else {
    val cnt = RegInit(0.U(n.W))
    val wrap = cnt === (n - 1).U
    val rbits = RegInit(0.U.asTypeOf(chiselTypeOf(gen)))
    val active = RegInit(false.B)

    val shifter = VecInit.fill(n)(0.U(narrowWidth.W))
    (0 until n).foreach {
      i => shifter(i) := rbits.data((i + 1) * narrowWidth - 1, i * narrowWidth)
    }

    io.done := WireDefault(false.B)
    io.cnt := cnt
    io.in.ready := !active
    io.out.valid := active || io.in.valid
    io.out.bits := io.in.bits
    when(!active && io.in.valid) {
      when(io.in.bits.hasData()) {
        cnt := Mux(io.out.ready, 1.U, 0.U)
        rbits := io.in.bits
        active := true.B
      }
      io.done := !io.in.bits.hasData()
    }
    when(active) {
      io.out.bits := rbits
      io.out.bits.data := shifter(cnt)
      when(io.out.ready) {
        cnt := cnt + 1.U
        when(wrap) {
          cnt := 0.U
          io.done := true.B
          active := false.B
        }
      }
    }
  }
}

object Serializer {
  def apply[T <: HasTileLinkData](in: DecoupledIO[T], n: Int): DecoupledIO[T] = {
    val s = Module(new Serializer(in.bits, n))
    s.io.in.valid := in.valid
    s.io.in.bits := in.bits
    in.ready := s.io.in.ready
    s.io.out
  }
}