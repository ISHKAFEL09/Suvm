package rocket2

import chisel3._

class Ctrl2DataIO extends Bundle {
  val selPC = Output(UInt(3.W))
  val wenBtb = Output(Bool())
  val stallFetch = Output(Bool())
  val stallDec = Output(Bool())
  val killFetch = Output(Bool())
  val killDec = Output(Bool())
  val ren1 = Output(Bool())
  val ren2 = Output(Bool())
  val selAlu1 = Output(Bool())
  val selAlu2 = Output(UInt(2.W))
  val aluDW = Output(UInt(1.W))
  val aluFun = Output(UInt(4.W))
  val mulVal = Output(Bool())
  val mulWb = Output(Bool())
  val mulFun = Output(UInt(3.W))
  val divVal = Output(Bool())
  val divWb = Output(Bool())
  val divFun = Output(UInt(4.W))
  val wen = Output(Bool())
  val selWa = Output(Bool())
  val selWb = Output(UInt(3.W))
  val renPcr = Output(Bool())
  val wenPcr = Output(Bool())
  val xcptIllegal = Output(Bool())
  val xcptFPU = Output(Bool())
  val xcptSysCall = Output(Bool())
  val eret = Output(Bool())
  val dcacheMiss = Output(Bool())
  // from datapath
  val bitHit = Input(Bool())
  val inst = Input(UInt(32.W))
  val brEq = Input(Bool())
  val brLt = Input(Bool())
  val brLtu = Input(Bool())
  val divResultVal = Input(Bool())
  val divRdy = Input(Bool())
  val mulResultVal = Input(Bool())
  val exWrAddr = Input(UInt(5.W))
  val exception = Input(Bool())
  val status = Input(UInt(8.W))
  val sbSet = Input(Bool())
  val sbSetA = Input(UInt(5.W))
  val sbClr0 = Input(Bool())
  val sbClr0Addr = Input(UInt(5.W))
  val sbClr1 = Input(Bool())
  val sbClr1Addr = Input(UInt(5.W))
}

class CtrlPath {

}
