package uvm.chiter

import com.sun.jna._
import firrtl._
import org.fusesource.scalate._
import treadle.PlusArgsAnnotation
import chisel3._
import uvm._

trait VerilatorSimulator extends ChiterSimulator {
  private var _so: Option[NativeLibrary] = None
  private var _simState: Option[Pointer] = None
  private var _top: Option[TopModuleInfo] = None
  private val mask64 = (BigInt(1) << 64) - 1

  lazy val so: NativeLibrary = _so.get
  lazy val simState: Pointer = _simState.get
  lazy val topInfo: TopModuleInfo = _top.get

  def setArgs(args: Array[String]): Unit =
    so.getFunction("set_args").invokeVoid(Array(Integer.valueOf(args.length), args))
    
  override def getTimeNow: BigInt =
    BigInt(getTimeFunc.invokeLong(Array(simState)))

  override def poke(signal: Data, value: BigInt): Unit = {
    assert(isRunning)
    val signalName = ports(signal)
    val (pin, sid) = getPin(topInfo.pokeable, signalName)
    var maskedValue = value & pin.mask
    val words = (pin.width + 63) / 64
    0 until words foreach { i =>
      pokeFunc.invokeVoid(Array(
        simState,
        Integer.valueOf(sid),
        Integer.valueOf(i),
        Long.box((maskedValue & mask64).toLong)
      ))
      maskedValue = maskedValue >> 64
    }
  }

  override def peek(signal: Data): BigInt = {
    assert(isRunning)
    val signalName = ports(signal)
    val (pin, sid) = getPin(topInfo.peekable, signalName)
    var value = BigInt(0)
    val words = (pin.width + 63) / 64
    0 until words foreach { i =>
      val w = BigInt(peekFunc.invokeLong(Array(
        simState,
        Integer.valueOf(sid),
        Integer.valueOf(i)
      ))) & mask64
      value = value | (w << (i * 64))
    }
    if (pin.signed)
      -(((~value) + 1) & ((BigInt(1) << pin.width) - 1))
    else
      value & ((BigInt(1) << pin.width) - 1)
  }

  override def step(n: Int): Unit = {
    assert(isRunning)
    update()
    val ret = takeSteps(n)
    val status = (ret >> 32) & 3
    if (status != 0)
      uvmFatal("CHITER", "The simulator has encountered an unrecoverable error.\n" +
        "Please consult the standard output and error for more details.")
  }

  override def simFinish(): Unit = {
    assert(isRunning, "Simulator is already stopped!")
    finishFunc.invokeVoid(Array(simState))
    isRunning = false
  }

  private var isRunning = true

  lazy val updateFunc: Function = so.getFunction("update")
  lazy val stepFunc: Function = so.getFunction("step")
  lazy val pokeFunc: Function = so.getFunction("poke")
  lazy val peekFunc: Function = so.getFunction("peek")
  lazy val getTimeFunc: Function = so.getFunction("get_time")
  lazy val finishFunc: Function = so.getFunction("finish")

  private def update(): Unit = {
    assert(isRunning)
    updateFunc.invokeVoid(Array(simState))
  }

  private def takeSteps(cycles: Int): Long = {
    assert(isRunning)
    require(cycles > 0)
    stepFunc.invokeLong(Array(simState, Integer.valueOf(cycles)))
  }

  private def getPin(seq: Seq[(PinInfo, Int)], signal: String): (PinInfo, Int) =
    seq.find { case (info, _) =>
      info.name == signal
    }.getOrElse(throw new RuntimeException(s"Unknown signal: $signal"))

  private def generateHarness(targetDir: os.Path): String = {
    val topName = topInfo.name
    val cppHarnessFileName = s"${topName}_harness.cpp"
    val vcdFile = targetDir / s"$topName.vcd"
    val template = getClass.getClassLoader.getResource("uvm/chiter/verilator/VerilatorHarness.mustache").getPath
    val engine = new TemplateEngine

    val codeBuffer = new StringBuilder
    topInfo.pokeable.foreach { case (PinInfo(name, width, _), id) =>
      if (width > 64)
        codeBuffer.append(s"case $id: data = (WData*)&(ss->dut->$name); words = ${(width - 1) / 32 + 1}; break;\n")
      else
        codeBuffer.append(s"case $id: ss->dut->$name = u; return;\n")
    }
    val pokeSwitches = codeBuffer.toString()

    codeBuffer.clear()
    topInfo.peekable.foreach { case (PinInfo(name, width, _), id) =>
      if (width > 64)
        codeBuffer.append(s"case $id: data = (WData*)&(ss->dut->$name); words = ${(width - 1) / 32 + 1}; break;\n")
      else
        codeBuffer.append(s"case $id: return (ss->dut->$name);\n")
    }
    val peekSwitches = codeBuffer.toString()

    val code = engine.layout(new java.io.File(template).getCanonicalPath, Map(
      "dutName" -> topName,
      "vcdFilePath" -> vcdFile,
      "pokeSwitches" -> pokeSwitches,
      "peekSwitches" -> peekSwitches
    ))
    os.write.over(targetDir / cppHarnessFileName, code)
    cppHarnessFileName
  }

  private val cFlags: Seq[String] = Seq(
    "-fPIC",
    "-shared",
    "-fvisibility=hidden",
    "-O1",
    "-DVL_USER_STOP",
    "-DVL_USER_FATAL",
    "-DVL_USER_FINISH"
  )
    
  private val ldFlags: Seq[String] = Seq(
    "-shared", 
    "-dynamiclib", 
    "-fvisibility=hidden"
  )

  private def osRun(cmd: Seq[String], cwd: os.Path): os.CommandResult = {
    // print the command and pipe the output to stdout
    os.proc(cmd)
      .call(cwd = cwd, stdout = os.ProcessOutput.Readlines(println), stderr = os.ProcessOutput.Readlines(println))
  }
  
  private def verilatorCC(targetDir: os.Path, 
                          cppHarness: String,
                          annos: AnnotationSeq): os.Path = {
    val verilatedDir = targetDir / "verilated"
    val flagAnnos = VerilatorLinkFlags(ldFlags) +: VerilatorCFlags(cFlags) +: annos
    
    val userCFlags = flagAnnos.collect { case VerilatorCFlags(f) => f }.flatten
    val userLdFlags = flagAnnos.collect { case VerilatorLinkFlags(f) => f }.flatten
    val userFlags = flagAnnos.collectFirst { case VerilatorFlags(f) => f }.getOrElse(Seq.empty)
    val flags = Seq(
      "--trace",
      "--assert", // we always enable assertions
      "--coverage-user", // we always enable use coverage
      "-Wno-fatal",
      "-Wno-WIDTH",
      "-Wno-STMTDLY",
      "--top-module",
      topInfo.name,
      "+define+TOP_TYPE=V" + topInfo.name,
      // flags passed to the C++ compiler
      "-CFLAGS",
      userCFlags.mkString(" "),
      "-LDFLAGS",
      userLdFlags.mkString(" "),
      // name of the directory that verilator generates the C++ model + Makefile in
      "-Mdir",
      verilatedDir.toString()
    ) ++ userFlags
    
    val cmd = Seq(
      "verilator",
      "--cc",
      "--exe",
      cppHarness
    ) ++ flags :+ s"${topInfo.name}.sv"
    
    val ret = osRun(cmd, targetDir)
    assert(ret.exitCode == 0, s"verilator command failed on circuit ${topInfo.name} in work dir $targetDir")
    verilatedDir
  }

  private def compile(verilatedDir: os.Path): os.Path = {
    val target = s"V${topInfo.name}"
    val processorCount = Runtime.getRuntime.availableProcessors.toString
    val cmd = Seq("make", "-C", verilatedDir.toString(), "-j", processorCount, "-f", s"V${topInfo.name}.mk", target)
    val ret = osRun(cmd, null)
    assert(
      ret.exitCode == 0,
      s"Compilation of verilator generated code failed for circuit ${topInfo.name} in work dir $verilatedDir"
    )
    val simBinary = verilatedDir / target
    assert(os.exists(simBinary), s"Failed to generate simulation binary: $simBinary")
    simBinary
  }
  
  override def createTester(rtl: CircuitState, needCompile: Boolean): Unit = {
    val targetDir = Compiler.requireTargetDir(rtl.annotations)
    _top = Some(TopModuleInfo(rtl.circuit))
    logger.Logger.setOutput((targetDir / "run.log").toString)
    println(s"Log written to file: ${targetDir / "run.log"}")

    val opts = new java.util.HashMap[String, Int]()
    opts.put(Library.OPTION_OPEN_FLAGS, 2)

    if (needCompile) {
      val cppHarness = generateHarness(targetDir)
      val verilogState = Compiler.lowFirrtlToSystemVerilog(rtl)
      val verilatedDir = verilatorCC(targetDir, cppHarness, rtl.annotations)
      val libPath = compile(verilatedDir)
      _so = Some(NativeLibrary.getInstance(libPath.toString(), opts))
    } else {
      val libPath = targetDir / "verilated" / s"V${topInfo.name}"
      _so = Some(NativeLibrary.getInstance(libPath.toString(), opts))
    }

    _simState = Some(so.getFunction("sim_init").invokePointer(Array()))
    
    setArgs(rtl.annotations.view.collect { 
      case PlusArgsAnnotation(args) => args }.flatten.toArray)
  }
}

trait VerilatorBackend extends ChiterBackend with ChiterMultiThreadBackend with VerilatorSimulator