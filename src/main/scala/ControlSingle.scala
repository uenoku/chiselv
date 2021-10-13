import Instruction._
import chisel3._
import chisel3.util._

class ControlSingle(
  bitWidth: Int = 32,
  instructionMemorySize: Int = 1 * 1024,
  dataMemorySize: Int = 1 * 1024,
  memoryFile: String = "",
) extends Module {

  // Instantiate the modules
  val registerBank = Module(new RegisterBank(bitWidth))
  registerBank.io.regPort.writeEnable := false.B
  registerBank.io.regPort.rs1_addr    := 0.U
  registerBank.io.regPort.rs2_addr    := 0.U
  registerBank.io.regPort.regwr_addr  := 0.U
  registerBank.io.regPort.regwr_data  := 0.S
  dontTouch(registerBank.io.regPort)

  val PC = Module(new ProgramCounter(bitWidth))
  PC.io.pcPort.countEnable := false.B
  PC.io.pcPort.writeEnable := false.B
  PC.io.pcPort.dataIn      := DontCare
  PC.io.pcPort.writeAdd    := false.B
  dontTouch(PC.io.pcPort)

  val ALU = Module(new ALU(bitWidth))
  ALU.io.ALUPort.inst := ERR_INST
  ALU.io.ALUPort.a    := 0.U
  ALU.io.ALUPort.b    := 0.U
  dontTouch(ALU.io.ALUPort)

  val decoder = Module(new Decoder(bitWidth))
  decoder.io.DecoderPort.op := DontCare
  dontTouch(decoder.io.DecoderPort)

  val instructionMemory = Module(new DualPortRAM(bitWidth, instructionMemorySize, memoryFile))
  instructionMemory.io.dualPort.readAddr    := DontCare
  instructionMemory.io.dualPort.writeAddr   := DontCare
  instructionMemory.io.dualPort.writeEnable := false.B
  instructionMemory.io.dualPort.writeData   := 0.U
  dontTouch(instructionMemory.io.dualPort)

  val dataMemory = Module(new DualPortRAM(bitWidth, dataMemorySize))
  dataMemory.io.dualPort.readAddr    := DontCare
  dataMemory.io.dualPort.writeAddr   := DontCare
  dataMemory.io.dualPort.writeEnable := false.B
  dataMemory.io.dualPort.writeData   := 0.U
  dontTouch(dataMemory.io.dualPort)

  // --- CPU Control --- //
  PC.io.pcPort.countEnable               := true.B
  instructionMemory.io.dualPort.readAddr := PC.io.pcPort.dataOut
  decoder.io.DecoderPort.op              := instructionMemory.io.dualPort.readData

  // ALU Operations
  when(decoder.io.DecoderPort.toALU) {
    registerBank.io.regPort.rs1_addr := decoder.io.DecoderPort.rs1
    registerBank.io.regPort.rs2_addr := decoder.io.DecoderPort.rs2

    ALU.io.ALUPort.inst := decoder.io.DecoderPort.inst
    ALU.io.ALUPort.a    := registerBank.io.regPort.rs1.asUInt()
    ALU.io.ALUPort.b := Mux(
      decoder.io.DecoderPort.use_imm,
      decoder.io.DecoderPort.imm.asUInt(),
      registerBank.io.regPort.rs2.asUInt(),
    )

    registerBank.io.regPort.writeEnable := true.B
    registerBank.io.regPort.regwr_addr  := decoder.io.DecoderPort.rd
    registerBank.io.regPort.regwr_data  := ALU.io.ALUPort.x.asSInt()
  }

  // Branch Operations
  when(decoder.io.DecoderPort.branch) {
    registerBank.io.regPort.rs1_addr := decoder.io.DecoderPort.rs1
    registerBank.io.regPort.rs2_addr := decoder.io.DecoderPort.rs2
    ALU.io.ALUPort.a                 := registerBank.io.regPort.rs1.asUInt()
    ALU.io.ALUPort.b                 := registerBank.io.regPort.rs2.asUInt()
    switch(decoder.io.DecoderPort.inst) {
      is(BEQ)(ALU.io.ALUPort.inst  := EQ)
      is(BNE)(ALU.io.ALUPort.inst  := NEQ)
      is(BLT)(ALU.io.ALUPort.inst  := SLT)
      is(BGE)(ALU.io.ALUPort.inst  := GTE)
      is(BLTU)(ALU.io.ALUPort.inst := SLTU)
      is(BGEU)(ALU.io.ALUPort.inst := GTEU)
    }
    when(ALU.io.ALUPort.x === 1.U) {
      PC.io.pcPort.writeEnable := true.B
      PC.io.pcPort.writeAdd    := true.B
      PC.io.pcPort.dataIn      := decoder.io.DecoderPort.imm
    }
  }
  // Jump Operations
  when(decoder.io.DecoderPort.jump) {
    when(decoder.io.DecoderPort.inst === JAL) {
      // Write next instruction address to rd
      registerBank.io.regPort.writeEnable := true.B
      registerBank.io.regPort.regwr_addr  := decoder.io.DecoderPort.rd
      registerBank.io.regPort.regwr_data  := (PC.io.pcPort.dataOut + 4.U).asSInt()
      // Set PC to jump address
      PC.io.pcPort.writeEnable := true.B
      PC.io.pcPort.writeAdd    := true.B
      PC.io.pcPort.dataIn      := decoder.io.DecoderPort.imm
    }
    when(decoder.io.DecoderPort.inst === JALR) {
      // Write next instruction address to rd
      registerBank.io.regPort.writeEnable := true.B
      registerBank.io.regPort.regwr_addr  := decoder.io.DecoderPort.rd
      registerBank.io.regPort.regwr_data  := (PC.io.pcPort.dataOut + 4.U).asSInt()
      // Set PC to jump address
      PC.io.pcPort.writeEnable         := true.B
      registerBank.io.regPort.rs1_addr := decoder.io.DecoderPort.rs1
      PC.io.pcPort.dataIn := Cat(
        (registerBank.io.regPort.rs1 + decoder.io.DecoderPort.imm.asSInt()).asUInt()(31, 1),
        0.U,
      )
    }
  }
  // LUI
  when(decoder.io.DecoderPort.inst === LUI) {
    registerBank.io.regPort.writeEnable := true.B
    registerBank.io.regPort.regwr_addr  := decoder.io.DecoderPort.rd
    registerBank.io.regPort.regwr_data  := Cat(decoder.io.DecoderPort.imm(31, 12), Fill(12, 0.U)).asSInt()
  }
  // AUIPC
  when(decoder.io.DecoderPort.inst === AUIPC) {
    registerBank.io.regPort.writeEnable := true.B
    registerBank.io.regPort.regwr_addr  := decoder.io.DecoderPort.rd
    registerBank.io.regPort.regwr_data := (PC.io.pcPort.dataOut + Cat(
      decoder.io.DecoderPort.imm(31, 12),
      Fill(12, 0.U),
    )).asSInt()
  }

  // Stores
  val memoryOffset = 0x8000_0000L.U
  when(decoder.io.DecoderPort.is_store) {
    when(decoder.io.DecoderPort.inst === SW) {
      registerBank.io.regPort.rs1_addr := decoder.io.DecoderPort.rs1
      registerBank.io.regPort.rs2_addr := decoder.io.DecoderPort.rs2

      dataMemory.io.dualPort.writeEnable := true.B
      dataMemory.io.dualPort.writeAddr :=
        registerBank.io.regPort.rs1.asUInt() +
          decoder.io.DecoderPort.imm + memoryOffset
      dataMemory.io.dualPort.writeData := registerBank.io.regPort.rs2.asUInt()
    }
  }
  // Loads
  when(decoder.io.DecoderPort.is_load) {
    when(decoder.io.DecoderPort.inst === LW) {
      registerBank.io.regPort.rs1_addr := decoder.io.DecoderPort.rs1
      dataMemory.io.dualPort.readAddr :=
        registerBank.io.regPort.rs1.asUInt() +
          decoder.io.DecoderPort.imm + memoryOffset

      registerBank.io.regPort.writeEnable := true.B
      registerBank.io.regPort.regwr_addr  := decoder.io.DecoderPort.rd
      registerBank.io.regPort.regwr_data  := dataMemory.io.dualPort.readData.asSInt()
    }
  }
}
