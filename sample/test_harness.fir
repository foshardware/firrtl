;buildInfoPackage: chisel3, version: 3.2-SNAPSHOT, scalaVersion: 2.12.4, sbtVersion: 1.1.1
circuit TestHarness :
  module IntXbar :
    input clock : Clock
    input reset : UInt<1>
    output auto : {flip int_in : UInt<1>[2], int_out : UInt<1>[2]}

    clock is invalid
    reset is invalid
    auto is invalid
    wire _T_7 : UInt<1>[2]
    _T_7 is invalid
    wire _T_15 : UInt<1>[2]
    _T_15 is invalid
    auto.int_out <- _T_15
    _T_7 <- auto.int_in
    _T_15[0] <= _T_7[0]
    _T_15[1] <= _T_7[1]

  extmodule plusarg_reader :
    output out : UInt<32>

    clock is invalid
    reset is invalid
    auto is invalid
