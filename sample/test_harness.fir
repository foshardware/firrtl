circuit TestHarness :
  module IntXbar :
    input clock : Clock
    input reset : UInt<1>
    output auto : {flip int_in : UInt<1>[2], int_out : UInt<1>[2]}

    clock is invalid
    reset is invalid
    auto is invalid
    wire _T_7 : UInt<1>[2] @[Nodes.scala 333:76]
    _T_7 is invalid @[Nodes.scala 333:76]
    wire _T_15 : UInt<1>[2] @[Nodes.scala 332:76]
    _T_15 is invalid @[Nodes.scala 332:76]
    auto.int_out <- _T_15 @[LazyModule.scala 173:49]
    _T_7 <- auto.int_in @[LazyModule.scala 173:31]
    _T_15[0] <= _T_7[0] @[Xbar.scala 21:44]
    _T_15[1] <= _T_7[1] @[Xbar.scala 21:44]

