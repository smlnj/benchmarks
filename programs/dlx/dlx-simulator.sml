(* dlx-simulator.sml
 *)

structure DLXSimulatorC1 : DLXSIMULATOR
  = DLXSimulatorFun (structure RF = RegisterFile
                     structure ALU = ALU
                     structure MEM = L1Cache1 )