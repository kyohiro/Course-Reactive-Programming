package simulations

import common._

class Wire {
  private var sigVal = false
  private var actions: List[Simulator#Action] = List()

  def getSignal: Boolean = sigVal
  
  def setSignal(s: Boolean) {
    if (s != sigVal) {
      sigVal = s
      actions.foreach(action => action())
    }
  }

  def addAction(a: Simulator#Action) {
    actions = a :: actions
    a()
  }
}

abstract class CircuitSimulator extends Simulator {

  val InverterDelay: Int
  val AndGateDelay: Int
  val OrGateDelay: Int

  def probe(name: String, wire: Wire) {
    wire addAction {
      () => afterDelay(0) {
        println(
          "  " + currentTime + ": " + name + " -> " +  wire.getSignal)
      }
    }
  }

  def inverter(input: Wire, output: Wire) {
    def invertAction() {
      val inputSig = input.getSignal
      afterDelay(InverterDelay) { output.setSignal(!inputSig) }
    }
    input addAction invertAction
  }

  def andGate(a1: Wire, a2: Wire, output: Wire) {
    def andAction() {
      val a1Sig = a1.getSignal
      val a2Sig = a2.getSignal
      afterDelay(AndGateDelay) { output.setSignal(a1Sig & a2Sig) }
    }
    a1 addAction andAction
    a2 addAction andAction
  }

  //
  // to complete with orGates and demux...
  //

  def orGate(a1: Wire, a2: Wire, output: Wire) {
    def orAction() {
      val a1Sig = a1.getSignal
      val a2Sig = a2.getSignal
      afterDelay(OrGateDelay) { output.setSignal(a1Sig | a2Sig) }
    }
    a1 addAction orAction
    a2 addAction orAction
  }
  
  def orGate2(a1: Wire, a2: Wire, output: Wire) {
    val na, nb, nab = new Wire
    inverter(a1, na)
    inverter(a2, nb)
    andGate(na, nb, nab)
    inverter(nab, output)
  }

  def demux(in: Wire, c: List[Wire], out: List[Wire]) {
    (c, out) match {
      //when there is no control and one output, just invert input twice
      case (Nil, o :: Nil) => {
        val inv = new Wire
        inverter(in, inv)
        inverter(inv, o)
      } 
      //when there is one control and two outputs, use 1to2 demux
      case (c :: Nil, List(o1, o0)) => {
        demux1to2(in, c, o1, o0)
      }
      //when there are more, build from multiple 1to2 demux
      case (c1 :: cw, o) => { 
        val w1, w0 = new Wire
        demux1to2(in, c1, w1, w0)
        demux(w1, cw, o.take(o.length/2)) //half high bits
        demux(w0, cw, o.drop(o.length/2)) //half low bits
      }
      case _ => println("cannot handle such cases")
    }
  }

  def demux1to2(in: Wire, c: Wire, o1: Wire, o0: Wire) {
    val inv = new Wire
    inverter(c, inv)
    andGate(in, inv, o0)
    andGate(in, c, o1)
  }
}

object Circuit extends CircuitSimulator {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5

  def andGateExample {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    probe("in1", in1)
    probe("in2", in2)
    probe("out", out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    in1.setSignal(true)
    run

    in2.setSignal(true)
    run
    
  }

}

object CircuitMain extends App {
  // You can write tests either here, or better in the test class CircuitSuite.
  Circuit.andGateExample
}
