package simulations

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers


@RunWith(classOf[JUnitRunner])
class CircuitSuite extends CircuitSimulator with FunSuite with Checkers{
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5
  
  test("inverter example") {
    val in1, out = new Wire
    inverter(in1, out)
    in1.setSignal(true)
    run
    assert(out.getSignal === false, "inverter 1")
    in1.setSignal(false)
    run
    assert(out.getSignal === true, "inverter 2")
  }

  test("andGate example") {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run
    
    assert(out.getSignal === false, "and 1")

    in1.setSignal(true)
    run
    
    assert(out.getSignal === false, "and 2")

    in2.setSignal(true)
    run
    
    assert(out.getSignal === true, "and 3")
  }
  
  test("orGage example") {
    val in1, in2, out = new Wire
    orGate(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run
    assert(out.getSignal === false, "or 1")
    
    in1.setSignal(true)
    run
    assert(out.getSignal === true, "or 2")
    
    in2.setSignal(true)
    run
    assert(out.getSignal === true, "or 3")
  }
  
  test("orGage2 example") {
    val in1, in2, out = new Wire
    orGate2(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run
    assert(out.getSignal === false, "or 1")
    
    in1.setSignal(true)
    run
    assert(out.getSignal === true, "or 2")
    
    in2.setSignal(true)
    run
    assert(out.getSignal === true, "or 3")
  }
  
  test("1to2 demux") {
    val in, c, o1, o2 = new Wire
    demux1to2(in, c, o1, o2)
    in.setSignal(true)
    c.setSignal(false)
    run
    assert(o1.getSignal === true)
    assert(o2.getSignal === false)
    
    c.setSignal(true)
    run
    assert(o1.getSignal === false)
    assert(o2.getSignal === true)
  }

  test("demux built by 1to2demux") {
    //0 control, 1 output
    val o1, input1 = new Wire
    demux(input1, List(), List(o1))
    input1.setSignal(true)
    run
    assert(o1.getSignal === true, "demux 0 control")
    
    //1 control, 2 output
    val o2, o3, c1, input2 = new Wire
    demux(input2, List(c1), List(o2, o3))
    input2.setSignal(true)
    c1.setSignal(false)
    run
    assert(o2.getSignal === true)
    assert(o3.getSignal === false)
   
    c1.setSignal(true)
    run
    assert(o2.getSignal === false)
    assert(o3.getSignal === true)
    
    //2 control, 4 output
    val b0, b1, b2, b3, d0, d1, input3 = new Wire
    demux(input3, List(d0, d1), List(b0, b1, b2, b3))
    input3.setSignal(true)
    d0.setSignal(true)
    d1.setSignal(false)
    run
    assert(b0.getSignal === false)
    assert(b1.getSignal === false)
    assert(b2.getSignal === true)
    assert(b3.getSignal === false)
    
    d0.setSignal(false)
    d1.setSignal(true)
    run
    assert(b0.getSignal === false)
    assert(b1.getSignal === true)
    assert(b2.getSignal === false)
    assert(b3.getSignal === false)
  }
  
  
}
