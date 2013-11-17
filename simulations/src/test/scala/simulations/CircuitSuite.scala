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
    val in, c, o1, o0 = new Wire
    demux1to2(in, c, o1, o0)
    in.setSignal(true)
    c.setSignal(false)
    run
    assert(o0.getSignal === true)
    assert(o1.getSignal === false)
    
    c.setSignal(true)
    run
    assert(o0.getSignal === false)
    assert(o1.getSignal === true)
  }

//  test("demux built by 1to2demux") {
//    //0 control, 1 output
//    val o1, input1 = new Wire
//    demux(input1, List(), List(o1))
//    input1.setSignal(true)
//    run
//    assert(o1.getSignal === true, "demux 0 control")
//    
//    //1 control, 2 output
//    val o2, o3, c1, input2 = new Wire
//    demux(input2, List(c1), List(o2, o3))
//    input2.setSignal(true)
//    c1.setSignal(false)
//    run
//    assert(o2.getSignal === true)
//    assert(o3.getSignal === false)
//   
//    c1.setSignal(true)
//    run
//    assert(o2.getSignal === false)
//    assert(o3.getSignal === true)
//    
//    //2 control, 4 output
//    val b0, b1, b2, b3, d0, d1, input3 = new Wire
//    demux(input3, List(d0, d1), List(b0, b1, b2, b3))
//    input3.setSignal(true)
//    d0.setSignal(true)
//    d1.setSignal(false)
//    run
//    assert(b0.getSignal === false)
//    assert(b1.getSignal === false)
//    assert(b2.getSignal === true)
//    assert(b3.getSignal === false)
//    
//    d0.setSignal(false)
//    d1.setSignal(true)
//    run
//    assert(b0.getSignal === false)
//    assert(b1.getSignal === true)
//    assert(b2.getSignal === false)
//    assert(b3.getSignal === false)
//  }
  
   test("demux with 2 controls test") {
    val in, c0, c1, out1, out2,  out3, out4  = new Wire
    demux(in, List(c1, c0), List(out4, out3, out2, out1))
    in.setSignal(true)
    c0.setSignal(false)
    c1.setSignal(true)
    run
    
    assert(out1.getSignal === false, "out1 signal")
    assert(out2.getSignal === false, "out2 signal")
    assert(out3.getSignal === true, "out3 signal")
    assert(out4.getSignal === false, "out4 signal")
  }
  
  test("demux medium") {
    val in, c0, c1, o0, o1, o2, o3 = new Wire
    val c = c1 :: c0 :: Nil
    val o = o3 :: o2 :: o1 :: o0 :: Nil
    demux(in, c, o)

    run
    assert(o0.getSignal === false, 0)
    assert(o1.getSignal === false, 1)
    assert(o2.getSignal === false, 2)
    assert(o3.getSignal === false, 3)

    in.setSignal(true)
    run
    assert(o0.getSignal === true, 0)
    assert(o1.getSignal === false, 1)
    assert(o2.getSignal === false, 2)
    assert(o3.getSignal === false, 3)
    
    in.setSignal(true)
    c0.setSignal(true)
    run
    assert(o0.getSignal === false, 0)
    assert(o1.getSignal === true, 1)
    assert(o2.getSignal === false, 2)
    assert(o3.getSignal === false, 3)
  }
  
  test("demux large") {
    val in, c0, c1, c2, c3, o0, o1, o2, o3, o4, o5, o6, o7, o8, o9, o10, o11, o12, o13, o14, o15 = new Wire
    val c = c3 :: c2 :: c1 :: c0 :: Nil
    val o = o15 :: o14 :: o13 :: o12 :: o11 :: o10 :: o9 :: o8 :: o7 :: o6 :: o5 :: o4 :: o3 :: o2 :: o1 :: o0 :: Nil
    demux(in, c, o)

    run
    assert(o0.getSignal === false, 0)
    assert(o1.getSignal === false, 1)
    assert(o2.getSignal === false, 2)
    assert(o3.getSignal === false, 3)
    assert(o4.getSignal === false, "o4")
    assert(o5.getSignal === false, "o5")
    assert(o6.getSignal === false, "o6")
    assert(o7.getSignal === false, "o7")
    assert(o8.getSignal === false, "o8")
    assert(o9.getSignal === false, "o9")
    assert(o10.getSignal === false, "o10")
    assert(o11.getSignal === false, "o11")
    assert(o12.getSignal === false, "o12")
    assert(o13.getSignal === false, "o13")
    assert(o14.getSignal === false, "o14")
    assert(o15.getSignal === false, "o15")

    in.setSignal(true)
    run
    assert(o0.getSignal === true, 0)
    assert(o1.getSignal === false, 1)
    assert(o2.getSignal === false, 2)
    assert(o3.getSignal === false, 3)
    assert(o4.getSignal === false, "o4")
    assert(o5.getSignal === false, "o5")
    assert(o6.getSignal === false, "o6")
    assert(o7.getSignal === false, "o7")
    assert(o8.getSignal === false, "o8")
    assert(o9.getSignal === false, "o9")
    assert(o10.getSignal === false, "o10")
    assert(o11.getSignal === false, "o11")
    assert(o12.getSignal === false, "o12")
    assert(o13.getSignal === false, "o13")
    assert(o14.getSignal === false, "o14")
    assert(o15.getSignal === false, "o15")
    
    in.setSignal(true)
    c0.setSignal(true)
    c3.setSignal(true)
    run
    assert(o0.getSignal === false, 0)
    assert(o1.getSignal === false, 1)
    assert(o2.getSignal === false, 2)
    assert(o3.getSignal === false, 3)
    assert(o4.getSignal === false, "o4")
    assert(o5.getSignal === false, "o5")
    assert(o6.getSignal === false, "o6")
    assert(o7.getSignal === false, "o7")
    assert(o8.getSignal === false, "o8")
    assert(o9.getSignal === true, "o9")
    assert(o10.getSignal === false, "o10")
    assert(o11.getSignal === false, "o11")
    assert(o12.getSignal === false, "o12")
    assert(o13.getSignal === false, "o13")
    assert(o14.getSignal === false, "o14")
    assert(o15.getSignal === false, "o15")
  }
  
  
}
