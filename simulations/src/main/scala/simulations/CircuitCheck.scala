package simulations


import org.scalacheck._
import Prop._

object CircuitCheck extends Properties("Circuit") {
   import Circuit.{demux, run}
   
   def genBoolean = Gen.oneOf(Gen.value(true), Gen.value(false))
   
   def genShortListBoolean = for {
     n <- Gen.choose(0, 8)
     g <- Gen.listOfN(n, genBoolean)
   } yield g
   
   def signalToBit(s: Boolean) = if (s) 1 else 0
   
   def setWiresToSignal(w: List[Wire], s: List[Boolean]) =  
     (w zip s) foreach {case (w, s) => w setSignal s}
   
   def controlToBitNumber(bs: List[Boolean]) = bs.foldLeft(0){
     case (acc, b) => 2 * acc + signalToBit(b)
   }
   
   def countOneBits(ws: List[Wire]) = ws.map(_.getSignal).filter(identity).length

   property("demux") = forAll (genBoolean, genShortListBoolean) {
     (sIn: Boolean, sControl: List[Boolean]) => {
       val numBitsControl = sControl.length
       val numBitsOutput = 1 << numBitsControl
       val wIn = new Wire
       val wControl = List.fill(numBitsControl)(new Wire)
       val wOutput = List.fill(numBitsOutput)(new Wire)
       
       demux(wIn, wControl, wOutput)
       wIn.setSignal(sIn)
       setWiresToSignal(wControl, sControl)
       run
       
       val numBitInOut = numBitsOutput - 1 - controlToBitNumber(sControl)
       val targetInput = wOutput(numBitInOut).getSignal == sIn
       
       targetInput && countOneBits(wOutput) == signalToBit(sIn)
     }
   }
}