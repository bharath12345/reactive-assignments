package simulations

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CircuitSuite extends CircuitSimulator with FunSuite {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5
  
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

  //
  // to complete with tests for orGate, demux, ...
  //
  
  test("demux zero") {
    val in = new Wire
    val c = List()
    val out = List(new Wire)
    demux(in, c, out)
    
    in.setSignal(true)
    run
    assert(out(0).getSignal === true, "demux true")
    
    in.setSignal(false)
    run
    assert(out(0).getSignal === false, "demux false")
    
  }

  test("demux one") {
    val A = new Wire
    
    val X = new Wire
    val c = List(X)
    
    val y1 = new Wire
    val y2 = new Wire
    val out = List(y1, y2)
    
    demux(A, c, out)
    
    A.setSignal(false)
    X.setSignal(false)
    run
    assert(y1.getSignal === false, "demux: x = false, y1 = false")
    assert(y2.getSignal === false, "demux: x = false, y2 = A = false")
    
    ////
    
    A.setSignal(true)
    X.setSignal(false)
    run
    assert(y1.getSignal === false, "demux: x = false, y1 = false")
    assert(y2.getSignal === true, "demux: x = false, y2 = A = true")
    
    ////
    A.setSignal(false)
    X.setSignal(true)
    run
    assert(y1.getSignal === false, "demux: x = true, y1 = A = false")
    assert(y2.getSignal === false, "demux: x = true, y2 = false")
    
    ////
    A.setSignal(true)
    X.setSignal(true)
    run
    assert(y1.getSignal === true, "demux: x = true, y1 = A = true")
    assert(y2.getSignal === false, "demux: x = true, y2 = false")
    
  }

  
}
