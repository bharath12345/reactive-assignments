package nodescala

import scala.language.postfixOps
import scala.util.{ Try, Success, Failure }
import scala.collection._
import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.async.Async.{ async, await }
import org.scalatest._
import NodeScala._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class NodeScalaSuite extends FunSuite {

  test("A Future should always be created") {
    val always = Future.always(517)

    assert(Await.result(always, 0 nanos) == 517)
  }

  test("A Future should never be created") {
    val never = Future.never[Int]

    try {
      Await.result(never, 1 second)
      assert(false)
    } catch {
      case t: TimeoutException => // ok!
    }
  }

  test("List of Futures => Future of a List") {
    val x = future(1)
    val y = future(2)
    val z = future(3)

    val fs = List(x, y, z)
    val afs = Future.all(fs)

    assert(Await.result(afs, 100 millis) == List(1, 2, 3))
  }

  test("List of Futures *with DELAY* => Future of a List") {
    val x = future { blocking(Thread.sleep(3000L)); 1 }
    val y = future { blocking(Thread.sleep(2000L)); 2 }
    val z = future { blocking(Thread.sleep(4000L)); 3 }

    val fs = List(x, y, z)
    val afs = Future.all(fs)

    assert(Await.result(afs, 10 seconds) == List(1, 2, 3))
  }

  test("Get first completed future") {
    val x = future { blocking(Thread.sleep(3000L)); "doneX" }
    val y = future { blocking(Thread.sleep(2000L)); "doneY" }
    val z = future { blocking(Thread.sleep(4000L)); "doneZ" }

    val fs = List(x, y, z)
    val afs = Future.any(fs)
    assert(Await.result(afs, 5 seconds) == "doneY")
  }

  test("Lets delay that future - 1 sec") {
    val x = Future.delay(4 seconds)
    assert(x.isCompleted == false)
  }

  test("Lets delay that future AGAIN - 5 sec") {
    val x = Future.delay(4 seconds)
    blocking(Thread.sleep(5000L));
    assert(x.isCompleted == true)
  }

  //// FutureOps => Here on...

  test("Get me NOW!") {
    val x = future { blocking(Thread.sleep(3000L)); "doneX" }
    val f = FutureOps(x)

    try {
      f.now
      assert(false)
    } catch {
      case t: NoSuchElementException => // ok!
    }
  }

  test("Get me a little later!") {
    val x = future { blocking(Thread.sleep(3000L)); "doneX" }
    val f = FutureOps(x)

    blocking(Thread.sleep(4000L));
    assert(f.now == "doneX")
  }

  /*test("let us continueWith it...") {
    def cont(in: Future[String]): Int = {
      val inS = Await.result(in, 1 second)
      println("inS = " + inS)

      val retInt = inS match {
        //case "doneX" => 10
        case _ => 20
      }
      println("return int = " + retInt)

      /*async {
        val ssss = await { in }
      }*/

      retInt
    }

    val x = future { /*blocking(Thread.sleep(1000L));*/ "doneX" }

    val f = FutureOps(x)
    val s = f.continueWith(cont)
    assert(Await.result(s, 2 seconds) == 10)
  }*/

  test("CancellationTokenSource should allow stopping the computation") {
    val cts = CancellationTokenSource()
    val ct = cts.cancellationToken
    //println("isCancelled = " + ct.isCancelled + " nonCancelled = " + ct.nonCancelled)
    val p = Promise[String]()

    async {
      while (ct.nonCancelled) {
        // do work

        //println("working hard...")
        //blocking(Thread.sleep(100L))
      }

      p.success("done")
    }

    //blocking(Thread.sleep(1000L))
    cts.unsubscribe()
    assert(Await.result(p.future, 1 second) == "done")
  }

  test("future run with cancellation token") {
    val working = Future.run() { ct =>
      async {
        while (ct.nonCancelled) {
          //println("working")
          blocking(Thread.sleep(500L))
        }
        //println("done")
      }
    }
    Future.delay(5 seconds) onSuccess {
      case _ => working.unsubscribe()
    }
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////
  /////// Part 2: An Asynchronous HTTP Server

  class DummyExchange(val request: Request) extends Exchange {
    @volatile var response = ""
    val loaded = Promise[String]()
    def write(s: String) {
      response += s
    }
    def close() {
      loaded.success(response)
    }
  }

  class DummyListener(val port: Int, val relativePath: String) extends NodeScala.Listener {
    self =>

    @volatile private var started = false
    var handler: Exchange => Unit = null

    def createContext(h: Exchange => Unit) = this.synchronized {
      assert(started, "is server started?")
      handler = h
    }

    def removeContext() = this.synchronized {
      assert(started, "is server started?")
      handler = null
    }

    def start() = self.synchronized {
      started = true
      new Subscription {
        def unsubscribe() = self.synchronized {
          started = false
        }
      }
    }

    def emit(req: Request) = {
      val exchange = new DummyExchange(req)
      if (handler != null) handler(exchange)
      exchange
    }
}

  class DummyServer(val port: Int) extends NodeScala {
    self =>
    val listeners = mutable.Map[String, DummyListener]()

    def createListener(relativePath: String) = {
      val l = new DummyListener(port, relativePath)
      listeners(relativePath) = l
      l
    }

    def emit(relativePath: String, req: Request) = this.synchronized {
      val l = listeners(relativePath)
      l.emit(req)
    }
  }

  test("Listener should serve the next request as a future") {
    val dummy = new DummyListener(8191, "/test")
    val subscription = dummy.start()

    def test(req: Request) {
      val f = dummy.nextRequest()
      dummy.emit(req)
      val (reqReturned, xchg) = Await.result(f, 1 second)

      assert(reqReturned == req)
    }

    test(immutable.Map("StrangeHeader" -> List("StrangeValue1")))
    test(immutable.Map("StrangeHeader" -> List("StrangeValue2")))

    subscription.unsubscribe()
  }

  test("Server should serve requests") {
    val dummy = new DummyServer(8191)
    val dummySubscription = dummy.start("/testDir") {
      request => for (kv <- request.iterator) yield (kv + "\n").toString
    }

    // wait until server is really installed
    Thread.sleep(2000)

    def test(req: Request) {
      val webpage = dummy.emit("/testDir", req)
      val content = Await.result(webpage.loaded.future, 3 second)
      val expected = (for (kv <- req.iterator) yield (kv + "\n").toString).mkString
      //println("content = " + content)
      //println("expected = " + expected)
      assert(content == expected, s"'$content' vs. '$expected'")
    }

    test(immutable.Map("StrangeRequest" -> List("Does it work?")))
    test(immutable.Map("StrangeRequest" -> List("It works!")))
    test(immutable.Map("WorksForThree" -> List("Always works. Trust me.")))

    dummySubscription.unsubscribe()
  }

}




