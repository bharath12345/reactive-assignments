package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("del1") = forAll { a: Int =>
    val h1 = insert(a, empty)
    val h2 = deleteMin(h1)
    isEmpty(h2)
  }

  property("meld1") = forAll { (h1: H, h2: H) =>
    val mina = findMin(h1)
    val minb = findMin(h2)
    //println("mina = " + mina + " minb = " + minb)

    val h = meld(h1, h2)
    val z = findMin(h)

    if (mina < minb)
      z == mina
    else
      z == minb
  }

  property("order1") = forAll { (h: H) =>
    if (!isEmpty(h)) {
      val a = findMin(h)
      val h1 = deleteMin(h)
      if (!isEmpty(h1)) {
        val b = findMin(h1)
        (a <= b)
      } else
        true
    } else
      true
  }

  property("knownorder") = forAll(Gen.choose(1, 10), Gen.choose(11, 20), Gen.choose(21, 30)) {
    (x: Int, y: Int, z: Int) =>
      val h1 = insert(x, empty)
      val h2 = insert(y, h1)
      val h3 = insert(z, h2)

      if (findMin(h3) != x) {
        false
      } else {
        val nh1 = deleteMin(h3)
        if (findMin(nh1) != y) {
          false
        } else {
          val nh2 = deleteMin(nh1)
          if (findMin(nh2) != z)
            false
          else
            true
        }
      }
  }

  lazy val genHeap: Gen[H] = for {
    k <- arbitrary[Int]
    m <- oneOf(value(empty), genHeap)
  } yield insert(k, m)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
