/**
 * Copyright (C) 2009-2013 Typesafe Inc. <http://www.typesafe.com>
 */
package actorbintree

import akka.actor._
import scala.collection.immutable.Queue
import akka.event.LoggingReceive

object BinaryTreeSet {

  trait Operation {
    def requester: ActorRef
    def id: Int
    def elem: Int
  }

  trait OperationReply {
    def id: Int
  }
  
  /** Message to signal successful completion of an insert or remove operation. */
  case class OperationFinished(id: Int) extends OperationReply

  ////////////////

  /** Request with identifier `id` to insert an element `elem` into the tree.
    * The actor at reference `requester` should be notified when this operation
    * is completed.
    */
  case class Insert(requester: ActorRef, id: Int, elem: Int) extends Operation 

  /** Request with identifier `id` to check whether an element `elem` is present
    * in the tree. The actor at reference `requester` should be notified when
    * this operation is completed.
    */
  case class Contains(requester: ActorRef, id: Int, elem: Int) extends Operation 
  
  /** Request with identifier `id` to remove the element `elem` from the tree.
    * The actor at reference `requester` should be notified when this operation
    * is completed.
    */
  case class Remove(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request to perform garbage collection*/
  case object GC

  /** Holds the answer to the Contains request with identifier `id`.
    * `result` is true if and only if the element is present in the tree.
    */
  case class ContainsResult(id: Int, result: Boolean) extends OperationReply
  
  //////////
  
  private case class Job(client: ActorRef, elem: Int)
  
  
}


class BinaryTreeSet extends Actor {
  import BinaryTreeSet._
  import BinaryTreeNode._

  def createRoot: ActorRef = context.actorOf(BinaryTreeNode.props(0, initiallyRemoved = true))

  var root = createRoot

  var pendingQueue = Queue.empty[Operation]

  def receive = normal

  /** Accepts `Operation` and `GC` messages. */
  val normal: Receive = LoggingReceive { 
    case Insert(requester: ActorRef, id: Int, elem: Int) => {
      println(s"Received Inserts message id = $id, elem = $elem")
      root ! Insert(requester, id, elem)
      context.become(waitForInsertToFinish(requester))
    }
    
    case Contains(requester: ActorRef, id: Int, elem: Int) => {
      println(s"Received Contains message id = $id, elem = $elem")
      root ! Contains(requester, id, elem)
      context.become(waitForContainsToFinish(requester))
    }
    
    case Remove(requester: ActorRef, id: Int, elem: Int) => {
      
    }
    
    case ContainsResult(id: Int, result: Boolean) => {
      
      
    }
    
    case OperationFinished(id: Int) => {
      
    }
    
    
  }
  
  def waitForInsertToFinish(requester: ActorRef): Receive = LoggingReceive {
    case OperationFinished(id: Int) => {
      requester ! OperationFinished(id)
      context.unbecome()
    }   
  }
  
  def waitForContainsToFinish(requester: ActorRef): Receive = LoggingReceive {
    case ContainsResult(id: Int, result: Boolean) => {
      requester ! ContainsResult(id, result)
      context.unbecome()
    }
  }

  /** Handles messages while garbage collection is performed.
    * `newRoot` is the root of the new binary tree where we want to copy
    * all non-removed elements into.
    */
  def garbageCollecting(newRoot: ActorRef): Receive = ???

}

////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////


object BinaryTreeNode {
  trait Position

  case object Left extends Position
  case object Right extends Position

  case class CopyTo(treeNode: ActorRef)
  case object CopyFinished

  def props(elem: Int, initiallyRemoved: Boolean) = Props(classOf[BinaryTreeNode],  elem, initiallyRemoved)
}

class BinaryTreeNode(val elem: Int, initiallyRemoved: Boolean) extends Actor {
  import BinaryTreeNode._
  import BinaryTreeSet._

  var subtrees = Map[Position, ActorRef]()
  var removed = initiallyRemoved

  def receive = normal

  /** Handles `Operation` messages and `CopyTo` requests. */
  val normal: Receive = LoggingReceive { 
    case CopyTo(treeNode: ActorRef) => ???
    
    case Insert(requester: ActorRef, id: Int, e: Int) => {
      if(elem == e) {
        sender ! OperationFinished(id)
        println(s"element $e already in the tree")
      } else {
        if(e < elem) {
          if(subtrees.contains(Left)) {
            println(s"going to the left of $elem")
            val leftTreeNode = subtrees(Left)
            leftTreeNode ! Insert(requester, id, e)
          } else {
            // no left subtree... so, create 
            val newLeftNode = context.actorOf(BinaryTreeNode.props(e, false), name = s"leftnode-$e-$id" )
            subtrees += (Left -> newLeftNode)
            sender ! OperationFinished(id)
            println(s"added $e to the left of $elem")
          }
        } else {
          if(subtrees.contains(Right)) {
            val rightTreeNode = subtrees(Right)
            rightTreeNode ! Insert(requester, id, e)
            println(s"going to the right of $elem")
          } else {
            // no right subtree... so, create 
            val newRightNode = context.actorOf(BinaryTreeNode.props(e, false), name = s"rightnode-$e-$id")
            subtrees += (Right -> newRightNode)
            sender ! OperationFinished(id)
            println(s"added $e to the right of $elem")
          }  
        }
      }
    }
    
    case Contains(requester: ActorRef, id: Int, e: Int) => {
      if(elem == e) {
        sender ! ContainsResult(id, true)
      } else {
        if(e < elem) {
          // go left
          if(subtrees.contains(Left)) {
            // search in subtree
            val leftTreeNode = subtrees(Left)  
            leftTreeNode ! Contains(requester, id, e)
          } else {
            // no left subtree... send a not found message back
            sender ! ContainsResult(id, false)
          }
        } else {
          // go right
          if(subtrees.contains(Right)) {
            val rightTreeNode = subtrees(Right)
            rightTreeNode ! Contains(requester, id, e)
          } else {
            sender ! ContainsResult(id, false)
          }
        }  
      }
    }
  }

  /** `expected` is the set of ActorRefs whose replies we are waiting for,
    * `insertConfirmed` tracks whether the copy of this node to the new tree has been confirmed.
    */
  def copying(expected: Set[ActorRef], insertConfirmed: Boolean): Receive = ???

}
