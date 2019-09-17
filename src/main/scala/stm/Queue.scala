package stm

import scala.collection.immutable.{Queue => ScalaQueue}

object Queue {
  import scalaz.zio._

  // task: model a bounded queue with backpressure on:
  // - offering to a queue that is already full
  // - taking from a queue that is empty
  case class Queue[A](capacity: Int, tref: stm.TRef[ScalaQueue[A]])

  def makeQueue[A](capacity: Int): UIO[Queue[A]] =
    stm.TRef.make(ScalaQueue.empty[A]).commit.map(Queue(capacity, _))

  def offer[A](queue: Queue[A], a: A): UIO[Unit] =
    (for {
      q <- queue.tref.get
      _ <- stm.STM.check(q.length < queue.capacity)
      _ <- queue.tref.update(_ enqueue a)
    } yield ()).commit

  def take[A](queue: Queue[A]): UIO[A] =
    (for {
      q <- queue.tref.get
      a <- q.dequeueOption match {
             case Some((a, as)) =>
               queue.tref.set(as) *> stm.STM.succeed(a)
             case None    => stm.STM.retry
      }
    } yield a).commit
}