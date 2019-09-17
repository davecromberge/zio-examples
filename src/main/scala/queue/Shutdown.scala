package queue

import scalaz.zio._

// Clean shutdown - lifetime of a queue is finite,
// and can prevent producers and consumers from
// interacting with queue once it has been shutdown.
// This happens via interruption
object Shutdown {
  def loop =
    for {
      queue  <- Queue.unbounded[String]
      fiber1 <- queue.offer("Give me coffee").forever.fork
      fiber2 <- queue.take.forever.fork
      _      <- queue.shutdown
    } yield ()
}