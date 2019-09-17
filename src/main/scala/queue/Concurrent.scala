package queue

import scalaz.zio._

object Concurrent {
  val runtime = new DefaultRuntime {}

  def putStrLine(s: String): Task[Unit] =
    IO(println(s))

  def loop: Task[Unit] =
    for {
      queue <- Queue.unbounded[String]
      // fork creates a new fiber which is a lightweight version of a thread with it's own context and stack,
      // that can be interrupted at any time.
      _     <- queue.offer("Make me a coffee").forever.fork
      _     <- queue.take.flatMap(putStrLine).forever.fork
    } yield ()

  def unsafeRun: Unit =
    runtime.unsafeRun(loop)
}


