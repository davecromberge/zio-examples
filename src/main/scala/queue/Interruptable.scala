package queue

import scalaz.zio._

// Fibers can be interrupted without having any explicit
// interruption logic, and without leaking resources.
// Leads to compositional designs.
object Interruptable {
  def putStrLine(s: String): Task[Unit] = 
    IO(println(s))

  def loop =
    for {
      queue <- Queue.unbounded[String]
      fiber <- queue.offer("Make coffee").forever.fork
      _     <- fiber.interrupt
    } yield ()

  lazy val runtime = new DefaultRuntime {}

  def unsafeRun = runtime.unsafeRun(loop)
}