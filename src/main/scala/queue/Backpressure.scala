package queue

import scalaz.zio._

// Producers can produce what they want, and this
// composes as there is no special logic in a 
// producer to slow down, as well as communicate
// this back upstream.  They are also suspended.
object Backpressure {

  def putStrLn(s: String): Task[Unit] =
    IO(println(s))

  def loop =
  for {
    queue      <- Queue.bounded[String](10)
    worker     = queue.offer("More coffee").forever
    workers10k = List.fill(10000)(worker)
    _          <- IO.forkAll(workers10k)
    _          <- queue.take.flatMap(putStrLn).forever.fork
  } yield ()
}