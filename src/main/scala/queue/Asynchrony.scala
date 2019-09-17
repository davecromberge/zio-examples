package queue

import scalaz.zio._

// 10k fibers do not consume 10k different threads
// If nothing is in the queue, then no cpu resources
// are used
object Asynchrony {
  def putStrLn(s: String): Task[Unit] =
    IO(println(s))

  def loop =
    for {
      queue      <- Queue.unbounded[String]
      worker     = queue.take.flatMap(putStrLn).forever
      workers10k = List.fill(10000)(worker)
      _          <- IO.forkAll(workers10k)
      _          <- queue.offer("More coffee!").forever.fork
    } yield ()
}