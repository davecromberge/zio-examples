package queue

import scalaz.zio._

case class Actor[+E, -A, +B](run: A => IO[E, B]) { self =>
  def ! (a: A): IO[E, B] = run(a)
}