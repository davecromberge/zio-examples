package stm

object Semaphore {
  import scalaz.zio._

  // A semaphore controls access to some limited resource, by
  // the release and acquisition of permits.
  // The following semaphore doesn't leak resources, and is
  // able to function correctly in the face of exceptions.
  type Semaphore = stm.TRef[Int]

  def makeSemaphore(initial: Int): UIO[Semaphore] =
    stm.TRef.make(initial).commit

  def acquire(semaphore: Semaphore, permits: Int): UIO[Unit] =
    (for {
      value <- semaphore.get
      _     <- stm.STM.check(value > permits)
      _     <- semaphore.set(value - permits)
    } yield ()).commit

  def release(semaphore: Semaphore, permits: Int): UIO[Int] =
    semaphore.update(_ + permits).commit
}