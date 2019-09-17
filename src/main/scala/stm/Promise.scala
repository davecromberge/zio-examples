package stm

object Promise {
  import scalaz.zio._

  // A promise is an asynchronous variable that is
  // set exactly once.  A number of other threads
  // can wait on the setting of the value, and when
  // it is set, they continue.  It is called `Deferred` in
  // cats effect.
  type Promise[A] = stm.TRef[Option[A]]

  def makePromise[A]: UIO[Promise[A]] =
    stm.TRef.make(Option.empty[A]).commit

  def complete[A](promise: Promise[A], a: A): UIO[Boolean] =
    (for {
      value  <- promise.get
      change <- value match { 
                  case Some(_) => stm.STM.succeed(false)
                  case None    => promise.set(Some(a)) *> stm.STM.succeed(true)
                }
    } yield change).commit

  // if collect does not match, it will try again until the TRef is
  // modified.  This will allow the waiters to extract the value
  def await[A](promise: Promise[A]): UIO[A] =
    promise.get.collect { case Some(a) => a }.commit
}