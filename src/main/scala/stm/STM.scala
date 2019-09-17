package stm

// STM[E, A] models a transaction that reads or writes TRefs; it can succeed, retry or fail.
// A TRef[A] models a transactional reference, which is read or written by an STM.
// STM allows transactions to be nested due to its support for composability
object STM {
  import scalaz.zio._

  // succeed creates a successful transaction with the specified value
  val hello: stm.STM[Nothing, String] = stm.STM.succeed("Welcome to Scalaz")

  // use flatMap and map to compose two transactions together
  val balance1: stm.STM[Nothing, Int] = stm.STM.succeed(1)
  val balance2: stm.STM[Nothing, Int] = stm.STM.succeed(1)
  val composed: stm.STM[Nothing, Int] = for {
    b1 <- balance1
    b2 <- balance2
  } yield b1 + b2
  
  // two transactions can also be combined together with `zip`
  val composed2: stm.STM[Nothing, (Int, Int)] = balance1 zip balance2

  // two transactions can also be combined together with `orElse`
  val composed3: stm.STM[Nothing, Int] = balance1 orElse balance2

  // failure transactions will rollback and undo the effect of the transaction
  val debit = for {
    amount0 <- stm.TRef.make(1)
    amount1 <- amount0.update(_ + 1)
    result  <- if (amount1 < 2) stm.STM.fail("Balance too low, failed")
               else stm.STM.succeed(Unit)
  } yield amount1

  // to recover from errors, we can fold on an STM.
  debit.fold(err => s"Failed: $err", i => s"Success, balance is $i")
  // foldM, allows us to run a transaction both in the error and success cases
  debit.foldM(err => stm.STM.fail(s"Failed: $err"), i => stm.STM.succeed(s"Success, balance is $i"))

  // STM can be retried depending on the contents of a TRef
  val booking = for {
    seat   <- stm.TRef.make(Some("1A"): Option[String])
    number <- seat.get
    ticket <- number match {
                case Some(n) => stm.STM.succeed(n)
                case None => stm.STM.retry
              }
  } yield ticket

  // `check` suspends a transaction until some predicate is satisfied
  val desired = 2
  val awaitBooking = for {
    available    <- stm.TRef.make(1)
    numAvailable <- available.get
    _            <- stm.STM.check(numAvailable > desired)
    _            <- available.modify(x => (x - desired, desired))
  } yield ()

  // `filter` lets us retrieve the value of the transaction and check in a single step
  val awaitBooking2 = for {
    available    <- stm.TRef.make(1)
    _            <- available.get.filter(_ > desired)
    _            <- available.modify(x => (x - desired, desired))
  } yield ()
}