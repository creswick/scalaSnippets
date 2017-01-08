import cats._
import cats.data._
import cats.implicits._

/**
  * Just a namespace for functions.
  */
object Sample {
  trait Error { def msg: String }
  case class KeyNotFoundError(msg: String) extends Error
  case class DuplicateKeyError(msg: String) extends Error


  /** Update a mapping iff the key already exists      */
  def update[K,V](theMap: Map[K,V], key: K, value: V): Either[KeyNotFoundError, Map[K,V]] =
    if (theMap.contains(key)) {
      Either.right(theMap + (key -> value))
    } else {
      Either.left(KeyNotFoundError(s"Key: '$key' does not exist."))
    }


  /** Add a mapping iff the key does not already exist */
  def addIfUnique[K,V](theMap: Map[K,V], key: K, value: V): Either[DuplicateKeyError, Map[K,V]] =
    if (theMap.contains(key)) {
      Either.left(DuplicateKeyError(s"Key: '$key' already exists."))
    } else {
      Either.right(theMap + (key -> value))
    }

  sealed trait TTask
  case class ToDo() extends TTask
  case class Done() extends TTask

  case class MyState(tasks: Map[String, TTask])

  val emptyList = MyState(Map.empty)

  def session(): State[MyState, Either[Error, Unit]] = for {
    _ <- Sample.add("item 1")
    _ <- Sample.add("item 2")
    _ <- Sample.add("item 3")
    _ <- Sample.list()
    _ <- Sample.complete("item 3")
    _ <- Sample.complete("item 4")
    res <- Sample.list()
  } yield Right(res)


  /** create task, save to state. Return number of tasks after addition. Error if duplicate. */
  def add(desc: String): State[MyState, Either[Error, Int]] =
    for{
      theTTasks <- State.inspect[MyState, Map[String, TTask]](_.tasks)
      // Does not compile:
      // res <- addIfUnique(theTTasks, desc, ToDo()) match {
      //   case Left(err) => State.pure(Left(err))

      //   case Right(newTTasks) => for {
      //     _ <- State.modify[MyState](_.copy(tasks = newTTasks))
      //     count <- State.inspect[MyState, Int](_.tasks.size)
      //   } yield Right(count)
      // }
      res = Right(5)
    } yield res

  /** change task to 'Done(desc)' if exists, return number of tasks after addition,
    * error if no task exists, or if task is already done. */
  def complete(desc: String): State[MyState, Either[Error, Int]] =
    for{
      _ <- State.modify[MyState](st => st.copy(tasks = st.tasks + ((desc, Done()))))
      count <- State.inspect[MyState, Int](_.tasks.size)
    } yield Either.right(count)

  /** List the tasks, in no particular order. */
  def list(): State[MyState, Unit] = for {
    ts <- State.inspect[MyState, Map[String, TTask]](_.tasks)
  } yield ts.foreach { case (k, v) => println(s"$k: $v")}
}
