package shipreq.utils

import cats.effect._
import cats.implicits._
import japgolly.microlibs.stdlib_ext.ParseInt

// Takes ~30 min
object GenerateSampleData extends IOApp {

  override def run(argsL: List[String]): IO[ExitCode] = {
    val args = argsL.toVector

    val getRounds: IO[Int] =
      args.lift(0) match {
        case Some(ParseInt(i)) => IO.pure(i)
        case Some(x)           => IO.raiseError(new RuntimeException("Invalid arg: " + x))
        case None              => IO.pure(1)
      }

    def thunks(rounds: Int): List[IO[Unit]] =
      for {
        size <- List(10000, 4000, 2000, 1000)
        _    <- (1 to rounds).toList
        typ  <- List("full", "no_req_codes")
      } yield IO(GenerateEvents.main(s"$size $typ".split(' ')))

    for {
      r <- getRounds
      t <- IO.pure(thunks(r))
      _ <- IO(println(s"Generating ${t.size} files..."))
      _ <- t.parSequence_
    } yield ExitCode.Success
  }
}
