object `Chapter 4` {

  object `Exercise 4.3` {
    def Try[A](a: => A): Option[A] = {
      try Some(a)
      catch { case e: Exception => None }
    }

    def lift[A, B](f:     A      => B): Option[A] => Option[B] = _ map f
    def lift2[A, B, C](f: (A, B) => C): (Option[A], Option[B]) => Option[C] = { (a, b) =>
      a.flatMap(x => b.map(y => f(x, y)))
    }
    def lift3[A, B, C, D](f: (A, B, C) => D): (Option[A], Option[B], Option[C]) => Option[D] = { (a, b, c) =>
      {
        for {
          x <- a
          y <- b
          z <- c
          res = f(x, y, z)
        } yield res
      }
    }

    def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
      a.flatMap(x => b.map(y => f(x, y)))
    }
    def map2Lift[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
      lift2(f)(a, b)
    }

    def insureanceRateQoute(age: Int, tickets: Int): Double = ???

    def parseInsuranceRateQoute(age: String, numberOfSpeedingTickets: String): Option[Double] = {
      val optAge:     Option[Int] = Try { age.toInt }
      val optTickets: Option[Int] = Try { numberOfSpeedingTickets.toInt }
      map2(optAge, optTickets)(insureanceRateQoute)
    }
  }

  object `Exercise 4.5 and 4.6` {

    def sequence[A](a: List[Option[A]]): Option[List[A]] = {
      val init: Option[List[A]] = Some(List())
      a.foldLeft(init)((acc: Option[List[A]], x: Option[A]) => {
        for {
          current <- acc
          item <- x
        } yield item :: current
      })
    }

    def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
      val init: Option[List[B]] = Some(List())
      a.foldLeft(init)((acc: Option[List[B]], x: A) => {
        for {
          current <- acc
          item <- f(x)
        } yield item :: current
      })
    }

    def sequenceFromTraverse[A](a: List[Option[A]]): Option[List[A]] = {
      traverse(a)(Predef.identity)
    }
  }

  object `Exercise 4.6 and 4.7` {
    trait EEither[+E, +A] {
      def map[B](f: A => B): EEither[E, B] = {
        this match {
          case Error(e)   => Error(e)
          case Success(a) => Success(f(a))
        }
      }

      def flatMap[EE >: E, B](f: A => EEither[EE, B]): EEither[EE, B] = {
        this match {
          case Error(e)   => Error(e)
          case Success(a) => f(a)
        }
      }

      def orElse[EE >: E, B >: A](b: EEither[EE, B]): EEither[EE, B] = {
        this match {
          case Error(_)   => b
          case Success(a) => Success(a)
        }
      }

      def map2[EE >: E, B, C](value: EEither[EE, B])(f: (A, B) => C): EEither[EE, C] = {
        this.flatMap(a => value.flatMap(b => Success(f(a, b))))
      }

      def sequence[E, A](es: List[EEither[E, A]]): EEither[E, List[A]] = {
        traverse(es)(Predef.identity)
      }

      def traverse[E, A, B](as: List[A])(f: A => EEither[E, B]): EEither[E, List[B]] = {
        val init: EEither[E, List[B]] = Success(List())
        as.foldRight(init)((item: A, acc: EEither[E, List[B]]) => {
          acc.flatMap(list => f(item).flatMap(it => Success(it :: list)))
        })
      }

      def traverseWithMap2[E, A, B](as: List[A])(f: A => EEither[E, B]): EEither[E, List[B]] = {
        as match {
          case Nil => Success(Nil)
          case h :: t => {
            val head = f(h)
            val tail = traverseWithMap2(t)(f)
            head.map2(tail)(_ :: _)
          }
        }
      }
    }
    case class Error[E, A](error:   E) extends EEither[E, A]
    case class Success[E, A](value: A) extends EEither[E, A]
  }

}
