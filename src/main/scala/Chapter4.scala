object Chapter4 {

    object Exercise_4_3 {
        def Try[A](a: => A) : Option[A] = {
            try Some(a)
            catch {case e : Exception => None }
        }

        def lift[A, B](f: A => B) : Option[A] => Option[B] = _ map f
        def lift2[A, B, C](f: (A, B) => C): (Option[A], Option[B]) => Option[C] = {
            (a, b) => a.flatMap(x => b.map(y => f(x , y)))
        }
        def lift3[A, B, C, D](f: (A, B, C) => D): (Option[A], Option[B], Option[C]) => Option[D] = {
            (a, b, c) => {
                for {
                    x <- a
                    y <- b
                    z <- c
                    res = f(x, y, z)
                } yield res
            }
        }
        
        def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C) : Option[C] = {
            a.flatMap(x => b.map(y => f(x, y)))
        }
        def map2Lift[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C) : Option[C] = {
            lift2(f)(a, b)
        }

        def insureanceRateQoute(age: Int, tickets: Int) : Double = ???

        def parseInsuranceRateQoute(age: String, numberOfSpeedingTickets: String) : Option[Double] = {
            val optAge : Option[Int] = Try{ age.toInt }
            val optTickets : Option[Int] = Try { numberOfSpeedingTickets.toInt }
            map2(optAge, optTickets)(insureanceRateQoute)
        }
    }

    object Exercise_4_4 {

        def sequence[A](a: List[Option[A]]) : Option[List[A]] = {
            val init : Option[List[A]] = Some(List())
            a.foldLeft(init)((acc : Option[List[A]], x : Option[A]) => {
                for {
                    current <- acc
                    item <- x
                } yield item :: current
            })
        }

        def traverse[A, B](a: List[A])(f: A => Option[B]) : Option[List[B]] = {
          val init : Option[List[B]] = Some(List())
          a.foldLeft(init)((acc : Option[List[B]], x : A) => {
            for {
              current <- acc
              item <- f(x)
            } yield item :: current
          })
        }

      def sequenceFromTraverse[A](a: List[Option[A]]) : Option[List[A]] = {
        traverse(a)(Predef.identity)
      }
    }
}
