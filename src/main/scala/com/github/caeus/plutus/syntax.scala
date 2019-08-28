package com.github.caeus.plutus

import com.github.caeus.plutus.Packer.{capture, fromList, fromPredicate, longestOf, pure, repeatExactly, repeatUpTo}
import com.github.caeus.plutus.PackerResult.{Done, Failed}

import scala.collection.SeqOps

object syntax {

  object P {
    @inline
    def apply[In, Out](packer: Packer[In, Out]): Packer[In, Out] = packer

    def apply[In, Repr](seq: SeqOps[In, Seq, Repr]): Packer[In, Unit] = fromList(seq.toList)

    def apply[In](pred: In => Boolean): Packer[In, Unit] = fromPredicate(pred)
  }

  implicit class PredicateOps[In](val value: In => Boolean) extends AnyVal {
    def &&(other: In => Boolean): In => Boolean = { char =>
      value(char) && other(char)
    }

    def ||(other: In => Boolean): In => Boolean = { char =>
      value(char) || other(char)
    }

    def unary_! : In => Boolean = { char =>
      !value(char)
    }
  }


  implicit class PackerOps[In, Out](val value: Packer[In, Out]) extends AnyVal {

    def ~[Out1](next: => Packer[In, Out1]): Packer[In, Unit] = value.flatMap(_ => next.map(
      _ => ()))

    def ~>[Out1](next: => Packer[In, Out1]): Packer[In, Out1] = value.flatMap(_ => next)

    def <~>[Out1](next: => Packer[In, Out1]): Packer[In, (Out, Out1)] = value.flatMap(a => next.map(
      b => a -> b))

    def <~[Out1](next: => Packer[In, Out1]): Packer[In, Out] = value.flatMap(a => next.map(
      _ => a))

    def ? : Packer[In, Option[Out]] = repeatUpTo(value, Some(1), pure(())).map(_.headOption)

    def log(name: String): Packer[In, Out] = new Packer[In, Out] {
      override def take(input: List[In]): PackerResult[In, Out] = {
        println(s"packer $name will take ${input.take(5).mkString("([", "][", "]...")} ${input.size}")
        val result = value.take(input)
        result match {
          case Done(value, taken) =>
            println(s"packer $name returned with value $value and took $taken tokens leaving a remaining ${input.slice(taken, taken + 5).mkString("([", "][", "]...")}")
          case Failed(m, taken) =>
            println(s"packer $name failed for input ${input.take(5).mkString("([", "][", "]...")}... $taken ahead with message: $m")
        }
        result
      }
    }

    def rep(
             min: Int = 0,
             max: Option[Int] = None,
             sep: Packer[In, _] = pure(())
           ): Packer[In, List[Out]] = {
      if (min > 0)
        for {
          prefix <- repeatExactly(value, min, sep)
          suffix <- repeatUpTo(sep.flatMap(_ => value), max, pure(()))
        } yield prefix ::: suffix
      else repeatUpTo(value, max, sep)
    }

    def rep: Packer[In, List[Out]] = {
      repeatUpTo(value, None, pure(()))
    }

    def ! : Packer[In, List[In]] = capture(value)

    def |[NewOut >: Out](other: => Packer[In, NewOut]): Packer[In, NewOut] = {
      longestOf(value, other)
    }
  }

}