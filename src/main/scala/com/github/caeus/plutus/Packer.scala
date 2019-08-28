package com.github.caeus.plutus

import scala.collection.SeqOps

import PackerResult.{Done, Failed}

//I like this structure, but something is off...
/*
* A result has only a value and tells me SOMEHOW, how the input was affected!
* How? A result obviously holds the value, and also a transformation on the original input.
* How?
*
* */

trait PackerResult[In, +Out] extends {
  def isDone: Boolean

  def value: Out

  def taken: Int

  final def map[Out1](func: Out => Out1): PackerResult[In, Out1] = this match {
    case Done(value, taken) =>
      Done(func(value), taken)
    case f => f.asInstanceOf[PackerResult[In, Out1]]
  }
}

object PackerResult {
  implicit def ioPlutusOrderingInstance[In, Out]: Ordering[PackerResult[In, Out]] = new Ordering[PackerResult[In, Out]] {
    override def compare(
                          x: PackerResult[In, Out],
                          y: PackerResult[In, Out]
                        ): Int = {
      val compare1 = x.isDone.compareTo(y.isDone)
      if (compare1 != 0)
        return compare1
      x.taken.compareTo(y.taken)
    }
  }

  case class Failed[In, +Out](
                               msg: String,
                               taken: Int
                             ) extends PackerResult[In, Out] {
    override def isDone: Boolean = false

    override def value: Out =
      throw new NoSuchElementException(msg)
  }

  case class Done[In, +Out](
                             value: Out,
                             taken: Int
                           ) extends PackerResult[In, Out] {
    override def isDone: Boolean = true
  }

  //Sorry byt later case class Cont[In, Out](next: Tarser[In, Out]) extends TarserResult[In, Out]
  //Adding this makes it very difficult to add the forking ability
}

trait Packer[In, +Out] {
  self =>
  def take(input: List[In]): PackerResult[In, Out]

  final def apply(input: List[In]): PackerResult[In, Out] = take(input)

  final def transform[Out1](func: PackerResult[In, Out] => PackerResult[In, Out1]): Packer[In, Out1] =
    (input: List[In]) => func(self.take(input))

  final def flatMap[Out1](func: Out => Packer[In, Out1]): Packer[In, Out1] =
    (input: List[In]) => self.take(input) match {
      case Done(value, taken) =>
        func(value).take(input.drop(taken)) match {
          case Done(newVal, moreTaken) => Done(newVal, taken + moreTaken)
          case f => f.asInstanceOf[PackerResult[In, Out1]] //TODO make the taken of this shit increase!
        }
      case f => f.asInstanceOf[PackerResult[In, Out1]]
    }

  final def map[Out1](func: Out => Out1): Packer[In, Out1] = flatMap(out => Packer.pure(func(out)))
}

object Packer {

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


  def capture[In, Out](value: Packer[In, Out]): Packer[In, List[In]] = {
    input: List[In] =>
      value.take(input) match {
        case Done(_, taken) =>
          Done(input.take(taken), taken)
        case f => f.asInstanceOf[PackerResult[In, List[In]]]
      }
  }

  def fromPredicate[In](predicate: In => Boolean): Packer[In, Unit] =
    (input: List[In]) => input match {
      case head :: tail if predicate(head) => Done((), 1)
      case head :: _ => Failed(s"token `$head` didn't satisfy the predicate $predicate", 1)
      case _ => Failed(s"Expected one token, EOI gotten instead", 0)
    }


  def repeatUpTo[In, Out](
                           packer: Packer[In, Out],
                           upTo: Option[Int],
                           sep: Packer[In, _]
                         ): Packer[In, List[Out]] = {
    def recursive(upTo: Option[Int], accum: List[Out]): Packer[In, List[Out]] = {
      upTo.map(_.compareTo(0)).getOrElse(1) match {
        case -1 => throw new IllegalArgumentException("Repeat quantity must be greater than 0")
        case 0 => pure(accum)
        case 1 =>
          val sepPacker =
            longestOf[In, Option[Out]](
              pure(None),
              (if (accum.nonEmpty)
                sep.flatMap(_ => packer)
              else
                packer).map(Some(_)))
          sepPacker.flatMap {
            case None =>
              pure(accum)
            case Some(value) =>
              recursive(upTo.map(_ - 1), value :: accum)
          }
      }
    }

    recursive(upTo, Nil).map(_.reverse)
  }

  def repeatExactly[In, Out](
                              packer: Packer[In, Out],
                              exactly: Int,
                              sep: Packer[In, _]
                            ): Packer[In, List[Out]] = {
    def recursive(exactly: Int, accum: List[Out]): Packer[In, List[Out]] = {
      exactly.compareTo(0) match {
        case -1 => throw new IllegalArgumentException("Repeat quantity must be greater than 0")
        case 0 => pure(accum)
        case 1 =>
          val sepPacker = if (accum.isEmpty) packer else sep.flatMap(_ => packer)
          sepPacker.flatMap {
            value =>
              recursive(exactly - 1, value :: accum)
          }
      }
    }

    recursive(exactly, Nil).map(_.reverse)
  }

  def pure[In, Out](value: Out): Packer[In, Out] = new PurePacker[In, Out](value)

  private class FromListPacker[In](from: List[In]) extends Packer[In, Unit] {
    def reqTake(
                 input: List[In],
                 recFrom: List[In],
                 recInput: List[In],
                 taken: Int
               ): PackerResult[In, Unit] = {
      (recFrom, recInput) match {
        case (h1 :: t1, h2 :: t2) if h1 == h2 =>
          reqTake(input, t1, t2, taken + 1)
        case (_ :: _, _) =>
          Failed(s"$recFrom expected. Got ${recInput.take(5)} instead", taken)
        case (Nil, _) =>
          Done((), taken)
      }
    }

    override def take(input: List[In]): PackerResult[In, Unit] = {
      reqTake(input, from, input, 0)
    }

  }

  def fromList[In](from: List[In]): Packer[In, Unit] = new FromListPacker[In](from)


  def longestOf[In, Out](
                          alternatives: Packer[In, Out]*,
                        ): Packer[In, Out] = new LongestOfPacker[In,Out](alternatives.toSeq)

  private class LongestOfPacker[In, Out](
                                          alternatives: Seq[Packer[In, Out]]
                                        ) extends Packer[In, Out] {
    override def take(input: List[In]): PackerResult[In, Out] = {
      alternatives.map(_.take(input)).max
    }
  }

  private class PurePacker[In, Out](value: Out) extends Packer[In, Out] {
    override def take(nput: List[In]): PackerResult[In, Out] = {
      Done(value, 0)
    }
  }

}