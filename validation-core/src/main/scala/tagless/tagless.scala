package jto.validation
package v3.tagless

import shapeless.{::, HList, HNil, tag}
import shapeless.tag.@@

object types {
  type flip[F[_, _]] = { type λ[B, A] = F[A, B] }
}

case class Goal[A, B](value: A) {
  def trivial(implicit ev: A =:= (B :: HNil)): B = value.head
}

trait MkLazy[K[_, _]] {
  def apply[A, B](k: => K[A, B]): K[A, B]
}

trait Merge[K[_, _], Out] {
  def merge[A, B <: HList](fa: K[Out, A], fb: K[Out, B]): K[Out, A :: B]
}

case class MergeOps[K[_, _], Out, B <: HList](fb: K[Out, B])(implicit M: Merge[K, Out]) {
  def ~:[A](fa: K[Out, A]): K[Out, A :: B] = M.merge(fa, fb)
}

trait Primitives[I, K[_, _]] {
  self: Grammar[I, K] =>

  type Out <: I // XXX: This is dubious
  type P <: Grammar[I, K] { type Out = self.Out }

  @inline private def camelToUnderscores(name: String) =
    "[A-Z]".r.replaceAllIn(name, { m =>
      "_" + m.group(0).toLowerCase()
    })

  @inline protected def mapKeyPath(f: String => String) =
     mapPath { p =>
      val ns =
        p.path.map {
          case KeyPathNode(n) => KeyPathNode(f(n))
          case i => i
        }
      Path(ns)
    }

  def underScoreCase = mapKeyPath(camelToUnderscores)

  def mapPath(f: Path => Path): P

  // TODO: Introduce NonEmptyPath
  // def at[A](p: Path)(k: => K[Option[_ >: Out <: I], A]): K[Out, A]
  def at(p: Path): At[K, Out, I]
  def knil: K[Out, HNil]

  def is[A](implicit K: K[_ >: Out <: I, A]): K[I, A]
  def req[A](implicit K: K[_ >: Out <: I, A]): K[Option[I], A]
  def opt[A](implicit K: K[_ >: Out <: I, A]): K[Option[I], Option[A]]

  def toGoal[Repr, A]: K[Out, Repr] => K[Out, Goal[Repr, A]]

  sealed trait Defered[A] {
    def apply[Repr](k: K[Out, Repr]): K[Out, Goal[Repr, A]] = toGoal(k)
  }

  def goal[A] = new Defered[A]{}

  implicit def int: K[I, Int] @@ Root
  implicit def string: K[I, String] @@ Root
  implicit def short: K[I, Short] @@ Root
  implicit def long: K[I, Long] @@ Root
  implicit def float: K[I, Float] @@ Root
  implicit def double: K[I, Double] @@ Root
  implicit def jBigDecimal: K[I, java.math.BigDecimal] @@ Root
  implicit def bigDecimal: K[I, BigDecimal] @@ Root
  implicit def boolean: K[I, Boolean] @@ Root

  // TODO: add non empty list
  implicit def seq[A](implicit k: K[_ >: Out <: I, A]): K[I, Seq[A]]
  implicit def list[A](implicit k: K[_ >: Out <: I, A]): K[I, List[A]]
  implicit def array[A: scala.reflect.ClassTag](implicit k: K[_ >: Out <: I, A]): K[I, Array[A]]
  implicit def map[A](implicit k: K[_ >: Out <: I, A]): K[I, Map[String, A]]
  implicit def traversable[A](implicit k: K[_ >: Out <: I, A]): K[I, Traversable[A]]

  import cats.arrow.Arrow

  def zip[A, B, C0, D](k1: K[Option[A], B], k2: K[Option[C0], D])(implicit A: Arrow[K]): K[Option[(A, C0)], (B, D)] = {
    val split = A.split(k1, k2)
    A.lmap(split){
      _.map{ case (a, b) =>
        (Option(a), Option(b))
      }.getOrElse((None, None))
    }
  }
}


trait LowPriorityTypeClasses[I, K[_, _]] {
  self: Typeclasses[I, K] with Primitives[I, K] =>
}

trait Typeclasses[I, K[_, _]] extends LowPriorityTypeClasses[I, K] {
  self: Primitives[I, K] =>
  import cats.arrow.Compose

  implicit def mkLazy: MkLazy[K]
  implicit def composeTC: Compose[K]
  implicit def semigroupTC[I0, O]: cats.Semigroup[K[I0, O] @@ Root]
  implicit def mergeTC: Merge[K, Out]
  implicit def mergeTCOpt: Merge[K, Option[Out]]
  implicit def toMergeOps[B <: HList, O: Merge[K, ?]](fb: K[O, B]): MergeOps[K, O, B] =
    MergeOps[K, O, B](fb)
}

trait Constraints[K[_, _]] {
  final type C[A] = K[A, A] @@ Root

  def min[A](a: A)(implicit O: Ordering[A]): C[A]
  def max[A](a: A)(implicit O: Ordering[A]): C[A]
  def notEmpty: C[String]
  def minLength(l: Int): C[String]
  def maxLength(l: Int): C[String]
  def pattern(regex: scala.util.matching.Regex): C[String]
  def email: C[String]
  // TODO: make is work for any S <: Seq
  def forall[I, O](k: K[I, O]): K[Seq[I], Seq[O]]
  def equalTo[A](a: A): C[A]
}

trait Grammar[I, K[_, _]]
  extends Primitives[I, K]
  with Typeclasses[I, K]
  with Constraints[K] {

  @inline final def coerce[B](k : K[Out, B]) : K[Init.OutF[I], B] =
    k.asInstanceOf[K[Init.OutF[I], B]]

  @inline final def coerceF[F[_], B](k : K[F[Out], B]) : K[F[Init.OutF[I]], B] =
    k.asInstanceOf[K[F[Init.OutF[I]], B]]
}

object Grammar {
  type Aux[I, K[_,_], O <: I] = Grammar[I,K] { type Out = O }
}

trait Init[I, A, B] {
  def ump[K[_,_]](g: Grammar[I, K]): K[A, B]

  @inline final def umpG[F[_], K[_,_]](g: Grammar[I, K])(implicit ev: A =:= F[Init.OutF[I]]) : K[F[g.Out], B] =
    ump[K](g).asInstanceOf[K[F[g.Out], B]]

  @inline final def coerce(implicit ev1: Init.OutF[I] <:< A, ev2 : A <:< I) : Init[I, I, B] =
    this.asInstanceOf[Init[I, I, B]]
}

object Init {
  final abstract class GrammarOut
  type OutF[I] = I with GrammarOut
}

final class InitFactory[I] extends Grammar[I, Init[I, ?, ?]] {
  import cats.arrow.Compose
  import cats.Semigroup

  type F[A,B] = Init[I, A, B]
  type Out = Init.OutF[I]

  private def root[A](a : A): A @@ Root = tag[Root][A](a)

  def at(p: Path) = new At[F, Out, I] {
    def run = new Init[I, Out, Option[I]] {
      def ump[K[_, _]](g: Grammar[I, K]) : K[Out, Option[I]] =
        g.coerce(g.at(p).run)
    }
  }

  def knil : F[Out, HNil] =
    new Init[I, Out, HNil] {
      def ump[K[_, _]](g: Grammar[I, K]) : K[Out, HNil] =
        g.coerce(g.knil)
    }

  def is[A](implicit ev: F[_ >: Out <: I, A]) : F[I,A] =
    new Init[I, I, A] {
      def ump[K[_, _]](g: Grammar[I, K]) : K[I, A] =
        g.is[A](ev.coerce.ump[K](g))
    }

  def req[A](implicit ev: F[_ >: Out <: I, A]) : F[Option[I], A] =
    new Init[I, Option[I], A] {
      def ump[K[_, _]](g: Grammar[I, K]) : K[Option[I], A] =
        g.req[A](ev.coerce.ump[K](g))
    }

  def opt[A](implicit ev: F[_ >: Out <: I, A]) : F[Option[I], Option[A]] =
    new Init[I, Option[I], Option[A]] {
      def ump[K[_, _]](g: Grammar[I, K]) : K[Option[I], Option[A]] =
        g.opt(ev.coerce.ump[K](g))
    }

  def min[A](a: A)(implicit O: Ordering[A]): C[A] = root[F[A,A]] {
    new Init[I, A, A] {
      def ump[K[_, _]](g: Grammar[I, K]): K[A,A] =
        g.min(a)
    }
  }

  def max[A](a: A)(implicit O: Ordering[A]): C[A] = root[F[A,A]] {
    new Init[I, A, A] {
      def ump[K[_, _]](g: Grammar[I, K]): K[A,A] =
        g.max(a)
    }
  }

  def notEmpty: C[String] = root[F[String, String]] {
    new Init[I, String, String] {
      def ump[K[_, _]](g: Grammar[I, K]): K[String, String] =
        g.notEmpty
    }
  }

  def minLength(l: Int): C[String] = root[F[String, String]] {
    new Init[I, String, String] {
      def ump[K[_, _]](g: Grammar[I, K]): K[String, String] =
        g.minLength(l)
    }
  }

  def maxLength(l: Int): C[String] = root[F[String, String]] {
    new Init[I, String, String] {
      def ump[K[_, _]](g: Grammar[I, K]): K[String, String] =
        g.maxLength(l)
    }
  }

  def pattern(regex: scala.util.matching.Regex): C[String] = root[F[String, String]] {
    new Init[I, String, String] {
      def ump[K[_, _]](g: Grammar[I, K]): K[String, String] =
        g.pattern(regex)
    }
  }

  def email: C[String] = root[F[String, String]] {
    new Init[I, String, String] {
      def ump[K[_, _]](g: Grammar[I, K]): K[String, String] =
        g.email
    }
  }

  def forall[I0, O](f: F[I0, O]): F[Seq[I0], Seq[O]] =
    new Init[I, Seq[I0], Seq[O]] {
      def ump[K[_, _]](g: Grammar[I, K]): K[Seq[I0], Seq[O]] =
        g.forall[I0,O](f.ump[K](g))
    }

  def equalTo[A](a: A): C[A] = root[F[A,A]] {
    new Init[I, A, A] {
      def ump[K[_, _]](g: Grammar[I, K]): K[A,A] =
        g.equalTo[A](a)
    }
  }


  implicit def mkLazy: MkLazy[F] =
    new MkLazy[F] {
      def apply[A, B](k: => F[A, B]): F[A, B] =
        new Init[I, A, B] {
          def ump[K[_, _]](g: Grammar[I, K]): K[A,B] =
            k.ump[K](g)
        }
    }


  implicit def composeTC: Compose[F] = new Compose[F] {
    def compose[A, B, C](f: F[B, C], h: F[A, B]): F[A, C] =
      new Init[I, A, C] {
        def ump[K[_, _]](g: Grammar[I, K]): K[A,C] =
          g.composeTC.compose[A,B,C](f.ump[K](g), h.ump[K](g))
      }
  }

  implicit def semigroupTC[I0, O]: Semigroup[F[I0, O] @@ Root] =
    new Semigroup[F[I0, O] @@ Root] {
      def combine(x: F[I0, O] @@ Root, y: F[I0, O] @@ Root) : F[I0, O] @@ Root = root[F[I0, O]] {
        new Init[I, I0, O] {
          def ump[K[_, _]](g: Grammar[I, K]): K[I0, O] =
            g.semigroupTC.combine(root(x.ump[K](g)), root(y.ump[K](g)))
        }
      }
    }

  implicit def mergeTC: Merge[F, Out] =
    new Merge[F, Out] {
      def merge[A, B <: HList](fa: F[Out, A], fb: F[Out, B]): F[Out, A :: B] =
        new Init[I, Out, A :: B] {
          def ump[K[_, _]](g: Grammar[I, K]): K[Out, A :: B] = {
            val ka: K[g.Out, A] = fa.umpG[cats.Id, K](g)
            val kb: K[g.Out, B] = fb.umpG[cats.Id, K](g)
            g.coerce(g.mergeTC.merge[A, B](ka, kb))
          }
        }
    }

  implicit def mergeTCOpt: Merge[F, Option[Out]] =
    new Merge[F, Option[Out]] {
      def merge[A, B <: HList](fa: F[Option[Out], A], fb: F[Option[Out], B]): F[Option[Out], A :: B] =
        new Init[I, Option[Out], A :: B] {
          def ump[K[_, _]](g: Grammar[I, K]): K[Option[Out], A :: B] = {
            val ka: K[Option[g.Out], A] = fa.umpG[Option, K](g)
            val kb: K[Option[g.Out], B] = fb.umpG[Option, K](g)
            g.coerceF[Option, A :: B](g.mergeTCOpt.merge[A, B](ka, kb))
          }
        }
    }

  def mapPath(f: Path => Path): P = ???

  def toGoal[Repr, A]: F[Out, Repr] => F[Out, Goal[Repr, A]] =
    (f : F[Out, Repr]) => new Init[I, Out, Goal[Repr, A]] {
      def ump[K[_, _]](g: Grammar[I, K]): K[Out, Goal[Repr, A]] =
        g.coerce(g.toGoal(f.umpG[cats.Id, K](g)))
    }

  implicit def int: F[I, Int] @@ Root = root {
    new Init[I, I, Int] {
      def ump[K[_, _]](g: Grammar[I, K]): K[I, Int] =
        g.int
    }
  }

  implicit def string: F[I, String] @@ Root = root {
    new Init[I, I, String] {
      def ump[K[_, _]](g: Grammar[I, K]): K[I, String] =
        g.string
    }
  }

  implicit def short: F[I, Short] @@ Root = root {
    new Init[I, I, Short] {
      def ump[K[_, _]](g: Grammar[I, K]): K[I, Short] =
        g.short
    }
  }

  implicit def long: F[I, Long] @@ Root = root {
    new Init[I, I, Long] {
      def ump[K[_, _]](g: Grammar[I, K]): K[I, Long] =
        g.long
    }
  }

  implicit def float: F[I, Float] @@ Root = root {
    new Init[I, I, Float] {
      def ump[K[_, _]](g: Grammar[I, K]): K[I, Float] =
        g.float
    }
  }

  implicit def double: F[I, Double] @@ Root = root {
    new Init[I, I, Double] {
      def ump[K[_, _]](g: Grammar[I, K]): K[I, Double] =
        g.double
    }
  }

  implicit def jBigDecimal: F[I, java.math.BigDecimal] @@ Root = root {
    new Init[I, I, java.math.BigDecimal] {
      def ump[K[_, _]](g: Grammar[I, K]): K[I, java.math.BigDecimal] =
        g.jBigDecimal
    }
  }

  implicit def bigDecimal: F[I, BigDecimal] @@ Root = root {
    new Init[I, I, BigDecimal] {
      def ump[K[_, _]](g: Grammar[I, K]): K[I, BigDecimal] =
        g.bigDecimal
    }
  }

  implicit def boolean: F[I, Boolean] @@ Root = root {
    new Init[I, I, Boolean] {
      def ump[K[_, _]](g: Grammar[I, K]): K[I, Boolean] =
        g.boolean
    }
  }
  
  implicit def seq[A](implicit ev: F[_ >: Out <: I, A]): F[I, Seq[A]] =
    new Init[I, I, Seq[A]] {
      def ump[K[_, _]](g: Grammar[I, K]) : K[I, Seq[A]] =
        g.seq[A](ev.coerce.ump[K](g))
    }
  implicit def list[A](implicit ev: F[_ >: Out <: I, A]): F[I, List[A]] =
    new Init[I, I, List[A]] {
      def ump[K[_, _]](g: Grammar[I, K]) : K[I, List[A]] =
        g.list[A](ev.coerce.ump[K](g))
    }

  implicit def array[A: scala.reflect.ClassTag](implicit ev: F[_ >: Out <: I, A]): F[I, Array[A]] =
    new Init[I, I, Array[A]] {
      def ump[K[_, _]](g: Grammar[I, K]): K[I, Array[A]] =
        g.array[A](implicitly, ev.coerce.ump[K](g))
    }

  implicit def map[A](implicit ev: F[_ >: Out <: I, A]): F[I, Map[String, A]] =
    new Init[I, I, Map[String, A]] {
      def ump[K[_, _]](g: Grammar[I, K]): K[I, Map[String, A]] =
        g.map[A](ev.coerce.ump[K](g))
    }

  implicit def traversable[A](implicit ev: F[_ >: Out <: I, A]): F[I, Traversable[A]] =
    new Init[I, I, Traversable[A]] {
      def ump[K[_, _]](g: Grammar[I, K]): K[I, Traversable[A]] =
        g.traversable[A](ev.coerce.ump[K](g))
    }
}