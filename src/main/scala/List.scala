package me.evgeniychurikov.postmodern.fplab

import scala.annotation.tailrec
import scala.collection.mutable

enum List[+A]:
  case Nil
  case Cons(hd: A, tl: List[A])

  override def toString: String =
    @scala.annotation.tailrec
    def go(sb: mutable.StringBuilder, as: List[A]): String = {
      as match {
        case List.Nil =>
          sb.result
        case List.Cons(h, t) =>
          go(
            sb
              .append(h)
              .append(if t == List.Nil then "]" else ", "),
            t
          )
      }
    }
    go(new mutable.StringBuilder("["), this)

object List:
  def apply[A](xs: A*): List[A] = of(xs*)
  def of[A](xs: A*): List[A] =
    xs.foldRight(List.Nil: List[A]) { case (x, acc) => List.Cons(x, acc) }

@tailrec
def mv[A](xs: List[A], buf: List[A] = List.Nil): List[A] =
  xs match
    case List.Nil => buf
    case List.Cons(hd, tl) => mv(tl, List.Cons(hd, buf))

def flatten[A](xs: List[List[A]]): List[A] =
  @tailrec
  def go(xs: List[List[A]], buf: List[A] = List.Nil): List[A] =
    xs match
      case List.Nil => buf
      case List.Cons(hd, tl) => go(tl, mv(hd, buf))
  mv(go(xs))

def intersperse[A](xs: List[A], a: A): List[A] =
  @tailrec
  def go(xs: List[A], a: A, buf: List[A]): List[A] =
    xs match
      case List.Nil => buf
      case List.Cons(hd, tl) => go(tl, a, List.Cons(a, List.Cons(hd, buf)))
  xs match
    case List.Nil => List.Nil
    case List.Cons(hd, tl) => List.Cons(hd, go(mv(tl), a, List.Nil))

//intercalate([0], [[1], [2], [3]]) == [1,0,2,0,3]
def intercalate[A](xs: List[A], yss: List[List[A]]): List[A] =
  flatten(intersperse(yss, xs))

def fromString(s: String): List[Char] =
  @tailrec
  def go(s: String, buf: List[Char]): List[Char] =
    if (s.isEmpty)
      buf
    else
      go(s.dropRight(1), List.Cons(s.last, buf))
  go(s, List.Nil)

def toString(cs: List[Char]): String =
  @tailrec
  def go(cs: List[Char], buf: String): String =
    cs match
      case List.Nil => buf
      case List.Cons(hd, tl) => go(tl, buf + hd)
  go(cs, "")

@main def run(): Unit =
  println(List(1))
