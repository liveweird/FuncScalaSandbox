package net.gebski.FuncScalaSandbox.Chapter4

import scala.annotation.tailrec

import scala.{Option => _, Some => _, Either => _, _}

sealed trait Option[+A] {

  def map[B](f: A => B): Option[B] = {
    this match {
      case a: Some[A] => Some(f(a.get))
      case None => None
    }
  }

  def getOrElse[B >: A](default: => B): B = {
    this match {
      case a: Some[A] => a.get
      case None => default
    }
  }

  def flatMap[B](f: A => Option[B]): Option[B] = {
    this.map(f) getOrElse(None)
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = {
    this match {
      case a: Some[A] => a
      case None => ob
    }
  }

  def filter(f: A => Boolean): Option[A] = {
    this match {
      case a: Some[A] => if (f(a.get)) return a else None
      case None => None
    }
  }
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]