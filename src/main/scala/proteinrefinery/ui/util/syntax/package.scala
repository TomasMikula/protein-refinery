package proteinrefinery.ui.util

import java.util.function.{BiFunction, Consumer, Predicate, Function => JFunction}
import javafx.beans.value.{ChangeListener, ObservableValue}
import javafx.event.{ActionEvent, EventHandler}
import javafx.scene.control.{Button, MenuItem}

import org.reactfx.util.{TetraFunction, TriFunction}
import org.reactfx.{EventStream, Subscription}
import org.reactfx.value.Val

package object syntax {

  implicit class AnyOps[A](a: A) {
    def <|(f: A => Unit): A = { f(a); a }

    /** Ignore non-unit return value without warnings. */
    def ignoreResult(): Unit = ()
  }

  implicit class ObservableValueOps[A](self: ObservableValue[A]) {
    def onChange(f: (A, A) => Unit): Unit = self.addListener(new ChangeListener[A] {
      def changed(observable: ObservableValue[_ <: A], oldValue: A, newValue: A): Unit = f(oldValue, newValue)
    })

    def |@|[B](that: ObservableValue[B]): Ap2Builder[A, B] = Ap2Builder(self, that)

    def filter1(p: A => Boolean): Val[A] = Val.filter(self, j(p))
    def filter2[A1, A2](p: (A1, A2) => Boolean)(implicit ev: A =:= (A1, A2)): Val[A] =
      Val.filter(self, j(a => { val (a1, a2) = ev(a); p(a1, a2) }))
    def filter3[A1, A2, A3](p: (A1, A2, A3) => Boolean)(implicit ev: A =:= (A1, A2, A3)): Val[A] =
      Val.filter(self, j(a => { val (a1, a2, a3) = ev(a); p(a1, a2, a3) }))
    def filter4[A1, A2, A3, A4](p: (A1, A2, A3, A4) => Boolean)(implicit ev: A =:= (A1, A2, A3, A4)): Val[A] =
      Val.filter(self, j(a => { val (a1, a2, a3, a4) = ev(a); p(a1, a2, a3, a4) }))

    def map1[B](f: A => B): Val[B] = Val.map(self, j(f))
    def map2[A1, A2, B](f: (A1, A2) => B)(implicit ev: A =:= (A1, A2)): Val[B] =
      Val.map(self, j((a: A) => { val (a1, a2) = ev(a); f(a1, a2) }))
    def map3[A1, A2, A3, B](f: (A1, A2, A3) => B)(implicit ev: A =:= (A1, A2, A3)): Val[B] =
      Val.map(self, j((a: A) => { val (a1, a2, a3) = ev(a); f(a1, a2, a3) }))
    def map4[A1, A2, A3, A4, B](f: (A1, A2, A3, A4) => B)(implicit ev: A =:= (A1, A2, A3, A4)): Val[B] =
      Val.map(self, j((a: A) => { val (a1, a2, a3, a4) = ev(a); f(a1, a2, a3, a4) }))
  }

  implicit class EventStreamOps[A](stream: EventStream[A]) {
    def forEach(f: A => Unit): Subscription = stream.subscribe(j(f))
  }

  implicit class ButtonOps(button: Button) {
    def onAction(f: () => Unit): Unit = button.setOnAction(new EventHandler[ActionEvent] {
      def handle(event: ActionEvent): Unit = f()
    })
  }

  implicit class MenuItemOps(menuItem: MenuItem) {
    def onAction(f: () => Unit): Unit = menuItem.setOnAction(new EventHandler[ActionEvent] {
      def handle(event: ActionEvent): Unit = f()
    })
  }

  def j[A](f: A => Unit): Consumer[A] = new Consumer[A] {
    def accept(a: A): Unit = f(a)
  }

  def j[A](p: A => Boolean): Predicate[A] = new Predicate[A] {
    def test(a: A): Boolean = p(a)
  }

  def j[A, R](f: A => R): JFunction[A, R] = new JFunction[A, R] {
    def apply(a: A): R = f(a)
  }

  def j[A, B, R](f: (A, B) => R): BiFunction[A, B, R] = new BiFunction[A, B, R] {
    def apply(a: A, b: B): R = f(a, b)
  }

  def j[A, B, C, R](f: (A, B, C) => R): TriFunction[A, B, C, R] = new TriFunction[A, B, C, R] {
    def apply(a: A, b: B, c: C): R = f(a, b, c)
  }

  def j[A, B, C, D, R](f: (A, B, C, D) => R): TetraFunction[A, B, C, D, R] = new TetraFunction[A, B, C, D, R] {
    def apply(a: A, b: B, c: C, d: D): R = f(a, b, c, d)
  }
}

package syntax {

  final case class Ap2Builder[A, B](a: ObservableValue[A], b: ObservableValue[B]) {
    def apply[R](f: (A, B) => R): Val[R] = Val.combine(a, b, j(f))
    def tuple: Val[(A, B)] = apply((a, b) => (a, b))
    def |@|[C](c: ObservableValue[C]): Ap3Builder[A, B, C] = Ap3Builder(a, b, c)
  }

  final case class Ap3Builder[A, B, C](a: ObservableValue[A], b: ObservableValue[B], c: ObservableValue[C]) {
    def apply[R](f: (A, B, C) => R): Val[R] = Val.combine(a, b, c, j(f))
    def tuple: Val[(A, B, C)] = apply((a, b, d) => (a, b, d))
    def |@|[D](d: ObservableValue[D]): Ap4Builder[A, B, C, D] = Ap4Builder(a, b, c, d)
  }

  final case class Ap4Builder[A, B, C, D](a: ObservableValue[A], b: ObservableValue[B], c: ObservableValue[C], d: ObservableValue[D]) {
    def apply[R](f: (A, B, C, D) => R): Val[R] = Val.combine(a, b, c, d, j(f))
    def tuple: Val[(A, B, C, D)] = apply((a, b, c, d) => (a, b, c, d))
  }

}