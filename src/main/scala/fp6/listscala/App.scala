package fp6.listscala
// 徒手写一个 List
// 深刻体现函数式编程中，模式匹配和递归的精髓


// 1. case class 方式 Cons & Nil
trait List[+A] {
  def length: Int = this match {
    case Nil => 0
    case Cons(h, t) => 1 + t.length
  }

  def sum[B >: A](z: B)(f: (B, B) => B): B = this match {
    case Nil => z
    case Cons(h, t) => f(h, t.sum(z)(f))
  }

  def head: A = this match {
    case Nil => sys.error("Empty List!")
    case Cons(h, t) => h
  }

  def tail: List[A] = this match {
    case Nil => sys.error("Empty List!")
    case Cons(h, t) => t
  }

  def take(n: Int): List[A] = n match {
    case k if (k < 0) => sys.error("index < 0 !")
    case 0 => Nil
    case _ => this match {
      case Nil => Nil
      case Cons(h, t) => Cons(h, t.take(n - 1))
    }
  }

  def takeWhile(f: A => Boolean): List[A] = this match {
    case Nil => Nil
    case Cons(h, t) => if (f(h)) Cons(h, t.takeWhile(f)) else Nil
  }

  def drop(n: Int): List[A] = n match {
    case k if k < 0 => sys.error("index < 0 !")
    case 0 => this
    case _ => this match {
      case Nil => Nil
      case Cons(h, t) => t.drop(n - 1)
    }
  }

  def dropWhile(f: A => Boolean): List[A] = this match {
    case Nil => Nil
    case Cons(h, t) => if (f(h)) t.dropWhile(f) else this
  }

  def ++[B >: A](a: List[B]): List[B] = this match {
    case Nil => a
    case Cons(h, t) => Cons(h, t.++(a))
  }

  def init: List[A] = this match {
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, t.init)
  }


  // 下面实现几个经典的函数
  def map[B](f: A => B): List[B] = this match {
    case Nil => Nil
    case Cons(h, t) => Cons(f(h), t.map(f))
  }

  def flatMap[B](f: A => List[B]): List[B] = this match {
    case Nil => Nil
    case Cons(h, t) => f(h) ++ t.flatMap(f)
  }

  def filter(f: A => Boolean):List[A] = this match {
    case Nil => Nil
    case Cons(h, t) => if (f(h)) Cons(h, t.filter(f)) else t.filter(f)
  }

}

// cons 是 FP 最经典的结构之一。有人也许会疑惑，如何添加元素，其实只需要构造 Cons(e, oldList) 即可
case class Cons[+A](h: A, t: List[A]) extends List[A]

// 注意 Nil 是一个 object
case object Nil extends List[Nothing]

// apply的传入参数as是个数组Array[A]，我们使用了Scala标准集合库Array的方法:as.head, as.tail
object List {
  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }
}



object App {
  def main(args: Array[String]): Unit = {
    val nums: List[Number] = List[Integer](5, 6, 7, 8)




    // 测试
    // 学习过 Scheme 的人对此不会陌生
    List(1, 2, 3, 4) // Cons(1,Cons(3,Cons(4,Nil)))
    // 不必写成 Cons(1,Cons(3,Cons(4,Nil)))

    // List 中实现 sum
    //List(1, 2, 3, 4).sum

    // 测试抽象化 sum2
    List(1, 2, 3).sum(0)(_ + _)
    List("a", "b", "c").sum("")((x, y) => x.concat(y))


    // 最后，将常用函数补充完整

    List(1, 2).init
  }
}


// 2. 使用方法实现， 后面讨论 Stream 时细说，这里先用第一种方式

//trait MyList2[+A] {
//  def node: Option[(A, MyList2[A])]
//  def isEmpty = node.isEmpty
//}
//
//object MyList2 {
//  def empty[A] = new MyList2[A] {
//    def node = None
//  }
//  def cons[A](head:A, tail: MyList2[A]) = new MyList2[A] {
//    def node = Some[(A, MyList2[A])](head, tail)
//  }
//}