package fp7

/**
  * foldRight 是最自然的，因为 Cons 结构最右端是 Nil. 从 tail 开始递归, 从内到外最后处理 head
  * foldLeft 则先从 head 开始处理，与初始值计算后再次作为初始值参与下次计算
  *
  */
object App {
  def main(args: Array[String]): Unit = {
    import fp6.listscala._
    def foldRight[A, B](l: List[A])(z: B)(op: (A, B) => B): B = l match {
      case Nil => z
      case Cons(h, t) => op(h, foldRight(t)(z)(op))
    }

    val r1 = foldRight(List(1, 2, 3))(1)(_ * _)
    println(r1)

    // 可以是不同类型的结果
    val r2 = foldRight(List(1, 2, 3))(Nil: List[Int])((e, acc) => Cons(e * 10, acc))
    println(r2) // 得到 Cons(10,Cons(20,Cons(30,Nil)))


    // foldLeft 中的初始值不断充当 head，累积值
    def foldLeft[A, B](l: List[A])(z: B)(op: (A, B) => B): B = l match {
      case Nil => z
      case Cons(h, t) => foldLeft(t)(op(h, z))(op)
    }

    val r3 = foldLeft(List(1, 2, 3))(1)(_ * _)
    println(r3)

    val r4 = foldLeft(List(1, 2, 3))(Nil: List[Int])((e, acc) => Cons(e * 10, acc))
    println(r4) // Cons(30,Cons(20,Cons(10,Nil))) 与 foldRight 顺序是相反的

    // JS 的函数式编程库 ramda 有一个很有特点的设计，将数据参数总是放到最后
    // 其参数的一般顺序为，函数参数-初始值-数据；函数参数形式一般为 (acc, e) => ...
    // 这是面向对象之外的另一种优雅的设计方法
    // 下面按照 ramda 的风格重构两个函数

    def foldRightRamda[A, B](op: (B, A) => B)(acc: B)(l: List[A]): B = l match {
      case Nil => acc
      case Cons(h, t) => op(foldRightRamda(op)(acc)(t), h)
    }

    def foldLeftRamda[A, B](op: (B, A) => B)(acc: B)(l: List[A]): B = l match {
      case Nil => acc
      case Cons(h, t) => foldLeftRamda(op)(op(acc, h))(t) // 最先计算的是 acc 和 h
    }

    // 类似函数还有 reduce, 在 Scala 中 reduce 和 fold 的区别是
    // 1. reduce 要求结果类型与元素类型一致
    // 2. reduce 默认以第一个元素作为初始值
    // 可以将 reduce 看作 fold 的简化版。而在 Java 中没有 fold, 靠重载 reduce 传入初始值

    def reduceRightRamda[A](op: (A, A) => A)(l: List[A]): A = l match {
      case Cons(h, Nil) => h
      case Cons(h, t) => op(h, reduceRightRamda(op)(t))
    }

    def reduceLeftRamda[A](op: (A, A) => A)(l: List[A]): A = l match {
      case Nil => sys.error("Empty list!")
      case Cons(h, t) => foldLeftRamda(op)(h)(t) // 注意这里用了 foldLeft
    }

    // 而 scan 与 reduce 的不同则是保留 op 每次的运算结果，最后汇总到 List 中
    def scanLeftRamda[A](op: (A, A) => A)(z: A)(l: List[A]): List[A] = l match {
      case Nil => Cons(z, Nil)
      case Cons(h, t) => Cons(z, scanLeftRamda(op)(h)(t))
    }


    def reverse[A](l:List[A]):List[A] = foldLeft(l)(Nil:List[A])((e, acc) => Cons(e, acc))

    println("reversed: "  + reverse(List(1, 2, 3)))

    def scanRight[A](op:(A, A) => A)(z: A)(data:List[A]):List[A] = {
      var scanned = List(z)
      var acc = z
      var ll = reverse(data)
      var x = z
      while (ll match {
        case Nil => false
        case Cons(h, t) => {x = h; ll = t; true}
      }) {
        acc = op(acc, x)
        scanned = Cons(acc, scanned)
      }
      scanned
    }

    // 上次实现了函数++，即append。我们同样可以用foldLeft和foldRight来实现
    // 原理是把一个 list 当作 acc

  }
}