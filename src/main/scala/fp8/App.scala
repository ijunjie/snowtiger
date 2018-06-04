package fp8

object App {
  def main(args: Array[String]): Unit = {
    var tree = Branch(
      Branch(Leaf(1), Leaf(2)),
      Branch(
        Branch(Leaf(10), Leaf(8)),
        Leaf(3)
      )
    )
    println(tree.size)
    println(tree.countLeafs)
    println(tree.countBranches)
  }

}


trait Tree[+A] {
  // 递归方式实现 size
  def size: Int = this match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + l.size + r.size
  }

  def countLeafs: Int = this match {
    case Leaf(_) => 1
    case Branch(l, r) => l.countLeafs + r.countLeafs
  }

  def countBranches: Int = this match {
    case Leaf(_) => 0
    case Branch(l, r) => 1 + l.countBranches + r.countBranches
  }

  def depth: Int = this match {
    case Leaf(_) => 0
    case Branch(l, r) => 1 + (l.depth max r.depth)
  }

  def maxValue: Int = this match {
    case Leaf(a: Int) => a
    case Branch(l, r) => l.maxValue max r.maxValue
  }

  // 神奇的是，以上这些函数是可以进一步抽象
  // 仔细观察上面每个函数的特点，
  // 整个函数是 A=>B，对 Leaf 的处理函数模型为 A=>B，对 Branch 的处理模型为 B=>B
  def fold[B](lf: A => B)(bf: (B, B) => B): B = this match {
    case Leaf(n) => lf(n)
    case Branch(l, r) => bf(l.fold(lf)(bf), r.fold(lf)(bf))
  }

  // 则前述方法可以用这个fold来实现
  def sizeByFold = fold(a => 1)(1 + _ + _)

  // 下面 maxValueByFold 实现较为困难，因为maxValue本身要求 A 为 Int
  // def maxValueByFold = fold[Int](a => a)((x, y) => 0 + (x max y))
  def depthByFold = fold(a => 0)((x, y) => 1 + (x max y))

  // 实现 map 和 flatMap
  def map[B](f: A => B): Tree[B] = this match {
    case Leaf(a) => Leaf(f(a))
    case Branch(l, r) => Branch(l.map(f), r.map(f))
  }

  def flatMap[B](f: A => Tree[B]): Tree[B] = this match {
    case Leaf(a) => f(a)
    case Branch(l, r) => Branch(l.flatMap(f), r.flatMap(f))
  }

}

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


