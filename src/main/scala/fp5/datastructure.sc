// Functional Data Structure 不可变数据结构

var arr0 = Array(1, 2, 3)
var sum0 = arr0(0) + arr0(1) + arr0(2) // 需要中间变量且在数据结构外部进行

val arr = Array(1, 2, 3)
val sum = arr.sum
val arr1 = arr.map { x => if(x == 1) sum else x}
// 理论上进行了复制，实际上 Scala 针对效率做了优化
// 效果上是函数式的。实际上底层可能用了一些命令式手段
