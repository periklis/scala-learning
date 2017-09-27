package experiments

package object patternmatching extends App {

  def swap[A](as: Seq[A]) = as match {
    case Seq(x, y, rest @ _*) => Seq(y, x) ++ rest
    case _ => as
  }

  println(swap(Array(1)))
  println(swap(Array(1,2)))
  println(swap(Array(1,2,3)))

  val tree = (8 :: 3 :: Nil) :: 2 :: (5 :: Nil) :: Nil
  println(tree)

  def leafSum(as: List[Any]): Int = as match {
    case (h: Int) :: Nil => h
    case List(x: Int, t @ _*) => x + leafSum(t.toList)
    case List(h: List[Any], t @ _*) => leafSum(h.toList) + leafSum(t.toList)
    case Nil => 0
  }

  assert(leafSum(tree) == 18)

  var btree = Node(Node(Node(Leaf(8), Leaf(3)), Leaf(2)), Leaf(5))

  def sum(t: BinaryTree): Int = t match {
    case Leaf(v) => v
    case Node(Leaf(l), Leaf(r)) => l + r
    case Node(Leaf(v), r @ Node(_,_)) => v + sum(r)
    case Node(l @ Node(_,_), Leaf(v)) => sum(l) + v
    case Node(l @ Node(_,_), r @ Node(_,_)) => sum(l) + sum(r)
  }

  assert(sum(btree) == 18)

  val ntree = TreeNode(
    TreeNode(
      TreeNode(
        TreeLeaf(1), TreeLeaf(2))),
    TreeNode(
      TreeNode(
        TreeLeaf(1), TreeLeaf(2)),
      TreeNode(
        TreeLeaf(1), TreeLeaf(2))),
    TreeNode(
      TreeNode(
        TreeLeaf(1), TreeLeaf(2))))

  def treeSum(t: Tree): Int = t match {
    case TreeLeaf(v) => v
    case TreeNode(ns @ _*) => ns.map(treeSum).sum
  }

  assert(treeSum(ntree) == 12)

  val opTree = OpNode(
    '+',
    OpNode(
      '*',
      OpLeaf(3),
      OpLeaf(8)
    ),
    OpLeaf(2),
    OpNode(
      '-',
      OpLeaf(0),
      OpLeaf(5)
    ),
    OpNode(
      '/',
      OpLeaf(3),
      OpLeaf(1),
      OpLeaf(3)
    )
  )

  def eval(op: OpTree): Int = op match {
    case OpLeaf(v) => v
    case OpNode('+', ns @ _*) => ns.map(eval).sum
    case OpNode('-', ns @ _*) => ns.map(eval).foldLeft(0)(_ - _)
    case OpNode('*', ns @ _*) => ns.map(eval).product
    case OpNode('/', ns @ _*) => ns.map(eval).foldLeft(1.0)((b, d) => d / b).toInt
    case _ => 0
  }

  println(eval(opTree))


  def compose(g: (Double) => Option[Double], f: (Double) => Option[Double]) =
    (x: Double) => f(x) match {
      case Some(v) => g(v)
      case _ => None
    }

  def f(x: Double) = if (x != 1) Some(1 / (x - 1)) else None
  def g(x: Double) = if (x >= 0) Some(math.sqrt(x)) else None
  val h = compose(g, f)

  println(h(1))
}

package patternmatching {

  sealed abstract class BinaryTree
  case class Leaf(value: Int) extends BinaryTree
  case class Node(left: BinaryTree, right: BinaryTree) extends BinaryTree

  sealed abstract class Tree
  case class TreeLeaf(value: Int) extends Tree
  case class TreeNode(nodes: Tree*) extends Tree

  sealed abstract class OpTree
  case class OpLeaf(value: Int) extends OpTree
  case class OpNode(op: Char, nodes: OpTree*) extends OpTree
}
