abstract class Node(val name: String) {
  def maxDepth: Int

  def minDepth: Int

  def nodesCount(): Int

  def nodes: List[Node]

  def add(path: String): Node = {
    def makeAdd(path: String): Node = {
      val parts = path.split("/").filterNot(_.isEmpty).toList
      parts match {
        case head :: Nil =>
          File(head)
        case head :: tail =>
          val nds = nodes
          val nn = nds.find(_.name == head)
          nn match {
            case Some(d) =>
              d.add(tail.mkString("/"))
            case _ =>
              Dir(head, List()).add(tail.mkString("/"))
          }
        case _ => throw new Exception("No node")
      }
    }

    val adds = makeAdd(path)
    Dir(name, adds :: nodes.filter(_.name != adds.name))
  }
}

case class Dir(override val name: String, nodes: List[Node]) extends Node(name) {
  override def maxDepth: Int = nodes.map(_.maxDepth).max + 1

  override def minDepth: Int = nodes.map(_.minDepth).min + 1

  override def nodesCount(): Int = nodes.map(_.nodesCount()).sum + 1

  override def toString: String = s"Dir($name)"
}

case class File(override val name: String) extends Node(name) {
  override def maxDepth: Int = 0

  override def minDepth: Int = 0

  override def nodes: List[Node] = throw new Exception("No nodes on List")

  override def add(name: String): Node = File(name)

  override def nodesCount(): Int = 1

  override def toString: String = s"File($name)"
}


object Main extends App {
  def dfs(node: Node): Int = {
    node match {
      case dir: Dir =>
        print(s"$dir (2) --> ")
        val nodesToGo = dir.nodes sortBy (_.minDepth)

        nodesToGo.foldLeft(0)((sum, node) => sum + dfs(node)) + 2
      case file =>
        print(s"delete $file (1) --> ")
        1
    }
  }

  val fs = Dir("/", List())
    .add("/etc/sudoers")
    .add("/root/readme")
    .add("/etc/zshrc")
    .add("/bin/curl")
    .add("/etc/ntp/keys")

  /*  val fs = Dir("/", List())
      .add("/etc")
      .add("/root")*/

  //  val fs = Dir("/", List())
  //    .add("/a/ab/abc")
  //    .add("/a/ab/aba")
  //    .add("/a/aa")
  //    .add("/a/ac/aca")
  //    .add("/b/ba/baa")

  val max = fs.maxDepth - 1
  val i: Int = dfs(fs) - 2
  println(i - max)
}
