package aima.core.search.problems

/**
  * @author Shawn Garner
  */
object RomaniaRoadMap {
  case class Node(name: String) extends AnyVal
  case class Edge(node1: Node, node2: Node, cost: Int)

  val Arad = Node("Arad")
  val Zerind = Node("Zerind")
  val Oradea = Node("Oradea")
  val Sibiu = Node("Sibiu")
  val Timisoara = Node("Timisoara")  
  val Lugoj = Node("Lugoj")
  val Mehadia = Node("Mehadia")
  val Drobeta = Node("Drobeta")
  val RimnicuVilcea = Node("Rimnicu Vilcea")
  val Craiova = Node("Craiova")
  val Fagaras = Node("Fagaras")
  val Pitesti = Node("Pitesti")
  val Bucharest = Node("Bucharest")
  val Giurgiu = Node("Giurgiu")
  val Urziceni = Node("Urziceni")
  val Neamt = Node("Neamt")
  val Iasi = Node("Iasi")
  val Vaslui = Node("Vaslui")
  val Hirsova = Node("Hirsova")
  val Eforie = Node("Eforie")

  // format: off
  val edges: Map[Node, List[Edge]] =
    List(
      Edge(Arad, Zerind, 75), Edge(Arad, Timisoara, 118), //Arad Edges
      Edge(Zerind, Arad, 75), Edge(Zerind, Oradea, 71), //Zerind Edges
      Edge(Oradea, Zerind, 71), Edge(Oradea, Sibiu, 151), //Oradea Edges
      Edge(Sibiu, Arad, 140), Edge(Sibiu, Oradea, 151), Edge(Sibiu, Fagaras, 99), Edge(Sibiu, RimnicuVilcea, 80), //Sibiu Edges
      Edge(Timisoara, Arad, 118), Edge(Timisoara, Lugoj, 111), //Timisoara Edges
      Edge(Lugoj, Timisoara, 111), Edge(Lugoj, Mehadia, 70),//Lugoj Edges
      Edge(Mehadia, Lugoj, 70), Edge(Mehadia, Drobeta, 75),//Mehadia Edges
      Edge(Drobeta, Mehadia, 75), Edge(Drobeta, Craiova, 120),//Drobeta Edges
      Edge(RimnicuVilcea, Sibiu, 80), Edge(RimnicuVilcea, Craiova, 146), Edge(RimnicuVilcea, Pitesti, 97),//Rimnicu Vilcea Edges
      Edge(Craiova, Drobeta, 120), Edge(Craiova, RimnicuVilcea, 146), Edge(Craiova, Pitesti, 138),//Craiova Edges
      Edge(Fagaras, Sibiu, 99), Edge(Fagaras, Bucharest, 211),//Fagaras Edges
      Edge(Pitesti, Craiova, 138), Edge(Pitesti, RimnicuVilcea, 97), Edge(Pitesti, Bucharest, 101),//Pitesti Edges
      Edge(Bucharest, Pitesti, 101), Edge(Bucharest, Fagaras, 211), Edge(Bucharest, Urziceni, 85), Edge(Bucharest, Giurgiu, 90),//Bucharest Edges
      Edge(Giurgiu, Bucharest, 90),//Giurgiu Edges
      Edge(Urziceni, Bucharest, 85), Edge(Urziceni, Vaslui, 142), Edge(Urziceni, Hirsova, 98),//Urziceni Edges
      Edge(Neamt, Iasi, 87),//Neamt Edges
      Edge(Iasi, Neamt, 87), Edge(Iasi, Vaslui, 92),//Iasi Edges
      Edge(Vaslui, Iasi, 92), Edge(Vaslui, Urziceni, 142),//Vaslui Edges
      Edge(Hirsova, Urziceni, 98), Edge(Hirsova, Eforie, 86),//Hirsova Edges
      Edge(Eforie, Hirsova, 86)//Eforie Edges
    ).foldLeft(Map.empty[Node, List[Edge]]) { (acc, edge) =>
      val node1 = edge.node1
      val listForEdge = acc.getOrElse(node1, List.empty[Edge])
      acc.updated(node1, edge :: listForEdge)
    }
  // format: on
}


