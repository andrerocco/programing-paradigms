import scala.io.Source

import solver.KojunSolver
import utils.Matrix

object Main {
  def main(args: Array[String]): Unit = {
    // Lê arquivos que contém
    // O tamanho N da matriz (N x N) na primeira linha
    // A matriz de valores (ocupando N linhas)
    /*
    2 0 0 0 1 0
    0 0 0 3 0 0
    0 3 0 0 5 3
    0 0 0 0 0 0
    0 0 3 0 4 2
    0 0 0 0 0 0
     */
    // A matriz de regiões (ocupando N linhas)
    /* 
    a a b b b c
    d d d d d c
    e f f f d g
    e e e f g g
    h h i j j j
    k k i i j j
     */
    val filePath = "src/inputs/6x6/kojun_1.txt" // Relative path to the file

    // Abre e lê o arquivo
    val fileContents = openFile(filePath)
    var valueGrid: List[List[Int]] = List()
    var regionGrid: List[List[String]] = List()

    fileContents match {
      case Some(contents) =>
        val matrixSize = contents.head.toInt
        // Lê a matriz de valores (inteiros) e regiões (caracteres)
        valueGrid = contents.slice(1, matrixSize + 1).map(line => line.split(" ").map(_.toInt).toList)
        regionGrid = contents.slice(matrixSize + 1, matrixSize * 2 + 1).map(line => line.split(" ").map(filterAlphabetic).toList)

      case None =>
        println("An error occurred while opening the file.")
        return
    }

    // Cria uma matriz de valores e regiões
    val valueMatrix = new Matrix[Int](valueGrid)
    val regionMatrix = new Matrix[String](regionGrid)

    // Imprime a matriz de valores e regiões
    /* println("Value matrix:")
    valueMatrix.printMatrix()
    println("Region matrix:")
    regionMatrix.printMatrix() */

    // Resolve o quebra-cabeça
    val solver = new KojunSolver(valueMatrix, regionMatrix)
    val solution = solver.solve()

    // Imprime a solução
    solution match {
      case Some(solutionMatrix) =>
        println("Solution:")
        solutionMatrix.printMatrix()
      case None =>
        println("No solution found.")
    }
  }

  // Função para filtrar caracteres alfabéticos
  private def filterAlphabetic(str: String): String = {
    str.filter(_.isLetter)
  }

  private def openFile(filePath: String): Option[List[String]] = {
    try {
      val source = Source.fromFile(filePath)
      val lines = source.getLines.toList
      source.close()
      Some(lines)
    } catch {
      case e: Exception =>
        println(s"An error occurred: ${e.getMessage}")
        None
    }
  }
}
