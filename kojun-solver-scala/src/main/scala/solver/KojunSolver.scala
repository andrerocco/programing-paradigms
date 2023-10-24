package solver

import utils.Matrix

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.collection.mutable
import scala.language.postfixOps

class KojunSolver(valueGrid: Matrix[Int], regionGrid: Matrix[String]) {
  private type Position = (Int, Int)

  private val regionMap = getRegionMap

  // Gera uma hashmap onde a chave é o valor da região e o valor é uma lista de tuplas (linha, coluna)
  private def getRegionMap: mutable.HashMap[String, List[(Int, Int)]] = {
    val regionMap = new mutable.HashMap[String, ListBuffer[(Int, Int)]]()
    
    for {
      row <- 0 until regionGrid.numRows
      col <- 0 until regionGrid.numCols
    } {
      val regionValue = regionGrid.getMatrixValue((row, col))
      regionMap.getOrElseUpdate(regionValue, ListBuffer()) += ((row, col))
    }
    // Converte a hashmap para uma lista de tuplas
    val result = regionMap.map { case (regionValue, positionsBuffer) =>
      regionValue -> positionsBuffer.toList
    }

    result
  }

  /**
   * A partir da matriz de valores, matriz de regiões, hashmap de regiões, uma Position e um valor Int verifica se o
   * valor pode ser posicionado em uma posição Position da matriz de valores seguindo as regras do Kojun:
   * 1. Inserir um número em cada célula da grade para que cada região de tamanho N contenha cada número de 1 a N
   * exatamente uma vez.
   * 2. Os números em células ortogonalmente adjacentes devem ser diferentes.
   * 3. Se duas células são adjacentes verticalmente na mesma região, o número na célula superior deve ser maior que o
   * número na célula inferior.
   */
  private def canInsertValue(
    position: Position,
    value: Int,
    valueGrid: Matrix[Int],
    regionGrid: Matrix[String],
    regionMap: mutable.HashMap[String, List[(Int, Int)]]
  ): Boolean = {
    val (row, col) = position
    val regionValue = regionGrid.getMatrixValue(position)
    val regionPositions = regionMap(regionValue)

    val adjacentValues = List(
      (row - 1, col),
      (row + 1, col),
      (row, col - 1),
      (row, col + 1)
    ).filter(valueGrid.isValidPosition).map(valueGrid.getMatrixValue)

    val isTopValid = if (valueGrid.isValidPosition((row - 1, col)) && regionGrid.getMatrixValue((row - 1, col)) == regionValue) {
      value < valueGrid.getMatrixValue((row - 1, col))
    } else true

    val isBottomValid = if (valueGrid.isValidPosition((row + 1, col)) && regionGrid.getMatrixValue((row + 1, col)) == regionValue) {
      value > valueGrid.getMatrixValue((row + 1, col))
    } else true

    !(
      valueGrid.getMatrixValue(position) != 0 ||
      value < 1 || value > regionPositions.length ||
      adjacentValues.contains(value) ||
      regionPositions.map(valueGrid.getMatrixValue).contains(value) ||
      !isTopValid ||
      !isBottomValid
    )
  }

  /**
   * Tenta colocar valores válidos na posição atual.
   * Input: Recebe a matriz de valores, de regiões, a linha e coluna atual e uma lista de valores a serem testados.
   * Output:
   * Caso a lista de valores a serem testados esteja vazia, retorna None.
   * Caso o valor possa ser inserido na posição atual, retorna a matriz de valores com o valor inserido.
   * Caso o valor não possa ser inserido na posição atual, chama a função recursivamente com a lista de valores
   * restantes.
   */
  @tailrec
  private def tryValues(
    row: Int,
    col: Int,
    valuesToTry: List[Int],
    valueGrid: Matrix[Int],
    regionGrid: Matrix[String],
    regionMap: mutable.HashMap[String, List[(Int, Int)]]
  ): Option[Matrix[Int]] = {
    if (valuesToTry.isEmpty) {
      None
    } else {
      val value = valuesToTry.head
      if (canInsertValue((row, col), value, valueGrid, regionGrid, regionMap)) {
        val updatedValueGrid = valueGrid.setMatrixValue((row, col), value)
        val nextPosition = if (col == valueGrid.numCols - 1) (row + 1, 0) else (row, col + 1)
        solveKojun(updatedValueGrid, regionGrid, nextPosition, regionMap) match {
          case Some(solution) => Some(solution)
          case None => tryValues(row, col, valuesToTry.tail, valueGrid, regionGrid, regionMap)
        }
      } else {
        tryValues(row, col, valuesToTry.tail, valueGrid, regionGrid, regionMap)
      }
    }
  }

  /**
   * Resolve o Kojun a partir da uma matriz de valores (Int) e da matriz de regiões (Char).
   * Por ser uma função recursiva, recebe um ponto de partida (linha e coluna) para aplicar o backtracking. A função
   * normalmente é chamada externamente com (0, 0).
   * O retorno é um Option que pode ser None caso não exista solução ou Some caso exista.
   */
  @tailrec
  private def solveKojun(
    valueGrid: Matrix[Int],
    regionGrid: Matrix[String],
    position: Position,
    regionMap: mutable.HashMap[String, List[(Int, Int)]]
  ): Option[Matrix[Int]] = {
    val (row, col) = position
    if (row == valueGrid.numRows) {
      Some(valueGrid)
    } else if (col == valueGrid.numCols) {
      solveKojun(valueGrid, regionGrid, (row + 1, 0), regionMap)
    } else if (valueGrid.isValidPosition(position) && valueGrid.getMatrixValue(position) != 0) {
      solveKojun(valueGrid, regionGrid, (row, col + 1), regionMap)
    } else {
      val maxRegionSize = regionMap(regionGrid.getMatrixValue(position)).length
      tryValues(row, col, (1 to maxRegionSize).toList, valueGrid, regionGrid, regionMap)
    }
  }

  def solve(): Option[Matrix[Int]] = {
    /*
    // Testa os diferentes casos de canInsertValue
    println("Can insert value: (0,0), 1 (should be false)")
    println(canInsertValue((0, 0), 1, valueGrid, regionGrid, regionMap))
    println("Can insert value: (5,5), 3 (should be false)")
    println(canInsertValue((5, 5), 3, valueGrid, regionGrid, regionMap))
    println("Can insert value: (5,5), 1 (should be true)")
    println(canInsertValue((5, 5), 1, valueGrid, regionGrid, regionMap))
    println("Can insert value: (1,4), 3 (should be false)")
    println(canInsertValue((1, 4), 3, valueGrid, regionGrid, regionMap))
    println("Can insert value: (1,4), 4 (should be false)")
    println(canInsertValue((1, 4), 4, valueGrid, regionGrid, regionMap))
    println("Can insert value: (1,4), 6 (should be true)")
    println(canInsertValue((1, 4), 6, valueGrid, regionGrid, regionMap))
    println("Can insert value: (1,0), 2 (should be false)")
    println(canInsertValue((1, 0), 2, valueGrid, regionGrid, regionMap))
    println("Can insert value: (1,0), 3 (should be false)")
    println(canInsertValue((1, 0), 3, valueGrid, regionGrid, regionMap))
    */

    solveKojun(valueGrid, regionGrid, (0, 0), regionMap)
  }
}
