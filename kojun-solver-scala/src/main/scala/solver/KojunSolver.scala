package solver

import utils.Matrix

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
  private def canInsertValue(position: Position, value: Int): Boolean = {
    val (row, col) = position
    val regionValue = regionGrid.getMatrixValue(position)
    val regionPositions = regionMap(regionValue)
    
    // Faz uma lista de valores adjacentes à posição usando isValidPosition e getMatrixValue
    val adjacentesValues = List(
      (row - 1, col),
      (row + 1, col),
      (row, col - 1),
      (row, col + 1)
    ).filter(valueGrid.isValidPosition).map(valueGrid.getMatrixValue)
    
    // Imprime a lista de valores adjacentes para debug
    // println(s"Adjacent positions for $position: $adjacentesValues")

    // Se o valor acima estiver dentro da matriz e for da mesma região, será válido se o valor for menor que value
    val isTopValid = if (valueGrid.isValidPosition((row - 1, col)) && regionGrid.getMatrixValue((row - 1, col)) == regionValue) {
      value < valueGrid.getMatrixValue((row - 1, col))
    } else true

    // Se o valor abaixo estiver dentro da matriz e for da mesma região, será válido se o valor for maior que value
    val isBottomValid = if (valueGrid.isValidPosition((row + 1, col)) && regionGrid.getMatrixValue((row + 1, col)) == regionValue) {
      value > valueGrid.getMatrixValue((row + 1, col))
    } else true

    !(
      // Verifica se a posição já está ocupada por algum valor diferente de 0
      valueGrid.getMatrixValue(position) != 0 ||
      // Verifica se o número está entre 1 e N, onde N é o tamanho da região
      value < 1 || value > regionPositions.length ||
      // Verifica se algum dos valores adjacentes é igual ao valor que queremos inserir
      adjacentesValues.contains(value) ||
      // Verifica se o valor já está na região
      regionPositions.map(valueGrid.getMatrixValue).contains(value) ||
      // Verifica se o valor é maior que o valor da célula de cima (se estiver na mesma região)
      !isTopValid ||
      // Verifica se o valor é menor que o valor da célula de baixo (se estiver na mesma região)
      !isBottomValid
    )
  }

  /**
   * Resolve o Kojun a partir da uma matriz de valores (Int) e da matriz de regiões (Char).
   * Por ser uma função recursiva, recebe um ponto de partida (linha e coluna) para aplicar o backtracking. A função
   * normalmente é chamada externamente com (0, 0).
   * O retorno é um monad Maybe, que pode ser Nothing (caso não haja solução) ou Just Matrix Int (caso haja solução)
   * onde a matriz de inteiros representa a solução que foi encontrada para o Kojun.
   */
  private def backtrack(row: Int, col: Int): Option[Matrix[Int]] = {
    if (row == valueGrid.numRows) {
      Some(valueGrid) // Base case: Reached the end of the matrix, return the solution
    } else if (col == valueGrid.numCols) {
      // Move to the next row when reaching the end of the current row
      backtrack(row + 1, 0)
    } else if (valueGrid.getMatrixValue((row, col)) != 0) {
      // Skip to the next column if the position is already occupied
      backtrack(row, col + 1)
    } else {
      // Try inserting values from 1 to maxRegionSize
      def tryValues(values: List[Int]): Option[Matrix[Int]] = values match {
        // Caso não haja mais valores para tentar, retorna None e imprime onde parou, com qual valor e a matriz atual
        case Nil =>
          println(s"Backtracking at position ($row, $col)")
          println(s"Current value: ${valueGrid.getMatrixValue((row, col))}")
          valueGrid.printMatrix()
          None
        case value :: rest =>
          if (canInsertValue((row, col), value)) {
            // If the value can be inserted, update the matrix and continue with the next position
            val updatedGrid = valueGrid.setMatrixValue((row, col), value)
            backtrack(row, col + 1) match {
              case Some(result) => Some(result) // Solution found
              case None => tryValues(rest) // Continue trying the next value
            }
          } else {
            // Value cannot be inserted, try the next value
            tryValues(rest)
          }
      }

      val currentRegionSize = regionMap(regionGrid.getMatrixValue((row, col))).length
      tryValues(1 to currentRegionSize toList)
    }
  }

  def solve(): Option[Matrix[Int]] = {
    // solveKojun(0, 0, regionGrid.numRows)

    // Tenta inserir valores de teste e imprime se foi possível ou não
    /* println("Can insert value: (0,0), 1 (should be false)")
    println(canInsertValue((0, 0), 1))
    println("Can insert value: (5,5), 3 (should be false)")
    println(canInsertValue((5, 5), 3))
    println("Can insert value: (5,5), 1 (should be true)")
    println(canInsertValue((5, 5), 1))
    println("Can insert value: (1,4), 3 (should be false)")
    println(canInsertValue((1, 4), 3))
    println("Can insert value: (1,4), 4 (should be false)")
    println(canInsertValue((1, 4), 4))
    println("Can insert value: (1,4), 6 (should be true)")
    println(canInsertValue((1, 4), 6))
    println("Can insert value: (1,0), 2 (should be false)")
    println(canInsertValue((1, 0), 2))
    println("Can insert value: (1,0), 3 (should be false)")
    println(canInsertValue((1, 0), 3)) */

    // Chama a função de backtracking recursiva
    backtrack(0, 0)
  }
}