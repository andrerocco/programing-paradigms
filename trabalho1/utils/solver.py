# Kojun Solver

# Gera recursivamente um set com todas as posições da região que contém a posição (row, col)
def get_region_from_position(region_grid, region, row, col, visited=set()) -> set:
    if (row < 0 or row >= len(region_grid) or col < 0 or col >= len(region_grid[0]) or
            region_grid[row][col] != region or (row, col) in visited):
        return list(visited)

    visited.add((row, col))

    # Recursão para cima, baixo, esquerda e direita
    get_region_from_position(region_grid, region, row - 1, col, visited)
    get_region_from_position(region_grid, region, row + 1, col, visited)
    get_region_from_position(region_grid, region, row, col - 1, visited)
    get_region_from_position(region_grid, region, row, col + 1, visited)

    return list(visited)

# 1. Inserir um número em cada célula da grade para que cada região de tamanho N contenha cada número de 1 a N exatamente uma vez.
# 2. Os números em células ortogonalmente adjacentes devem ser diferentes.
# 3. Se duas células são adjacentes verticalmente na mesma região, o número na célula superior deve ser maior que o número na célula inferior.
def is_valid(value_grid, region_grid, row, col, value):
    # Obtém a região da posição (row, col)
    region = region_grid[row][col]

    # Calcula o tamanho da região
    region_size = sum(row.count(region) for row in region_grid)

    # Verifica se o número está entre 1 e N, onde N é o tamanho da região
    if value < 1 or value > region_size:
        return False

    # Verifica se alguma célula adjacente tem o mesmo valor
    if (row > 0 and value_grid[row - 1][col] == value):
        return False
    if (row < len(value_grid) - 1 and value_grid[row + 1][col] == value):
        return False
    if (col > 0 and value_grid[row][col - 1] == value):
        return False
    if (col < len(value_grid) - 1 and value_grid[row][col + 1] == value):
        return False
    
    # Verifica se a célula de cima é da mesma região
    # Se sim, garante que o valor seja maior
    if (row > 0 and region_grid[row - 1][col] == region_grid[row][col] and value_grid[row - 1][col] <= value):
        return False
    
    # Verifica se a célula de baixo é da mesma região
    # Se sim, garante que o valor seja menor
    if (row < len(value_grid) - 1 and region_grid[row + 1][col] == region_grid[row][col] and value_grid[row + 1][col] >= value):
        return False

    # Verifica se a região já possui o valor
    # TODO - Otimizar essa parte
    for i in range(len(value_grid)):
        for j in range(len(value_grid)):
            if (region_grid[i][j] == region_grid[row][col] and value_grid[i][j] == value):
                return False

    return True

def solve(value_grid, region_grid, row=0, col=0):
    if row == len(value_grid):
        return True
    elif col == len(value_grid[0]):
        return solve(value_grid, region_grid, row + 1, 0)
    elif value_grid[row][col] != 0:
        return solve(value_grid, region_grid, row, col + 1)
    else:
        for value in range(1, len(value_grid) + 1):
            if (is_valid(value_grid, region_grid, row, col, value)):
                value_grid[row][col] = value
                if (solve(value_grid, region_grid, row, col + 1)):
                    return True
                value_grid[row][col] = 0
        return False

def solve_puzzle(value_grid, region_grid):
    if (solve(value_grid, region_grid, 0, 0)):
        return value_grid
    else:
        return None
    
def print_grid(grid):
    for row in grid:
        print(row)

def check_solution(value_grid, region_grid, solution_grid):
    def is_valid_cell(row, col):
        region = region_grid[row][col]
        value = solution_grid[row][col]

        # Verifica se o valor está dentro dos limites da região
        region_size = sum(row.count(region) for row in region_grid)
        if value < 1 or value > region_size:
            return False

        # Verifica se os vizinhos possuem valores diferentes
        neighbors = [(row-1, col), (row+1, col), (row, col-1), (row, col+1)]
        for neighbor_row, neighbor_col in neighbors:
            if 0 <= neighbor_row < len(solution_grid) and 0 <= neighbor_col < len(solution_grid[0]):
                if solution_grid[neighbor_row][neighbor_col] == value:
                    return False

        # Verifica a regra da célula superior e inferior na mesma região
        if row > 0 and region_grid[row - 1][col] == region and value_grid[row - 1][col] <= value:
            return False

        return True

    # Verifica cada célula
    for row in range(len(solution_grid)):
        for col in range(len(solution_grid[0])):
            if value_grid[row][col] != 0 and not is_valid_cell(row, col):
                return False

    return True

def main():
    value_grid = [
        [0,0,4,0,2,0],
        [0,0,3,0,0,0],
        [1,4,0,4,0,0],
        [0,5,0,0,0,2],
        [0,0,0,0,3,0],
        [6,2,0,2,0,5],
    ]
    region_grid = [
        ['a','b','b','b','c','d'],
        ['a','e','b','c','c','c'],
        ['a','a','f','c','g','g'],
        ['h','i','f','j','j','g'],
        ['h','i','i','k','k','g'],
        ['i','i','i','k','k','k'],
    ]

    # Solve
    solution = solve_puzzle(value_grid, region_grid)

    if (solution == None):
        print("No solution found")
    else:
        print("Solution:")
        print_grid(solution)

    if (check_solution(value_grid, region_grid, solution)):
        print("Solution is valid")
    else:
        print("Solution is invalid")

if __name__ == "__main__":
    main()