# Práctica 1 de IA en IO: Resolución de Problemas con Algoritmos de Búsqueda

Este repositorio contiene la implementación en R de los algoritmos de búsqueda explicados en la parte de teoría de la asignatura, con el fin de que sirva de apoyo para los estudiantes. En concreto, los algoritmos de búsqueda que se incluyen son:

- búsqueda en profundidad (DFS),
- búsqueda en anchura (BFS),
- algoritmo A\*.

También se incluye la implementación en R de un ejemplo de resolución de los problemas incluidos en el guion de la práctica.

- búsqueda normal,
- multibúsqueda,
- problema de las *n*-reinas.

## Instalación

Puede instalar las implementaciones en R indicadas a través del paquete devtools de R:

```r
	install.packages("devtools")
	devtools::install_github("genimarca/2022-IAIO-P1")
```

## Ejecución

El nombre de la librería/paquete es Practica1. Para poder ejecutar todas las funciones de la librería sin tener que indicar siempre su nombre debe primeramente cargar la librería en memoria:

```r
library("Practica1")
```

Obtención de la representación de un problema:

```r
	getBoard()
	getMultiBoard()
	getNQueens()
```

Ejecución de los algoritmos de búsqueda disponibles:

```r
	bfs()
	dfs()
	astar()
```

Nota: El algoritmo A\* tiene dos parámetros de entrada:

- el problema a resolver,
- la heurística a usar.

Las heurísticas disponibles son:

- distancia euclídea (euclidean),
- distancia euclídea al cuadrado (euclidean2),
- distancia de Manhattan (manhattan),
- distancia del tablero de ajedrez (chessboard).
- coste uniforme (ucs).

La visualización el camino desde el estado final al objetivo se realiza con la función: *representPath*.

Ejemplo de uso para resolver un problema de búsqueda simple y conseguir una representación gráfica:

```r
	board <- getBoard()
	results.bfs <- bfs(board)
	representPath(results.bfs$problem, results.bfs$path)$img
```

#### Créditos

El material original fue preparado por Jacinto Carrasco Castillo ([@JacintoCC](https://github.com/JacintoCC)) para la asignatura de IA en Investigación Operativa del Grado de Estadística de la Universidad de Granada adscrita al departamento de Ciencias de la Computación e Inteligencia Artificial.