# funkcja makeCacheMatrix tworzy specjalny obiekt "macierz", 
# który może przechowywać swoją odwrotność

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL 
  set <- function(y) {
    x <<- y 
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

# funkcja cacheSolve oblicza odwrotność macierzy stworzonej przez pierwszą funkcję. 
# Jeśli odwrotność została już wcześniej obliczona, 
# funkcja pobiera ją bezpośrednio z pamięci podręcznej

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matrix <- x$get()
  inv <- solve(matrix, ...)
  x$setinverse(inv)
  inv
}

macierz <- matrix(c(1,2,3,4), ncol = 2, nrow = 2)

# Oryginalna macierz
macierz

cache <- makeCacheMatrix(macierz)

# Pierwsze wywołanie funkcji cacheSolve - jeszce nie ma obiektu w pamięci
cacheSolve(cache)

# Dugie wywołanie cacheSolve - już jest obiekt w pamięci
cacheSolve(cache)



