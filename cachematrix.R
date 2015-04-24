## These functions take a matrix and store in function scope and then cache the
##  the inverse of the matrix.

## makeCacheMatrix takes a matrix and returns a function list 
##  Functionlist contains set, get to set and get the input matrix
##  and setInvertedMatrix and getInvertedMatrix to get / set the inversion of the 
##  input matrix within the scope of the function.
makeCacheMatrix <- function(storedMatrix = matrix()) {
  storedInvertedMatrix <- NULL
  set <- function(newVector) {
    storedMatrix <<- newVector
    storedInvertedMatrix <<- NULL
  }
  get <- function() storedMatrix
  setInvertedMatrix <- function(invertedMatrix) storedInvertedMatrix <<- invertedMatrix
  getInvertedMatrix <- function() storedInvertedMatrix
  list(set = set, get = get,
       setInvertedMatrix = setInvertedMatrix,
       getInvertedMatrix = getInvertedMatrix)
}


## cacheSolve takes the funcList from makeCacheMatrix and either returns the 
## cached data or it calculates the invertedMatrix, stores and then returns it.

cacheSolve <- function(funcList, ...) {
  storedInvertedMatrix <- funcList$getInvertedMatrix()
  if(!is.null(storedInvertedMatrix)) {
    message("getting cached data")
    return(storedInvertedMatrix)
  }
  data <- funcList$get()
  storedInvertedMatrix <- solve(data, ...)
  funcList$setInvertedMatrix(storedInvertedMatrix)
  storedInvertedMatrix
}
