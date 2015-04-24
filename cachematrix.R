## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

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
