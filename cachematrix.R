# These are a pair of functions that cache the inverse of a matrix  
# rather than compute it repeatedly 

# This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve){ m <<- solve}
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
  }


# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed),
# then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}

# Testing

aMatrix <- makeCacheMatrix(matrix(c(1,2,3,4),2,2)) # assign a matrix to aMatrix using the makeCacheMatrix function

cacheSolve(aMatrix) # calculate the aMatrix inverse using cacheSolve function

cacheSolve(aMatrix) # repet the calculation of aMatrix inverse. It returns the legend "getting cached data"

aNewMatrix <- makeCacheMatrix(matrix(c(2,3,4,5),2,2)) # assign a new matrix to aNewMatrix using the makeCacheMatrix function

cacheSolve(aNewMatrix) # calculate the aNewMatrix inverse using cacheSolve function

cacheSolve(aNewMatrix) # repet the calculation of aMatrix inverse. It returns the legend "getting cached data"

