##Our aim in this experiment is to write a pair of functions, namely,
##"makeCacheMatrix" and "cacheSolve" that cache the inverse of a matrix

##makeCacheMatrix is a function which creates a special "matrix" object that can
##cache its inverse for the input (which is an invertible square matrix)

## Simply set the input x as a matrix
## Then set the solved value, "j" as a null
## then I changed every reference to "mean" to "solve"

makeCacheMatrix <- function(x = matrix()) {
  j <- NULL
  set <- function(y){
  x <<- y
  j <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) j <<- inverse
  getInverse <- function() j 
  list(set = set, get = get, 
  setInverse = setInverse, 
  getInverse = getInverse)
}


##cacheSolve is a function which computes the inverse of the special "matrix"
##returned by makeCacheMatrix above. If the inverse has already been calculated
##(and the matrix has not changed), then the cachesolve should retrieve the
##inverse from the cache

## Same here, changed "mean" to "solve" and "m" to "s"


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
         s <- x$getsolve()
  if(!is.null(s)) {
    message("getting inversed matrix")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}
