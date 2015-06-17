## Check this out: http://masterr.org/r/how-to-cache-a-matrix-inversion-in-r/

## Create vector of get/set functions for input matrix and imverted matrices
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  set <- function(y) {
    x <<- y    # remember new matrix
    i <<- NULL # invalidate inversed one
  }
  get <- function() x # retrieve matrix
  setsolve <- function(solve) i <<- solve
  getsolve <- function() i
  
  list(set = set, 
       get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Return a matrix that is the inverse of 'xx'
cacheSolve <- function(xx, ...) {
  
  s <- xx$getsolve() # do we have cached solution?
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  else {
    data = xx$get()
    s = solve(data, ...)
    xx$setsolve(s) # remember solved matrix
    return(s)
  }

}


if(exists("mat"))
  rm(mat)
mat <- matrix(rnorm(9), ncol=3, nrow=3)

mv <- makeCacheMatrix(mat)
cacheSolve(mv)
cacheSolve(mv)
