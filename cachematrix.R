
## Create vector of get/set functions for input matrix and imverted matrices
makeCacheMatrix <- function(x = matrix()) {
# Build a list of functions for access to
#   set() - assing matrix
#   get() - retrieve matrix
#   setsolve() - assign inversion
#   getsolve() - retrieve inversion
  
  i <- NULL # no inverted matrix
  
  set <- function(y) {
    x <<- y    # remember new matrix
    i <<- NULL # invalidate inverted one
  }
  get <- function() x 
  setsolve <- function(solve) i <<- solve 
  getsolve <- function() i
  
  list(set = set, 
       get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Return a matrix that is the inverse of 'xx'
cacheSolve <- function(xx, ...) {
  
  s <- xx$getsolve() 
  if(!is.null(s)) { # do we have cached inverted matrix?
    message("getting cached data")
    return(s) # yes, return cached invsed matrix
  }
  else { # calculate inversion now
    data = xx$get()
    s = solve(data, ...)
    xx$setsolve(s) # remember inverted matrix
    return(s) # return calculated inverted matrix
  }

}

# Test code
# if(exists("mat"))
#   rm(mat)
# mat <- matrix(rnorm(9), ncol=3, nrow=3)
# 
# mv <- makeCacheMatrix(mat)
# # First calculate and cache
# cacheSolve(mv)
# # Now use the cache. Expecting message "getting cached data"
# cacheSolve(mv)
