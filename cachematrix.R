## This group of functions calculates inverse matrices and creates a list that 
## hold functions used for caching matrices that have been loaded into the list.

## Function to create a list of functions that hold the cached inverse matrices
## in the function's environment.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
  
}


## This function returns solved inverse matrices.  First it checks for a cached inverse 
## matrix, if it does not exist it runs solve() to calculate a new one and then stores 
## it in the list created above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  mat <- x$get()
  m <- solve(mat,...)
  x$setsolve(m)
  m
  
}
