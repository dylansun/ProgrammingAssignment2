## In this example we introduce the <<- operator which can be used to assign a value 
## to an object in an environment that is different from the current environment.
## Below are two functions that are used to create a special object that stores a 
## matrix and cache's its invers.

## The first function, makeCacheMatrix creates a special "vector", which is really a list 
## containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  invs <- NULL
  set <- function(y) {
    x <<- y
    invs <<- NULL
  }
  get <- function() x
  setinvs <- function(matrix_invs) invs <<- matrix_invs
  getinvs <- function() invs
  list(set = set, get = get,
       setinvs = setinvs,
       getinvs = getinvs)
}


## The following function calculates the inverse of the special "matrix" created with t
## he above function. However, it first checks to see if the invs has already been calculated.
## If so, it gets the invs from the cache and skips the computation. Otherwise, it calculates
## the invs of the data and sets the value of the invs in the cache via the setinvs function.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invs <- x$getinvs()
  if(!is.null(invs)) {
    message("getting cached data")
    return(invs)
  }
  data <- x$get()
  invs <- solve(data)
  x$setinvs(invs)
  invs
}

