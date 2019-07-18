## functions that determine if a matrix inverse has been previously
## caluclated and if so, then used cached version to save computation and demonstrate
## scoping in R


## Write a short comment describing this function

## Function creates a special “matrix", containing a list of functions to:
## set and get the value of the matrix, and set , get the value of the matrix inverse


makeCacheMatrix <- function(x = matrix()) {
  ##  create a variable object to store inverse
  ## initialize the value of the matrix inverse variable  to NULL
  invers <- NULL
  ##  delcare 'set' function and set y to store matrix value that is to be cached .
  set <- function(y) {
    x <<- y
    invers <<- NULL ## reset inverse matrix variable to null.
  }
  get <- function() x
  setInvr <- function(solve) invers <<- solve # r 'solve' function to calculate inverse of a square matrix
  getInvr <- function() invers
  list(set = set,get = get, setInvr = setInvr,getInvr = getInvr)
}


## function to determine if inverse matrix object has been created and if so
## then use the cached matrix version function and reduce computation
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInvr()
  ## if this is not the first call to this matrix inverse then used cached function
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  res_matrix <- x$get()
  inv <- solve(res_matrix, ...)   ## use R 'solve' function to calculate square matrix inverse
  x$setInvr(inv)
  inv
}