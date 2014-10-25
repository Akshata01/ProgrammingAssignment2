## The two functione below are used to catche the inverse of a matrix.
## Use: Computing the inverse of a matrix is computationally intensive and hence
## it makes sense to catche the inverse value, especially for large matrices.

## This function does gets and sets the value of the matrix.
## It also gets and sets the value of the matrix inverse


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## This function checks if the matrix inverse is already catched. 
## If so, it picks up the value from the catch. 
## Otherwise, it computes the new inverse.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
}


