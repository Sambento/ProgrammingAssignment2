## Programming Assignment 2 - Sambento
##
## This assignment intends to demonstrate the benefits of storing results of
## time-consuming computations through the use of the operator "<<-"
## that allows to assign values to symbols across different environments.

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL ## resets the symbol that stores the inverse of a matrix
  set <- function(y) { ## assigns an original matrix to special "matrix" object
    x <<- y
    m <<- NULL
  }
  get <- function() x ## returns the original matrix
  setinverse <- function(inverse) m <<- inverse
  ## assigns an inverse matrix to the special "matrix" object
  getinverse <- function() m ## returns the inverse matrix stored 
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned
## by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve
## the inverse from the cache.

cacheSolve <- function(x, ...) { ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse() ## gets inverse from the "matrix" object
  if(!is.null(m)) { ## if it's not NULL retrieves it from cache
    message("getting cached data")
    return(m)
  }
  matrix <- x$get() ## if it's NULL retrieves the original matrix
  m <- solve(matrix, ...) ## computes its inverse
  x$setinverse(m) ## stores the inverse in the matrix object
  m ## prints the inverse
}
