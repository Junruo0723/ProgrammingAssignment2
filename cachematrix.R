##Matrix inversion is usually a costly computation and there may be some benefit to caching the 
##inverse of a matrix rather than compute it repeatedly.This assignment is to write a pair of 
##functions that cache the inverse of a matrix.

##makeCacheMatrix: This function create a special "matrix" object that can cache its inverse.
##In this part, we need to make matrix assign to variable x, and initialize inv to NULLs.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) { ##reset matrix
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve
##should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if (!is.null(inv)) { ##if same matrix had calculated before
    message("getting cached data")
    return(inv)  ##return result
  }
  mat <- x$get() ## get the new matrix
  inv <- solve(mat, ...) ##compute the inverse of a square matrix
  x$setInverse(inv)  ##assign inverse matrix
  inv  ##print
}
