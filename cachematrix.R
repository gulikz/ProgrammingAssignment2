## Assignment, 3 week - R Programming
## Lexical Scoping

##1. makeCacheMatrix. The function creates a special "matrix" object that 
##can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function (y) {
      x <<- y
      inv <<- NULL
    }
    get <- function () x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function () inv
    list (set=set, 
          get=get, 
          setInverse=setInverse, 
          getInverse=getInverse)
}


##2. cacheSolve. The function computes the inverse of the special "matrix" returned by makeCacheMatrix 
##above. If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if(!is.null(inv)) {
          message("cached data.")
          return (inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setInverse(inv)
        inv
}

x <- matrix(2:5, 2, 2)
m <- makeCacheMatrix(x)
m$get()
cacheSolve(m)
m$getInverse()




