makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      } # sets x
      get <- function() x #gets x
      setinverse <- function(inverse) m <<- inverse #sets the inverse of x
      getinverse <- function() m #gets the inverse of x
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
      m <- x$getinverse()
      if(!is.null(m)) {
            message("getting the inverse of x")
            return(m)
      }
      matrix <- x$get()
      m <- solve(matrix,...)
      x$setinverse(m)
      m
}
