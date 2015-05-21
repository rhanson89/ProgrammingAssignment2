

## Creates a matrix which can store the value of its inverse

makeCacheMatrix <- function(x = matrix()) {
      ## input is inversible matrix
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      ## 1st function sets the value of the matrix
      get <- function() x
      ## 2nd function gets the value of the matrix
      setInv <- function(solve) inv <<- solve
      ## 3rd function sets the value of the inverse
      getInv <- function() inv
      ## 4th function gets the value of the inverse
      list(set=set, get=get, setInv=setInv, getInv=getInv)
}


## cacheSolve is used to get the inverse of the input matrix, either by
##calculating it or retrieving an already calculated value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inv <- x$getInv()
      if(!is.null(inv)) {
            message("retrieving cached data")
            return(inv)
      }
      ## if the inverse is already calculated it retrieves it from cache
      data <- x$get()
      inv <- solve(data,...)
      x$setInv(inv)
      inv
      ## otherwise the inverse is calculated
}
