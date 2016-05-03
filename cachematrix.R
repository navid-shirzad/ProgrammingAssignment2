## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than computing it 
## repeatedly.
## The following pair of functions cache the inverse of a matrix.




## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL
     set <- function(y) {
          x <<- y
          inv <<- NULL
     }
     get <- function() x
     setinverse <- function(inverse) inv <<- inverse
     getinverse <- function() inv
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}



## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then cacheSolve should retrieve the inverse from the
## cache. Also, if det(matrix) = 0 the function returns NULL as inverse of the 
## matrix.
cacheSolve <- function(x, ...) {
     inv <- x$getinverse()
     if(!is.null(inv)) {
          message("getting cached data")
          return(inv)
     }
     data <- x$get()
     # check determinant of the matrix, if det is zero matrix does not have an 
     # inverse (function will return Null as inverse).
     if (det(data) == 0){
          print("matrix is not invertible")
          inv <- NULL
     }
     else {
          inv <- solve(data, ...)
     }
     #cache the inverse matrix and return its numeric value
     x$setinverse(inv)
     inv
}
