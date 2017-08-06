## Caching the Inverse of a Matrix
## functions do 

##  "makeCacheMatrix": This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
   
          inv <- NULL
          set <- function(y) {
            x <<- y
            inv <<- NULL
         }
       get <- function() x
       setInverse <- function(inverse) inv <<-inverse
       getInverse <- function() inv
       list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}
  > 
## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
            
          inv <- x$getInverse()
          if ( ! is.null(inv)) {
                print("getting cached data")
                return(inv)
               }
          data <- x$get()
          inv <- solve(data, ...)
          x$setInverse(inv)
          inv
 }
