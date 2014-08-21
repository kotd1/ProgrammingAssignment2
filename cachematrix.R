### These two functions take an invertible matrix, make it cacheable
### inverse the matrix, and return it inversed, either from cache or directly from computation.


## The makeCacheMatrix() function takes a matrix and creates a special object - a "matrix" that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {   # the function's input is an invertable matrix
  s <- NULL                             # s for "solved", resetting the inverted matrix value
  set <- function(y) {                  # a function that would allow to recreate the invertable matrix, with a new value (y)
    x <<- y                             # superassigning a new value to x
    s <<- NULL                          # resetting the cached inverted matrix, using superassignment
  }                     
                                        # The following funcions would be called by the cacheSolve function,
                                        # or the user:
  get <- function() {x}                 # get - this function returns the original, non-inverted matrix
  setinv <- function(solve) s <<- solve # setinv - this function will cache the inverted matrix 
                                        #   in the first run of cacheSolve, using a superassignment
  getinv <- function() {s}              # getinv - a function that returns the cached inverted matrix
  list(set = set, get = get,            # a list of the created functions that makes them accessiable
       setinv = setinv,
       getinv = getinv)
}


## this function uses the makeCacheMatrix to invert the matrix, cache it and return it. 
## its input is the output of makeCacheMatrix()
  

cacheSolve <- function(x, ...) {         # its input in makeCacheMatrix's output
                                         
  s <- x$getinv()                       # calling the getinv function to give s the value of the cached matrix
  if(!is.null(s)) {                     # checking if the matrix was already cached.  if it was:
    message("getting cached data")      # printing this message
    return(s)                           # returning the cached matrix 
  }
  data <- x$get()                       # if there was no cached inverted matrix - 
  s <- solve(data, ...)                 # the matrix gets caculated
  x$setinv(s)                           # cached
  s                                     # returning the inverted matrix
}
