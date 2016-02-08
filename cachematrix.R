## Two functions that create a matrix and return de inverse. The inverse is calculated and stored in the cache, if it asked again the functions don´t 
## calculate it again it just bring it from the cache

## This function creates a special object, which is really a list containing a function to set the value of the matrix, get the value of the matrix, set the value
## of the inverse matrix, and get the value of the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
                                           i   <- NULL
                                           set <- function(y) {
                                                               x <<- y
                                                               i <<- NULL
                                                               }
                                           get        <- function() x
                                           setInverse <- function(inverse) i <<- inverse
                                           getInverse <- function() i
                                           list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
                                          }

## This function calculates the inverse of the matrix created with the above function. However, it first checks to see if the inverse has already been calculated. If 
## so, it gets the inverse from the cache and skips the computation. Otherwise, it calculates the inverse of the matrix pass throught and sets the value of the inverse
## in the cache via the setinverse function.


cacheSolve <- function(x, ...) {
                                i <- x$getInverse()
                                if(!is.null(i)) {
                                                 message("getting cached data")
                                                 return(i)
                                                 }
                                data <- x$get()
                                i    <- solve(data, ...)
                                x$setInverse(i)
                                i
                                }