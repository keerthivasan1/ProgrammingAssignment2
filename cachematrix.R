## This program writes a set of functions to calculate the inverse of a matrix
## and stores the calculated data in the cache to prevent repeated computations

## makeCacheMatrix is the main function that gets the matrix and calls the solve
## function to calculate the inverse and stores the calculated data after checking
## to make sure that the data is not already stored in the cache.

makeCacheMatrix <- function(x = matrix()) {
             +     inv <- NULL
+     set <- function(y) { ## function to store the matrix data in the cache
+         x <<- y
+         inv <<- NULL ## indicates that inverse has not yet been calculated for new data
+     }
+     get <- function() x ## gets the matrix data
+     setinv <- function(inverse) inv <<- inverse ## stores the inverse data to cache
+     getinv <- function() inv ##gets the inverse data from the cache
+     list(set = set, get = get,
+          setinv = setinv,
+          getinv = getinv)

}


## cacheSolve function is the function used to calculate the inverse of the matrix
## while returing the inverse data if it is already stored in the cache

cacheSolve <- function(x, ...) {
+     inv <- x$getinv() ## gets the inverse data from the cache
+     if(!is.null(inv)) { ## checks if the inverse is already calculated
+         message("getting cached data")
+         return(inv) ## returns the inverse data from cache if it already exists
+     }
+     data <- x$get()
+     inv <- solve(data, ...) ## calculates the inverse data from the matrix
+     x$setinv(inv) ## sets the calculated inverse matrix data in the cache
+     inv        ## Return a matrix that is the inverse of 'x'
}
