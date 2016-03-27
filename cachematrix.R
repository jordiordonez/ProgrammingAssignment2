## Below are two functions that are used to create a special
## object that stores a matrix and caches its inverse

## This first function, makeCacheMatrix creates a special
## "vector", which is really a list containing a function to
## 1  set the value of the matrix
## 2  get the value of the matrix
## 3  set the value of the Inverse
## 4  get the value of the Inverse

makeCacheMatrix <- function(x = matrix()) {
		INV <- NULL
       set <- function(y) {
               x <<- y
               INV <<- NULL
       }
       get <- function() x
       setinv <- function(inv) INV <<- inv
       getinv <- function() INV
       list(set = set, get = get,
            setinv = setinv,
            getinv = getinv)
}


## The following function calculates the Inverse of the special
## "matrix" created with the above function. However, it first
## checks to see if the Inverse has already been calculated. If
## so,it gets the Inverse from the cache and skips the
## computation. Otherwise, it calculates the inverse of the
## matrix and sets the value of the Inverse in the cache via the
## setinv function.

cacheSolve <- function(x, ...) {
        INV <- x$getinv()
        if(!is.null(INV)) {
                message("getting cached data")
                return(INV)
        }
        mat <- x$get()
        INV <- solve(mat, ...)
        x$setinv(INV)
        INV
}
