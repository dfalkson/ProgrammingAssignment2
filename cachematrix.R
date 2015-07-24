## The following functions allow caching a matrix and its inverse,
## and calculating the inverse of the matrix or retrieving the inverse
## matrix from the cache.

## The following function returns a list of functions, that
## respectively allows setting the value of a matrix, retrieving
## the value of the matrix, setting the inverse matrix that corresponds
## to the matrix, and retrieving the inverse matrix that corresponds
## to the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv<- NULL
        setmat <- function (y) {
                x <<- y
                inv <<- NULL
        }
        getmat <- function () x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function () inv
        list (setmat = setmat, getmat = getmat,
              setinv = setinv, getinv = getinv)
}


## The following function calculates the inverse of the matrix
## created with the function above. It first checks to see if
## the inverse matrix was already calculated and cached, if not
## it calculates the inverse matrix.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$getmat()
        inv <- solve(data)
        x$setinv(inv)
        inv
}
