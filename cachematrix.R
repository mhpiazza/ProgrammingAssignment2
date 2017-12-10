## makeCacheMatrix: creates a special "matrix" object that can cache its inverse.
## 
## cacheSolve: computes the inverse of the special "matrix" returned by makeCacheMatrix. 

## this function sets the value of the cached matrix
## and gets the value of the inverse matrix
## whenever x is reset, the cached value of m is cleared
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        set_inv_matrix <- function(solve) m <<- solve
        get_inv_matrix <- function() m
        list(set = set, 
             get = get,
             set_inv_matrix = set_inv_matrix,
             get_inv_matrix = get_inv_matrix)
}

## this function will check to see if the inverse matrix for 'x' exists 
## and if it does it will get from cache
## otherwise it will create the inverse matrix
cachesolve <- function(x, ...) {
        m <- x$get_inv_matrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$set_inv_matrix(m)
        m
}
