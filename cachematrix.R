# Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of 
# a matrix rather than compute it repeatedly (there are also alternatives to matrix inversion that we will 
# not discuss here). Your assignment is to write a pair of functions that cache the inverse of a matrix.

## This first function creates a matrix that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv = NULL
        set = function(y) {
                x <<- y
                inv <<- NULL
        }
        get = function() x 
        setInv = function(inverse) inv <<- inverse 
        getInv = function() inv
        list(set = set, 
             get = get, 
             setInv = setInv, 
             getInv = getInv)
}

## This second function calculates the inverse of a matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv = x$getInv()
        
        
        if (!is.null(inv)) {
                # get it from the cache and skips the computation. 
                message("getting cached data")
                return(inv)
        }
        mat.data = x$get()
        inv = solve(mat.data, ...)
        x$setInv(inv)
        return(inv)
}

vector <- rnorm(100, mean = 3, sd = 2)
teste <- matrix(vector, 10, 10)
makeCacheMatrix(teste)


