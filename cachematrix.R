## Maxtrix inversion is usually costly, especially when running inside a loop. 
## The following functions can compute and cache the inverse of a matrix so 
## that they can be looked up later instead of recomputing the inverse.

## Written by:P.Mousavi

## "makeCacheMatrix"function creates a special "matrix" object that can cache 
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
        ## @x: a square invertible matrix
        ## return: a list containing functions to
        ##              1. set the matrix
        ##              2. get the matrix
        ##              3. set the inverse
        ##              4. get the inverse
        ##         this list is used as the input to cacheSolve()
        inv <- NULL
        set <- function(y) {
                # use `<<-` to assign a value to an object in an environment 
                # different from the current environment. 
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## "cacheSolve" function computes the inverse of the "matrix" returned by 
## makeCacheMatrix(). If the inverse has already been calculated and the matrix
## has not changed, it'll retrieves the inverse from the cache directly.

cacheSolve <- function(x, ...) {
       
        ## @x: output of makeCacheMatrix()
        ## return: inverse of the original matrix input to makeCacheMatrix()
        
        inv <- x$getInverse()
       
         # if the inverse has already been calculated
        if(!is.null(inv)) {
                # get it from the cache and skips the computation.
                message("getting cached data")
                return(inv)
        }
        # otherwise, calculates the inverse 
        mat <- x$get()
        inv <- solve(mat, ...)
       
         # sets the value of the inverse in the cache via the setinv function.
        x$setInverse(inv)
        
        return(inv)
}
