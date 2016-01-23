## Matrix inversion is usually computationally intensive process, as such, it makes
## sense to cache this process instead of repeating it.
## Below are a couple of functions that are used to create an object that stores 
## a matrix and caches the inverse

## MakeCacheMatrix makes an object that will cache the matrices' inverse.

MakeCacheMatrix <- function(x = matrix()) {
        #Set inverse to NULL until changed
        inv <- NULL 
        #Set function for the matrix but not the inverse
        set <- function(y) { 
                x <<- y
                inv <<- NULL
        }
        #Retreive the function
        get <- function() x
        #Manually set the inverse of Matrix
        setInverse <- function(inverse) inv <<- inverse
        #Retreives the inverse
        getInverse <- function() inv
        #Set it to a list
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function figures out the inverse of the matrix object created by 
## MakeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

CacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        #Retreive the value of the inverse, check if it's been cached and computed
        inv <- x$getInverse()
        #If it was, show the following message
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
