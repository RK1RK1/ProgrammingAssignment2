## Put comments here that give an overall description of what your
## functions do
##function that is able to cache potentially time-consuming computations. For
##example, taking the mean of a numeric vector is typically a fast
##function that is able to cache potentially time-consuming computations.
##For example, taking the mean of a numeric vector is typically a fast
##operation. However, for a very long vector, it may take too long to
##in a loop). If the contents of a vector are not changing, it may make
##sense to cache the value of the mean so that when we need it again, it
##can be looked up in the cache rather than recomputed. In this
####language and how they can be manipulated to preserve state inside of an
##R object.
##the R language and how they can be manipulated to preserve state inside
##of an R object.
## Write a short comment describing this function
##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
         inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## Write a short comment describing this function
##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

##Computing the inverse of a square matrix can be done with the solve function in R. For example, if X is a square invertible matrix, then solve(X) returns its inverse.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
