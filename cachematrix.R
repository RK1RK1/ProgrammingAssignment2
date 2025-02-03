## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##makeCacheMatrix function: It initializes a matrix x and a cached inverse matrix inv. 
##The set function updates the matrix x and clears the cached inverse, inv. 
##The get function retrieves the matrix, and the setinverse and getinverse functions store and retrieve the cached inverse, respectively.
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
##cacheSolve function: It first checks if the inverse is already cached by calling getinverse. 
##If cached, it returns the inverse matrix with a message indicating that cached data is being used. 
##If not cached, it calculates the inverse using the solve function and stores it with setinverse.
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
