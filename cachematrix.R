## Put comments here that give an overall description of what your
## functions do
## These functions solve for the inverse of a matrix and store the result in a cached 
## non-environment variable, thus saving the time to calculate the inverse of matrices
## that have already been solved.  See below for details of each function.



## Write a short comment describing this function
## function makeCacheMatrix initializes m to NULL and creates a list of 4 functions
## which include 1)set to store the supplied makeCacheMatrix argument in a cached 
## variable and to set the cached variable for storing the inverse m to NULL
## 2) get which retrieves the value stored in the cached variable x 
## 3) setinverse which stores the supplied argument in the cached variable m, used
##    to store the matrix inverse 
## 4) getinverse which retrieves the value in the cached variable m which contains the matrix inverse


makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function
## function cacheSolve retrieves the cached inverse result using the function getinverse
## if the cached inverse is null, the inverse of the matrix supplied as the argument 
##    is calculated using the solve function and then cached using the setinverse function
## if the cached inverse is not null the matrix inverse is returned


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
