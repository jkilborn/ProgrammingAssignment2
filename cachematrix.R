## The following set of functions will create a cache for matrix inversions.
## If a matrix is invertible and the inverse of the matrix has already been 
## calculated then the set of functions will return the value for the inverse
## that is stored in the cache rather than recalculating a potentially time-
## consuming inverse of the matrix.  If it has not been calculated then once it
## has been the answer gets stored in the cache.
## Put comments here that give an overall description of what your
## functions do
        
        
## The 'makeCacheMatrix' function will create a special object which will be 
## used in the 'cacheSolve' function.  The special object is a list containing 
## four elements, each of which is a function and an associated environment.
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinvert <- function(invert) inv <<- invert
    getinvert <- function() inv
    list(set = set, get = get,
         setinvert = setinvert,
         getinvert = getinvert)
}


## The 'cacheSolve' function takes the elements of the list produced in the 
## 'makeCacheMatrix' function and checks to see if the inverse solution to the 
## original matrix has already been calculated.  If so, it returns the value 
## stored in the cache.  Otherwise, it calculates the inverse of the matrix and
## what inverse now gets stored in the cache.
## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    
    ## Return a matrix that is the inverse of 'x'
    
    inv <- x$getinvert()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinvert(inv)
    inv
    ## Return a matrix that is the inverse of 'x'
}