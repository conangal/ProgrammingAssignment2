## The purpose of the pair of functions is to improve efficiency when repeatedly
## computing the inverse of one or many matrices. They achieve this by caching
## the inverse of the matrix and allowing the retrieval of the inverse instead
## of re-computation


## This function generates 'matrix object' which is made up of a list of four 
## functions which allows a matrix and it's inverse to be cached in memory.
## The inverse matrix, b, is reset to NULL when a new matrix is passed into 
## the function using the set sub-function.

makeCacheMatrix <- function(x = matrix()) {

    # initialise inverse matrix as null
    b <- NULL
    
    # set and get allow the matrix to be set and retrieved
    set <- function(y) {
        x <<- y
        b <<- NULL
    }
    get <- function() x
     
    # setinv and getinv allow the inverse to be set and retrieved
    setinv <- function(inv) b <<- inv
    getinv <- function() b
    
    # outputs list of the 4 functions
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
    
}


## This function takes a 'matrix object' as per the makeCacheMatrix function
## as input. It takes the cached value of the inverse and returns it along with 
## message 'getting cached data'. If the inverse has not previously been cached
## it will be stored as NULL and the function will compute it. It will then 
## pass the inverse matrix to the cache and return the inverse matrix
cacheSolve <- function(x, ...) {

    #inverse is retrieved from 'matrix object', x
    b <- x$getinv()

    # if the inverse isn't null it is returned
    if(!is.null(b)) {
        message("getting cached data")
        return(b)
    }
    
    # the inverse is calculated if the inverse is null
    data <- x$get()
    b <- solve(data, ...)

    # value of the inverse is passed to the cache and returned
    x$setinv(b)
    b

}