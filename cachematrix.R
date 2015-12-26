## Pair of functions makeCacheMatrix and cacheSolve stores in environment with matrix data also
## inverse matrix data. If inverse matrix is necessary for the first time, then it is caclculated
## and stored into cache. If inverse matrix is neccessary more than once, then cached inverse matrix 
## data is returned. 


## Function makeCacheMatrix adds to matrix 4 operations:
## set - stes the value of the matrix
## get - gets the value of the matrix
## setsolve - sets the value of the inverse matrix
## getsolve - gets the value of the inverse matrix


makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## Function cacheSolve tests, if there are already inverse matrix in cache. 
## If inverse matrix is found, then it is returned. If there is no inverse matrix 
## (it is asked for inverse matrix for first time), then inverse matrix is calculated
## and result is returned and inverse matrix also is saved in the environment.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    s <- x$getsolve()       ## take inverse matrix from cache
    if(!is.null(s)) {       ## cache contains inverse matrix - it is returned
        message("getting cached data")
        return(s)
    }
    data <- x$get()         ## cache does not contain inverse matrix
    s <- solve(data, ...)   ## inverse matrix is calculated
    x$setsolve(s)           ## calculated inverse matrix is saved in cache
    s
}
