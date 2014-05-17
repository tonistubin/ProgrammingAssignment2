## Here we calculate and cache the inverse of a square and invertible matrix 

## makeCacheMatrix creates a special matrix that can cache its inverse.
## It returns functions for setting and getting the value of the matrix
## as well as setting and getting its inverse value

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {                      
                x <<- y
                m <<- NULL
        }
        get <- function() x                       
        setmatrix <- function(solve) m <<- solve  
        getmatrix <- function() m                 
        list(set = set, get = get,                ## return function list 
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}


## This function calculates the inverse of the matrix 
## created by makeCacheMatrix

cacheSolve <- function(x, ...) {
        m <- x$getmatrix()     ## check if inverse has already been calculated
        if(!is.null(m)) {      ## if inverse already exists, get the cached value
                message("getting cached data")
                return(m)     
        }
        data <- x$get()
        m <- solve(data, ...)  ## calculate inverse of matrix
        x$setmatrix(m)         ## cache the inverse value
        m
}
