## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## this function cache the inverse of matrix using a temporary matrix

makeCacheMatrix <- function(x = matrix()) {
        ## initiate a matrix
        m <- NULL
        ## set matrix x
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        ## get matrix x
        get <- function() x
        ## set inverse of matrix x
        setinverse <- function(inverse) m <<- inverse
        ## get inverse of matrix x
        getinverse <- function() m
        ## list internal functions
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
## this function computes the inverse of the matrix return by makeCacheMatrix
## if the inverse has been calculated and the matrix has not change
## then the cacheSolve retrieve the inverse from the cache
## otherwise it calculates the inverse and set and return the result

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## get the inverse of matrix x
        m <- x$getinverse()
        ## estimate whether the inverse is calculated or not
        if(!is.null(m)){
                message("getting cache inverse")
                return(m)
        }
        ## the inverse has not been calculuted
        ## calculate the inverse and set and return the result
        data <- x$get()
        ## calculated the inverse
        m <- solve(data, ...)
        ## set and return the result
        x$setinverse(m)
        m
}
