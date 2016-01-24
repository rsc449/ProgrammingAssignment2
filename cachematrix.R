## These functions are used to prevent the need to recalculate a matrix inverse 
## every time it is used. The first time cacheSolve(x) is called, the x matrix
## object will not contain the inverse, so cacheSolve will calculate it and 
## store it in the object x. The next cacheSolve is called, x will have a cached
## inverse and it will simply be returned.


## Example Usage
##
##      data = matrix(rnorm(4*4), 4, 4) 
##      x = makeCacheMatrix(data)
##      xInverse1 = cacheSolve(x)   ## first time cacheSolve calculates inverse
##      xInverse2 = cacheSolve(x)   ## second time cacheSolve retrieves inverse
##                                  ## from x
##      ident = data %*% xInverse1  ## matrix times its inverse is the identity


## This function creates a matrix object which contains the matrix data, matrix 
## inverse data, functions to get, set the matrix data and matrix inverse.

makeCacheMatrix <- function(x = matrix()) {
    
    mInv = NULL
    
    ## Set a new matrix to the matrix object also reseting the cached inverse 
    ## to NULL
    set <- function(y) {
        x <<- y
        mInv <<- NULL
    }
    
    ## Get the matrix x
    get <- function() x
    
    ## cache the matrix inverse
    setInverse <- function(mInverse) mInv <<- mInverse
    
    ## get the matrix inverse
    getInverse <- function() mInv
    
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## cachSolve returns the inverse of a matrix. 
## The argument to cacheSolveis a matrix object defined by makeCacheMatrix 
## if the matrix object has a valid cached inverse then cache solve will simply 
## return the cached value. Otherwise it will calculate the inverse, store it 
## in the object and return the inverse.

cacheSolve <- function(x, ...) {
    
    ## check to see if the matrix object already has a cached inverse if so,
    ## just return it.
    mInv <- x$getInverse()
    if(!is.null(mInv)) {
        message("getting cached data")
        return(mInv)
    }
    
    ## Since the inverse has not been cached, calculate it and the cache it in 
    ## the matrix object.
    data = x$get()
    mInv = solve(data)
    x$setInverse(mInv)
    mInv
}
