## These functions are meant to cache the calculated inverse of a matrix in order to avoid 
## repeating potentially time-consuming calculations.

## The first function function, 'makeMatrix', creates a special "matrix" containing functions to:
##
## 1. set the matrix whose inverse is wanted into cache
## 2. get the matrix - return the matrix that is being worked with
## 3. set the inverse of the matrix into cache
## 4. get the inverse of the matrix - return the inverse of the matrix


## The second function, 'cacheSolve', calculates the inverse of the special "matrix" created with the
## function 'makeMatrix'. It first checks to see if the inverse has already been calculated, if so, it 
## retrieves that value using getInv. Otherwise, it calculates the inverse and stores it in the cache via
## setInv function.

## Create cache for storing matrix and its inverse: 

makeCacheMatrix <- function(x = matrix()) {
	Inv <- NULL                                   # initialize inverse to NULL 
	set <- function(y) {									
		x <<- y                                     # (re-)set the matrix 
		Inv <<- NULL                                # re-initialize the inverse
	}
	get <- function() x                           # return the matrix
	setInv <- function(inverse) Inv <<- inverse   # set the inverse
	getInv <- function() Inv                      # return the inverse
	list(set = set, get = get,
		setInv = setInv, getInv = getInv)           # returned special "matrix" of functions

}


## Calculate and store inverse:

cacheSolve <- function(x, ...) {
	inverse <- x$getInv()                   # retrieve current inverse
	if( !is.null(inverse) ){                # if inverse is cached return it
		message("getting cached inverse")     
		return(inverse)												
	}
	message("inverse has not been cached") 
	mat <- x$get()                          # retrieve cached matrix
	inverse <- solve(mat)                   # obtain inverse
	x$setInv(inverse)                       # cache the calculated inverse
	message("inverse cached")
	inverse                                 # return the inverse cached

}
