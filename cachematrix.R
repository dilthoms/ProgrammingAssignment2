# Create a special object to make matrix inverse computation
# faster when the matrix does not change by caching the earlier
# result. 

# This code is modified from the example version of caching
# mean of a vector available in the R programming course webpage


# makeCacheMatrix creates a special matrix which has the ability
# to cache its inverse. makeCacheMatrix creates a list of functions namely:
# 1. set() - set the matrix
# 2. get() - the matrix
# 3. set the inverse of the matrix
# 4. get the cached inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {

	# initialize cachedinv to null to indicate cache is invalid 
	cachedinv <- NULL
	
	# if the matrix changes, the new matrix can be set using this function.
	# Invalidate the cache after setting the new matrix.
	set <- function(y) {
		mat <<- y
		cachedinv <<- NULL
	}

	# retrieve the stored matrix
	get <- function() mat
	
	# store the inverse in the cache
	setinverse <- function(inv) cachedinv <<- inv

	# retireve the inverse stored in the cache
	getinverse <- function() cachedinv

	# the set of functions associated with this special matrix object	
	list(set = set, get = get,
	     setinverse = setinverse,
	     getinverse = getinverse)

}


# cacheSolve does the job of returning the inverse. If the inverse 
# exists in the cache, the value of the cache is returned. Otherwise
# the inverse is computed using solve and stored in the cache.
cacheSolve <- function(x, ...) {

	# retrieve the value of inverse in the cache
	inv <- x$getinverse()
	
	# check if the cache is valid
	if(!is.null(inv)) {

		# if valid, return the cached value of inverse
		return(inv)
	}

	# if the cache is not valid, get the matrix and compute inverse
	data <- x$get()
	
	# solve does the actual job of computing the inverse
	inv <- solve(data, ...)

	# once solved, store the inverse in the cache
	x$setinverse(inv)
	
	#Finally, return the inverse
	inv
}
