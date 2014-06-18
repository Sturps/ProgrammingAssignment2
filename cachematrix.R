#16/06/2014
# Two functions to invert an invertable matrix 
###First Function
makeCacheMatrix <- function(x = numeric()) {
  #Create i object
	i <- NULL
  #Create y object so as to not alter x
	set <- function(y) {
			x <<- y		    
			i <<- NULL
		    
	  }
  #set various functions
	get <- function() x
	setinverse <- function(solve) i <<- solve
	getinverse <- function() i
      list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

###Second Function
cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        #See if i is already created
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        #If i is not already created, find inverse
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
