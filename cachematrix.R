## Function takes in a matrix, create cache and return cached matrix 
## To run:
## > d <- matrix(1:4 nrow=2, ncol=2)
## > a <- makeCacheMatrix(d)

makeCacheMatrix <- function(x = matrix()) {
		iflag  <- 0
		set <- function(y)
		{
			x <<- y
			iflag <<- 0
		}
		get <- function() x
		getflag <- function() iflag
		setflag <- function() iflag <<- 1
		list(set = set, get = get, 
			getflag = getflag, 
			setflag = setflag)
		
}


## Function to check if invert cache exist. if yes message displayed and
## cached inverse retrived. If none, inverse matrix and cache
## To run:
## > cacheSolve(a)

cacheSolve <- function(x, ...) {
	   data <- x$get()		
	   lflag <- x$getflag()
	   if(lflag == 1)
	   {
		message("Retrieve inverse matrix from cache.")
		return(data)
	   }
	   idata <- solve(data)
	   x$set(idata)
	   x$setflag()
	   
        ## Return a matrix that is the inverse of 'x'
	  return(idata)
	  
}
