# These two functions work together to provide an inverse matrix of a given square invertible matrix.  
# The functions cache their results for more efficient processing.
# An example usage would be as follows:
# set.seed(1)
# X=matrix(runif(9),nrow=3) #create a 3x3 matrix of random numbers
# X2=makeCacheMatrix(X) 
# cacheSolve(X2)
# cacheSolve(X2)  # the second time the code is run a cached version will be retrieved and this will be indicated with a message "getting cached data"

# Note: Many ideas for the comments were taken from Coursera message board posted by Pavel Kirjanas (https://class.coursera.org/rprog-016/forum/thread?thread_id=96)
# Comments were modified from their original version to fit the different programming challenge.

## This function returns a list of functions that are used by cacheSolve
makeCacheMatrix <- function(x = matrix()) {# input x will be a numeric matrix
  m <- NULL #  m will be our inverse matrix.  It is reset to NULL every time makeCacheMatrix is called.
  set <- function(y) { #this is called by cacheSolve() 
    x <<- y
    m <<- NULL
  }
  get <- function() x # this function returns the value of the original matrix
  setInverse <- function(inverse) m <<- inverse # this is called by cacheSolve() during the first cacheSolve access and it will store the value using superassignment
  getInverse <- function() m  # this will return the cached value to cacheSolve() on subsequent accesses
  list(set = set, get = get,  # the output is a list of functions that will be produced every time makeCacheMatrix is called
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function returns a matrix that is the inverse of 'x'.  If the inverse has already been calculated, a cached version of the inverse matrix will be retrieved.
cacheSolve <- function(x, ...) {   # the input x is an object created by makeCacheMatrix
  m <- x$getInverse()   # accesses the object 'x' and gets the value of the inverse matrix
  if(!is.null(m)) {  # if inverse matrix was already cached (not NULL) 
    message("getting cached data") #message to console
    return(m) # and return the value
  }
  data <- x$get() #if x$getInverse() returned NULL then the code will reach this point
  m <- solve(data,...) #calculate the inverse matrix
  x$setInverse(m)  # store the calculated inverse matrix in x
  m # return the inverse matrix
}