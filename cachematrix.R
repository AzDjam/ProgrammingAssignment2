
## The two functions help to create a special objetc that stores a matrix and its inverse.                           
## To avoid recomputing the inverse several times , the inverse matrix is computed 
## just once and then the precomputed result can be looked in the cache.



##makeCacheMatrix  has a matrix x as given  argument and 
##returns a  list containing a function to
# 1- set the value of the matrix 
# 2- get the value of the matrix
# 3- set the inverse of the matrix
# 4- get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  set = function(y) {
    # "<<-" assign a value to an object in an environment 
    # different from the current environment. 
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}




## return  the inverse of the matrix x.The inverse  of the matrix x is computed just one.otherwise
##it returns the precomputed result stored in the cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  
  # Get the current state of the inverse and see if it
  # has been computed 
  inv <- x$getinverse()
  
  # if the inverse has already been computed
  if(!is.null(inv)) {
    #  return the calculated inverse from the cache and skip computation		
    message("Getting cached matrix")
    return(inv)
  }
  
  # otherwise, calculates the inverse 
  # Get the matrix itself
  data <- x$get()
  
  # Find the inverse
  inv <- solve(data, ...)
  
  # Cache this result in the object
  x$setinverse(inv)
  
  # Return this new result
  inv    
}
  
