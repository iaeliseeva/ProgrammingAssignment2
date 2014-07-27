## These functions compute the inverse of matrix and cache it in the special "matrix"
## Next time, if the matrix has not changed, return cached data.

# This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function (x = matrix()){ 
  x_inverse <- NULL
  set <- function (i){  # define x and x_inverse in local enviroment
    x <<- i
    x_inverse <<- NULL
  }
  get <- function() x   # get x (defined earlier by set function)
  set_inverse <- function(inverse) x_inverse <<- inverse # redefine x_inverse in local enviroment
  get_inverse <- function() x_inverse # get x_inverse
  list (set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse) # create special "matrix"
}


# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) <- function (x,...){ 
  inverse <- x$get_inverse() # get "x_inverse" from special "matrix"
  if(!is.null(inverse)) {    # check existence of "x_inverse" in special "matrix"
    message("getting cached data") # if TRUE, print message, get "x_inverse" from special "matrix" and exit function
    return(inverse)
  }
  data <- x$get()             # get "x" from special "matrix"     
  inverse <- solve(data,...)  # computes the inverse of "x" from special "matrix" 
  x$set_inverse(inverse)      # redefine "x_inverse" in "special "matrix"
  inverse                     # get inverse of "x"
}
