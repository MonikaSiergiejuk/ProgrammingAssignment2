## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


#Function "makeCacheMatrix" allows to create objects (m and x),
#then retrive data and set data value within objects (to NULL, to y),
#and finally to return a list of objects (with each element of the
#list named) to the parent environment.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  
  setmatrix <- function(solve) m <<- solve
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}

## Write a short comment describing this function

#Function "cacheSolve" allows to calculate inverse matrix.
#It checks if m was cleared. If not, it returs cached data
#with the appriopriate message. If it was cleared, it calculates
#inverse matrix again.

cacheSolve <- function(x, ...) {
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmatrix(m)
  m
}

m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)

#Assigning my matrix to a new object
asdf <- makeCacheMatrix(m1)

#Calculating inverse matrix
cacheSolve(asdf)
#Checking --> calling cacheSolve again should retrieve (not recalculate) inverse matrix
cacheSolve(asdf)
