#### There are two function use to create a matrix can cache its inverse


## Function use to create a matrix that can cache its inverse by
## using special operator "<<-"

makeCacheMatrix <- function(x = matrix()){
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  set_inverse <- function(inverse) inv <<- inverse
  get_inverse <- function() inv
  list(set = set, get = get, 
       set_inverse = set_inverse, 
       get_inverse = get_inverse)
}


## Function use to calculate the inverse of 'x', if its inverse existed,
## function will get the cached inverse, else function calculate new inverse

cacheSolve <- function(x,...){
  ##Return a matrix that is the inverse of 'x'
  inv <- x$get_inverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$set_inverse(inv)
  inv
}

