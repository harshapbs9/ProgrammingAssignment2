## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##This function is used to make a cache matrix gets a matrix as an input,set the value of a matrix
#to get the value of a matrix, to set the inverse of a matrix and get inverse of a matrix,The matrix object can cache
#cache it's own object
##This function creates a special "matrix" object that can cache it's inverse
#<<- this operator is used to assign value to an object in an environment that is 
#different from current environment

makeCacheMatrix <- function(x = matrix()){ ## define the argument with default mode of matrix
  inv<-NULL ##We are initialising the inverse as null
  set<-function(y){ ##define the set function to assign new
    x<<-y           ##value of matrix in parent environment
    inv<<-NULL     ##if there is a new matrix, reset inv to null
  }
  get<-function() x ##define the get function - returns value of matrix argument
  setinverse<- function(inverse) inv <<-inverse ##assigns value of inv in parent environment
  getinverse <- function() inv           ##gets the value of inv when called
  list(set= set,get= get,setinverse=setinverse,getinverse=getinverse) ##you need this in order to refer to the function with the $ operator
}


## Write a short comment describing this function
##This function computes the inverse of the special "matrix" returned by make cache matrix above
##If the inverse has already been calculated (and the matrix has notr changed),
##then cachesolve will retrieve from the cache
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv<-x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data<-x$get()
  inv<-solve(data, ...)
  x$setinverse(inv)
  inv  
}