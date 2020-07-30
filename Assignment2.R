Matrix <- function(x = matrix()) 
{inverse  <- NULL
  set <- function(y) 
{x <<- y
 inverse <<- NULL}
  get <- function() 
  {x}
  setinverse <- function(inv)
  {inverse <<- inv}
  getinverse <- function()
  {inverse} 
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)}

Solve <- function(x, ...) 
{inverse <- x$getinverse()
  if(!is.null(inverse)) 
  {message("getting cached data")
    return(inverse)}
  
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse}

FinalFunction <- function() 
{matrix_1 <- Matrix(matrix(1:9, 3, 3))
  matrix_1$get()
  matrix_1$getinverse()
  Solve(matrix_1)
  matrix_1$set(matrix(c(0,4,3,23,21,44,65,71,12), nrow=3, ncol=3))
  Solve(matrix_1)   
  matrix_1$get()         
  matrix_1$getinverse()   
  matrix_1$get() %*% my_matrix$getinverse() 
}
