# the function is created by the fucntion command, it is an object whose name is given by a variable
# to which I will assign this object
r_permutation = function(n,k) # here I enter the number of parameters and their names
{ # the whole body of the function is enclosed in parentheses {...}
    numerator = factorial(n)  # the factorial in the original Rku exists so we will use it
    denominator = factorial(n-k)
    return(numerator/denominator)    # what the function returns is given in the return(...) statement
}

r_permutation_repetition = function(n,k)
{
  return(n^k)
}

permutation = function(n)
{
  return(r_permutation(n,n))
}

permutation_repetition = function(vec_n) # vec_n is the vector of values eg: vec_n=c(2,2,2,4,3)
{
    n = sum(vec_n) # we calculate how many values we have in total
    res_temp=factorial(n) # their factorial=value in the numerator
    # a simple loop starts with the for statement, then the iterator name az follows in parentheses
    # what list will be taken
    for(count in vec_n) # count is an iterator and will gradually take values from the vector vec_n
    {
        # we gradually divide by the factorial of each number of unique entities
        res_temp=res_temp/factorial(count) 
    }
    return(res_temp)
}

combinations = function(n,k)
{
  return(choose(n,k)) # the function for combination already exists in Rku and is called choose
}

combinations_repetition = function(n,k)
{
  return(choose(n+k-1,k)) # we use a known formula
}
