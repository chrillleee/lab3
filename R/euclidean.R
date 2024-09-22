euclidean <-
function(num1,num2){
    stopifnot(is.numeric(num1)&length(num1)==1,is.numeric(num2)&length(num2)==1)

    if(num1>=num2){
        a <- num1
        b <- num2
    } else{
        b <- num1
        a <- num2
    }

    while(b != 0){
        temp <- b
        b <- a %% b
        a <- temp
    }
    return(a)
}
