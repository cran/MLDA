MMR <-
function (x) 
{
    tmp <- abs(x - median(x))/(mad(x)/0.6745)
    k <- sqrt(qchisq(0.975, 1))
    print(k)
    flag <- (tmp > k)
    flag
}
