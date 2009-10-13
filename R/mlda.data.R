mlda.data <-
function (dig1, undig1, dig2, undig2, unmeth.flag, cl, sample.names = NULL, 
    flag1 = NULL, flag2 = NULL) 
{
    R <- undig1
    G <- dig1
    Rds <- dig2
    Gds <- undig2
    dataset <- list(R = R, G = G, Rds = Rds, Gds = Gds, mito.flag = unmeth.flag, 
        cl = cl, sample.names = sample.names, flag = flag1, flagds = flag2)
    dataset
}
