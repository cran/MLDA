weight.score <-
function (lr.obj, lr.thres) 
{
    nr <- nrow(lr.obj$ratio1)
    nc <- ncol(lr.obj$ratio2)
    score <- matrix(0, nr, nc)
    size <- nc
    for (i in 1:size) {
        a = lr.thres[1, i]
        b = lr.thres[2, i]
        x1 <- lr.obj$ratio1[, i]
        y1 <- lr.obj$ratio2[, i]
        score[x1 >= b & y1 >= b, i] <- 1
        score[x1 <= a & y1 <= a, i] <- (-1)
        score[x1 > 0 & x1 < b & y1 > 0 & y1 < b, i] <- 0.2
        score[x1 > 0 & x1 < b & y1 >= b, i] <- 0.45
        score[x1 >= b & (y1 < b & y1 > 0), i] <- 0.45
        score[x1 < 0 & x1 > a & y1 < 0 & y1 > a, i] <- (-0.2)
        score[x1 < 0 & x1 > a & y1 <= a, i] <- (-0.45)
        score[x1 <= a & y1 < 0 & y1 > a, i] <- (-0.45)
        score[x1 > 0 & x1 < b & y1 < 0 & y1 > a, i] <- 1e-04
        score[x1 < 0 & x1 > a & y1 > 0 & y1 < b, i] <- 1e-04
    }
    score
}
