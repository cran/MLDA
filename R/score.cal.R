score.cal <-
function (mlda.obj, cutoff.obj, mlda.data, perm = 200, control = NULL) 
{
    cl <- mlda.data$cl
    score <- weight.score(mlda.obj, cutoff.obj$lr.thres)
    if (is.matrix(score[, cl == 1])) 
        score.se <- apply(score[, cl == 1], 1, mean)
    else score.se <- score[, cl == 1]
    if (is.matrix(score[, cl == 2])) 
        score.re <- apply(score[, cl == 2], 1, mean)
    else score.re <- score[, cl == 2]
    sr.obj <- SR(score.se, score.re, perm = perm, control = control)
    result <- list(sr.obj = sr.obj, score = score)
    result
}
