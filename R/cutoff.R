cutoff <-
function (dataset, mlda.obj, IR = 0.01, CR = 1.4, ncut = 30, 
    unmeth.start = 1, meth.start = 1) 
{
    list <- dataset$sample.names
    if (is.null(list)) {
        list <- paste("sample", 1:ncol(mlda.obj$ratio1))
    }
    size <- length(list)
    test <- list(length = size)
    lr.thres <- matrix(0, 2, size)
    datalr <- mlda.obj
    for (i in 1:size) {
        if (length(dataset$flag) != 0) {
            tmp1 <- datalr$ratio1[dataset$flag[, i] != (-50) & 
                dataset$flagds[, i] != (-50), i]
            tmp2 <- datalr$ratio2[dataset$flagds[, i] != (-50) & 
                dataset$flag[, i] != (-50), i]
            cat(length(tmp1), "\n")
        }
        else {
            tmp1 <- datalr$ratio1[, i]
            tmp2 <- datalr$ratio2[, i]
        }
        t1 <- min(quantile(tmp1[tmp1 > 0], 0.9), quantile(tmp2[tmp2 > 
            0], 0.9))
        gap1 <- t1/ncut
        t2 <- abs(max(quantile(tmp1[tmp1 < 0], 0.1), quantile(tmp2[tmp2 < 
            0], 0.1)))
        gap2 <- t2/ncut
        test[[i]] <- inconsist(tmp1, tmp2, beta = seq(meth.start, 
            t1, by = gap1), beta1 = seq(unmeth.start, t2, by = gap2), 
            list = list[i], thres1 = IR, thres2 = CR, plot = plot)
        lr.thres[1, i] <- test[[i]]$IR2
        lr.thres[2, i] <- test[[i]]$IR1
    }
    rownames(lr.thres) <- c("unmethylation cut-off", "methylation cut-off")
    colnames(lr.thres) <- dataset$sample.names
    result <- list(lr.thres = lr.thres, rate = test)
    result
}
