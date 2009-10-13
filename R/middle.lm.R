middle.lm <-
function (mito.lm.obj) 
{
    ds <- mito.lm.obj$ds
    dataset <- mito.lm.obj$dataset
    list <- mito.lm.obj$list
    b1 <- b2 <- rep(0, length(list))
    cut <- ceiling(nrow(dataset$R) * 0.025)
    result <- mito.lm.obj
    for (i in 1:length(list)) {
        tt <- sort(result$resid1[, i], index.return = TRUE)
        b1[i] <- (dataset$G[tt$ix[cut], i] - dataset$R[tt$ix[cut], 
            i] + 1.96 * result$lamda1[i])
        tt <- sort(result$resid2[, i], index.return = TRUE)
        if (ds) {
            b2[i] <- (dataset$Rds[tt$ix[cut], i] - dataset$Gds[tt$ix[cut], 
                i] + 1.96 * result$lamda2[i])
        }
        else {
            b2[i] <- (dataset$Gds[tt$ix[cut], i] - dataset$Rds[tt$ix[cut], 
                i] + 1.96 * result$lamda2[i])
        }
    }
    low1 <- low2 <- matrix(FALSE, nrow(dataset$R), ncol(dataset$R))
    fit1 <- fit2 <- resid1 <- resid2 <- sd.resid1 <- sd.resid2 <- matrix(0, 
        nrow(dataset$R), ncol(dataset$R))
    size <- ncol(dataset$R)
    for (i in 1:size) {
        fit1[, i] <- dataset$R[, i] + b1[i]
        resid1[, i] <- dataset$G[, i] - fit1[, i]
        sd.resid1[, i] <- resid1[, i]/result$lamda1[i]
        if (ds) {
            fit2[, i] <- dataset$Gds[, i] + b2[i]
            resid2[, i] <- dataset$Rds[, i] - fit2[, i]
            sd.resid2[, i] <- resid2[, i]/result$lamda2[i]
        }
        else {
            fit2[, i] <- dataset$Rds[, i] + b2[i]
            resid2[, i] <- dataset$Gds[, i] - fit2[, i]
            sd.resid2[, i] <- resid2[, i]/result$lamda2[i]
        }
        if (length(dataset$flag) != 0) {
            low1[, i] <- (sd.resid1[, i] < 2 & dataset$flag[, 
                i] != (-50))
            low2[, i] <- (sd.resid2[, i] < 2 & dataset$flagds[, 
                i] != (-50))
        }
        else {
            low1[, i] <- (sd.resid1[, i] < 2)
            low2[, i] <- (sd.resid2[, i] < 2)
        }
    }
    result <- list(dataset = dataset, resid1 = resid1, resid2 = resid2, 
        low1 = low1, low2 = low2, b1 = b1, b2 = b2, list = list)
    result
}
