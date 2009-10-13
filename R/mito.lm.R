mito.lm <-
function (dataset, list = NULL, mito.flag, loci.ID = NULL, trim = 0.05, 
    norm = TRUE, ds = TRUE) 
{
    size <- ncol(dataset$R)
    if (length(list) == 0) {
        list <- paste("sample", c(1:size))
    }
    mito <- list(R = dataset$R[mito.flag, ], G = dataset$G[mito.flag, 
        ], Rds = dataset$Rds[mito.flag, ], Gds = dataset$Gds[mito.flag, 
        ])
    lm1 <- lm2 <- vector("list", size)
    fit1 <- fit2 <- resid1 <- resid2 <- sd.resid1 <- sd.resid2 <- lr <- matrix(0, 
        nrow(dataset$R), ncol(dataset$R))
    r2 <- r2.ds <- lamda1 <- lamda2 <- vector(length = size)
    no <- matrix(0, 2, size)
    if (ds) {
        for (i in 1:size) {
            lm1[[i]] <- lm(mito$G[, i] ~ mito$R[, i])
            lm2[[i]] <- lm(mito$Rds[, i] ~ mito$Gds[, i])
            fit1[, i] <- dataset$R[, i] * lm1[[i]]$coefficients[2] + 
                lm1[[i]]$coefficients[1]
            fit2[, i] <- dataset$Gds[, i] * lm2[[i]]$coefficients[2] + 
                lm2[[i]]$coefficients[1]
            resid1[, i] <- dataset$G[, i] - fit1[, i]
            resid2[, i] <- dataset$Rds[, i] - fit2[, i]
        }
    }
    else {
        for (i in 1:size) {
            lm1[[i]] <- lm(mito$G[, i] ~ mito$R[, i])
            lm2[[i]] <- lm(mito$Gds[, i] ~ mito$Rds[, i])
            fit1[, i] <- dataset$R[, i] * lm1[[i]]$coefficients[2] + 
                lm1[[i]]$coefficients[1]
            fit2[, i] <- dataset$Rds[, i] * lm2[[i]]$coefficients[2] + 
                lm2[[i]]$coefficients[1]
            resid1[, i] <- dataset$G[, i] - fit1[, i]
            resid2[, i] <- dataset$Gds[, i] - fit2[, i]
        }
    }
    for (i in 1:size) {
        tmp1 <- resid1[mito.flag, i]
        tmp2 <- resid2[mito.flag, i]
        myr <- quantile(tmp1, c(trim, 1 - trim))
        lamda1[i] <- sqrt(var(tmp1[tmp1 < myr[2] & tmp1 > myr[1]]))
        no[1, i] <- length(tmp1[tmp1 < myr[2] & tmp1 > myr[1]])
        myr <- quantile(tmp2, c(trim, 1 - trim))
        lamda2[i] <- sqrt(var(tmp2[tmp2 < myr[2] & tmp2 > myr[1]]))
        no[2, i] <- length(tmp2[tmp2 < myr[2] & tmp2 > myr[1]])
        sd.resid1[, i] <- resid1[, i]/lamda1[i]
        sd.resid2[, i] <- resid2[, i]/lamda2[i]
        data <- list(x = mito$R[, i], y = mito$G[, i])
        model <- lm1[[i]]
        r2[i] <- R2(data, model)
        data <- list(x = mito$Gds[, i], y = mito$Rds[, i])
        model <- lm2[[i]]
        r2.ds[i] <- R2(data, model)
    }
    if (norm) {
        coef1 <- matrix(0, size, 4)
        for (i in 1:size) {
            coef1[i, 1] <- lm1[[i]]$coefficients[1]
            coef1[i, 2] <- lm1[[i]]$coefficients[2]
            coef1[i, 3] <- lm2[[i]]$coefficients[1]
            coef1[i, 4] <- lm2[[i]]$coefficients[2]
        }
        round(coef1, 4)
        coef1 <- cbind(list, round(coef1[, 1:2], 4), round(lamda1, 
            4), round(r2, 4), round(coef1[, 3:4], 4), round(lamda2, 
            4), round(r2.ds, 4))
        for (i in 1:size) {
            dataset$R[, i] <- dataset$R[, i] + as.double(coef1[i, 
                2])/2
            dataset$G[, i] <- dataset$G[, i] - as.double(coef1[i, 
                2])/2
            dataset$Rds[, i] <- dataset$Rds[, i] - as.double(coef1[i, 
                6])/2
            dataset$Gds[, i] <- dataset$Gds[, i] + as.double(coef1[i, 
                6])/2
        }
        result <- mito.lm(dataset = dataset, mito.flag = mito.flag, 
            list = list, norm = FALSE, ds = ds, trim = trim)
    }
    else {
        result <- list(mito.flag = mito.flag, list = list, dataset = dataset, 
            lm1 = lm1, lm2 = lm2, resid1 = resid1, resid2 = resid2, 
            fit1 = fit1, fit2 = fit2, lamda1 = lamda1, lamda2 = lamda2, 
            sd.resid1 = sd.resid1, sd.resid2 = sd.resid2, r2 = r2, 
            r2.ds = r2.ds, ds = ds)
    }
    result
}
