inconsist <-
function (ratio1, ratio2, beta = seq(0.1, 10, by = 0.2), beta1 = seq(0.1, 
    10, by = 0.2), list = NULL, thres1 = 0.01, thres2 = 1.4, 
    plot = FALSE) 
{
    cat("\ncalculating inconsistant rate and consistant rate in", 
        list, "\n")
    n = length(ratio1)
    gap <- gap1 <- 1
    m <- 1
    p.tmp <- m.tmp <- rep(0, length(beta1))
    pm.min <- NULL
    mm.max <- NULL
    for (j in 1:length(beta1)) {
        class1 <- class2 <- matrix(0, n, length(beta))
        mytable <- chi <- result <- list(length = length(beta))
        MM <- PM <- matrix(0, 2, length(beta))
        d <- matrix(0, 1, length(beta))
        cat("\nunmethylation cut-off = ", -beta1[j], "methylation cut-off = ", 
            "\n")
        for (i in 1:length(beta)) {
            cat(beta[i], "||")
            class1[ratio1 > beta[i], i] <- 1
            class1[ratio1 < (-beta1[j]), i] <- (-1)
            class2[ratio2 > beta[i], i] <- 1
            class2[ratio2 < (-beta1[j]), i] <- (-1)
            mytable[[i]] <- table(class1[, i], class2[, i])
            MM[1, i] <- round((mytable[[i]][1, 3]/(mytable[[i]][1, 
                3] + mytable[[i]][2, 3] + mytable[[i]][3, 3]) + 
                mytable[[i]][3, 1]/(mytable[[i]][3, 1] + mytable[[i]][3, 
                  2] + mytable[[i]][3, 3])), 4)
            PM[1, i] <- mytable[[i]][3, 3]/(mytable[[i]][1, 3] + 
                mytable[[i]][3, 3] + mytable[[i]][2, 3]) + mytable[[i]][3, 
                3]/(mytable[[i]][3, 3] + mytable[[i]][3, 1] + 
                mytable[[i]][3, 2])
            MM[2, i] <- round((mytable[[i]][3, 1]/(mytable[[i]][3, 
                1] + mytable[[i]][2, 1] + mytable[[i]][1, 1]) + 
                mytable[[i]][1, 3]/(mytable[[i]][1, 3] + mytable[[i]][1, 
                  2] + mytable[[i]][1, 1])), 4)
            PM[2, i] <- mytable[[i]][1, 1]/(mytable[[i]][3, 1] + 
                mytable[[i]][2, 1] + mytable[[i]][1, 1]) + mytable[[i]][1, 
                1]/(mytable[[i]][1, 1] + mytable[[i]][1, 2] + 
                mytable[[i]][1, 3])
            d <- abs(sqrt((MM[1, i] - thres1)^2 + (MM[2, i] - 
                thres1)^2))
            d1 <- abs(sqrt((PM[1, i] - thres2)^2 + (PM[2, i] - 
                thres2)^2))
            if (gap > d & gap1 > d1) {
                gap <- d
                gap1 <- d1
                IR1 <- beta[i]
                IR2 <- (-beta1[j])
                m1 <- MM[1, i]
                m2 <- MM[2, i]
                p1 <- PM[1, i]
                p2 <- PM[2, i]
            }
        }
        pm.min <- cbind(pm.min, apply(PM, 2, min))
        mm.max <- cbind(mm.max, apply(MM, 2, min))
    }
    cat("\n")
    result <- list(IR1 = IR1, IR2 = IR2, m1 = m1, m2 = m2, p1 = p1, 
        p2 = p2, pm.min = pm.min, mm.max = mm.max)
}
