lr.plot <-
function (mlda.data, mlda.obj, cutoff.obj, loci = NULL, legend = TRUE) 
{
    lr.thres = cutoff.obj$lr.thres
    par(mfrow = c(2, 2))
    ratio1 <- mlda.obj$ratio1
    ratio2 <- mlda.obj$ratio2
    probeID <- rownames(mlda.data$R)
    if (!is.null(mlda.data$sample.names)) {
        size <- length(mlda.data$sample.names)
    }
    else size <- ncol(mlda.obj$ratio1)
    m <- 1
    for (i in 1:size) {
        plot(ratio1[, i], ratio2[, i], xlab = "log likelihood ratio1", 
            ylab = "log likelihood ratio2", main = mlda.data$sample.names[i])
        abline(v = lr.thres[1, i], h = lr.thres[1, i], col = 2)
        abline(v = lr.thres[2, i], h = lr.thres[2, i], col = 4)
        if (!is.null(loci)) {
            for (j in 1:length(loci)) {
                points(ratio1[probeID == loci[j], i], ratio2[probeID == 
                  loci[j], i], col = 1 + j, pch = 16)
            }
            if (legend) {
                legend("topleft", loci, col = c(2:(1 + length(loci))), 
                  pch = 16, cex = 0.6)
            }
        }
        if (m%%4 == 0 & m != size) {
            dev.new()
            par(mfrow = c(2, 2))
        }
        m <- m + 1
    }
}
