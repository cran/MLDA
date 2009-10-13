FDR <-
function (sd.re, perm = 200, thres, pos = TRUE, control = NULL) 
{
    if (!is.null(control)) {
        tmp <- !control
    }
    else {
        tmp <- MMR(sd.re)
    }
    m <- mean(sd.re[!tmp])
    sigma <- sd(sd.re[!tmp])
    cat("mean = ", m, "sigma = ", sigma, "\n")
    count <- fdr.median <- fdr.90 <- rep(0, length(thres))
    for (j in 1:length(thres)) {
        if (pos) {
            fdr <- rep(0, perm)
            for (i in 1:perm) {
                cat("perm = ", i, "\n")
                norm.data <- rnorm(length(sd.re), m, sigma)
                fdr[i] <- sum(norm.data >= thres[j])/sum(sd.re >= 
                  thres[j])
            }
            count[j] <- sum(sd.re >= thres[j])
        }
        else {
            fdr <- rep(0, perm)
            for (i in 1:perm) {
                cat("perm = ", i, "\n")
                norm.data <- rnorm(length(sd.re), m, sigma)
                fdr[i] <- sum(norm.data <= thres[j])/sum(sd.re <= 
                  thres[j])
            }
            count[j] <- sum(sd.re <= thres[j])
        }
        fdr.median[j] <- median(fdr)
        fdr.90[j] <- quantile(fdr, 0.9)
    }
    result <- cbind(fdr.median, fdr.90, thres, count)
    result
}
