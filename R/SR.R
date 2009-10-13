SR <-
function (score.se, score.re, plot = FALSE, perm = 200, control = NULL) 
{
    myline <- line(score.se, score.re)
    alpha <- NULL
    if (plot) {
        plot(score.se, score.re)
        abline(myline, col = 2)
    }
    re <- re.control <- residuals(myline)
    q <- Qn(re)
    if (q > 1.75 & q < 1.95) {
        alpha <- 0.05 + (q - 1.75) * (0.4 - 0.05)/(1.95 - 1.75)
    }
    if (q < 1.75) 
        alpha <- 0.05
    if (q > 1.95) {
        alpha <- 0.4
    }
    cat("Trim Degree = ", alpha, "\n")
    myr <- quantile(re, c(alpha, 1 - alpha))
    sigma <- sqrt(var(re[re < myr[2] & re > myr[1]]))
    sd.re <- re/sigma
    fdr.pos <- FDR(sd.re, thres = seq(2, max(sd.re), 0.5), perm = perm, 
        control = control)
    fdr.neg <- FDR(sd.re, thres = seq(-2, min(sd.re), -0.5), 
        pos = FALSE, perm = perm, control = control)
    result <- list(SR = sd.re, fdr.pos = fdr.pos, fdr.neg = fdr.neg, 
        alpha = alpha, line = myline, score.se = score.se, score.re = score.re)
}
