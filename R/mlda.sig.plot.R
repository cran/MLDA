mlda.sig.plot <-
function (score.obj, pvalue = 0.05, control1 = NULL, control2 = NULL, 
    control3 = NULL) 
{
    sr.obj <- score.obj$sr.obj
    score1 <- sr.obj$score.se
    score2 <- sr.obj$score.re
    cc <- 1
    while (!is.na(sr.obj$fdr.pos[cc, 1]) && cc < 500 && sr.obj$fdr.pos[cc, 
        1] > pvalue) {
        cc <- cc + 1
        if (cc <= nrow(sr.obj$fdr.pos)) {
            pos.cut <- sr.obj$fdr.pos[cc, "thres"]
            up.flag <- sr.obj$SR >= pos.cut
        }
        else {
            pos.cut <- 0
            up.flag <- rep(FALSE, length(sr.obj$SR))
            break
        }
    }
    cc <- 1
    while (!is.na(sr.obj$fdr.neg[cc, 1]) && cc < 500 && sr.obj$fdr.neg[cc, 
        1] > pvalue) {
        cc <- cc + 1
        if (cc <= nrow(sr.obj$fdr.neg)) {
            neg.cut <- sr.obj$fdr.neg[cc, "thres"]
            down.flag <- sr.obj$SR <= neg.cut
        }
        else {
            neg.cut <- 0
            down.flag <- rep(FALSE, length(sr.obj$SR))
            break
        }
    }
    plot(score1, score2, xlab = "averaged sensitive score", ylab = "averaged resistant score", 
        pch = ".")
    points(score1[up.flag], score2[up.flag], col = 2, pch = 16)
    points(score1[down.flag], score2[down.flag], col = 4, pch = 16)
    points(score1[control1], score2[control1], col = 3, pch = 16)
    points(score1[control2], score2[control2], col = 5, pch = 16)
    points(score1[control3], score2[control3], col = 6, pch = 16)
    legend("bottomright", c("positive", "negative", "control1", 
        "control2", "control3"), col = c(2, 4, 3, 5, 6), pch = 16, 
        cex = 0.6)
    abline(sr.obj$line, col = 2, lwd = 2)
}
