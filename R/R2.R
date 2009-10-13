R2 <-
function (data, model, type = "lm") 
{
    m <- mean(data$y)
    ss.tot <- sum((data$y - m)^2)
    if (type == "lm") {
        y.pre <- predict(model)
    }
    else {
        y.pre <- data$x * coef(model)[2] + coef(model)[1]
    }
    ss.reg <- sum((data$y - y.pre)^2)
    r2 <- 1 - ss.reg/ss.tot
    r2
}
