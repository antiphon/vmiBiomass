# test eval compertz

devtools::load_all()

x <- seq(0, 200, l = 50) / 100

id1 <- lookup$id[20]

val1 <- evalGompertz(x, id1, mc = 1)
val2 <- evalGompertz(x, id1, mc = 1000)


plot(val1)
lines(val2)
lines(val2[,c(1,3)])
lines(val2[,c(1,4)])
