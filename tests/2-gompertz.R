# test eval compertz

devtools::load_all()

x <- seq(0, 200, l = 50) / 100

id1 <- lookup$id[20]

val1 <- evalGompertz(x, id1, mc = 1)
val2 <- evalGompertz(x, id1, mc = 1000)


plot(val1[,"mean"])
lines(val2[,c("2.5%")])
lines(val2[,c("97.5%")])


c1_1 <- getCurves(id = l1$id, log_scale = !TRUE)
c1_2 <- evalGompertz(id = l1$id, log_scale = !TRUE)

library(ggplot2)

bind_rows(c1_1 %>% mutate(m = "posterior mean curve"),
          c1_2 %>% mutate(m = "curve at posterior mean")) %>%
  ggplot( aes(x, mean, col = m)) +
  geom_line()

