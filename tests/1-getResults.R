# testing
devtools::load_all()

x <- getResults(err_model = c("A", "M"))


library(ggplot2)

x %>% filter(!grepl("sigma", par)) %>% ggplot() + geom_pointrange(aes(par, y = mean, ymin = `2.5%`, ymax = `97.5%`, col = err_model))
