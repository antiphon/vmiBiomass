# testing
devtools::load_all()

x <- getParameters(err_model = c("A", "M"))

library(ggplot2)

x %>% filter(!grepl("sigma|A", par)) %>%
  ggplot() +
  geom_pointrange(aes(par, y = mean, ymin = `2.5%`, ymax = `97.5%`, col = err_model),
                  position = position_dodge(w=.1))


#
z <- getCurves(err_model = c("A", "M"))

z %>% ggplot(aes(x * 100, mean)) +
  geom_ribbon(aes(ymin = `2.5%`, ymax = `97.5%`, fill = err_model), alpha = .5) +
  geom_line(aes(col = err_model))
