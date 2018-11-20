library(tidyverse)
library(vegan)

data=read_csv("data/avocado_rating.csv") %>%
  mutate(variety = tools::toTitleCase(variety),
         rater = tools::toTitleCase(rater))

ggplot(data, aes(reorder(variety, rating, FUN = mean ), rating)) +
  geom_point(aes(color = rater), size = 2) +
  geom_boxplot(alpha=0.3) +
  scale_y_continuous(limits = c(1,10), breaks = seq(2,10,2))+
  ylab("Rating (1-10)") + 
  xlab("Avocado Variety")  +
  theme_classic() +
  theme(legend.position = "bottom",
        text = element_text(size = 20))

ggsave(filename = paste(Sys.Date(),".variety.rating.png", sep = ""), 
       width = 10, 
       height = 6, 
       units = "in",
       dpi = 300, 
       type="cairo-png")

data_summary <- data %>%
  group_by(rater, variety) %>%
  summarize(mean=mean(rating))

ggplot(data = data_summary, aes(x = reorder(rater, mean, FUN = median), y = mean, group = variety)) +
  geom_line(aes(color = variety), size = 2) +
  geom_point(aes(color = variety), size = 4)+
  xlab("Person")+
  ylab("Rating")+
  theme_classic() +
  theme(legend.position = "bottom",
        text = element_text(size = 20))


ggsave(filename = paste(Sys.Date(),".variety.rating.person.png", sep = ""), 
       width = 10, 
       height = 6, 
       units = "in",
       dpi = 300, 
       type="cairo-png")
#ordination?

rater_spread <- data_summary %>%
  spread(rater, mean) %>%
  column_to_rownames(., "variety") %>%
  as.matrix(.)
rater_spread[is.na(rater_spread)] <- 0

ord<- decorana(veg = rater_spread )

plot(ord)

rater_mean <- data %>%
  group_by(rater) %>%
  summarise(mean = mean(rating))

ord_data = ord$cproj %>%
  as.data.frame(.) %>%
  rownames_to_column(., "rater") %>%
  left_join(., rater_mean)

ggplot(ord_data, aes(DCA1, DCA2, label=rater))+
  geom_text(aes(color=mean), fontface = "bold", size=6) +
  theme_classic() +
  scale_color_viridis_c(option = "D",
                        guide = guide_colourbar(barwidth = 20, 
                                                barheight = 2,
                                                title.position = "top",
                                                title.hjust = 0.5,
                                                title = "Mean Rating"))+
  theme(legend.position = "bottom",
        text = element_text(size=20))

ggsave(filename = paste(Sys.Date(),".variety.ordination.png", sep = ""), 
       width = 10, 
       height = 6, 
       units = "in",
       dpi = 300, 
       type="cairo-png")