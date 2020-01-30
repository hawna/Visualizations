##read excel
data <- read_excel("/Users/file.xlsx", sheet = "sheet1")

###Box Plots
pBoxAllStaffAct <- ggplot(data, aes(x = LCAType, y = actTotal)) +
  geom_boxplot(outlier.shape = 16, outlier.size = 2, notch = FALSE) +
  theme_light() +
  theme(legend.title = element_text(size=12), legend.text = element_text(size = 10), 
        axis.text.x = element_text(size = 10), axis.title.x = element_text(size = 12), 
        axis.text.y = element_text(size = 10), axis.title.y = element_text(size = 12)) +
  labs(title = "Boxplots of User Actions by LCA Data Use Types, N= tk Users", 
       subtitle = "All Action Types",
       x = "LCA Data Use Type", 
       y = "Total User Actions",
       caption = "Note. TBA")

pBoxAllStaffAct
