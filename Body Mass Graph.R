setwd("C:/Users/alfie/OneDrive/Documents/University/Masters/Semester 4/Coding/Graphs and Results")

library(readxl)
library(ggplot2)


dat <- read_excel("Astragali Body Mass.xlsx")

ggplot(dat, aes(x = Astragali, y = `Body Mass (Kg)`)) +
  geom_point(size = 2.5) +
  labs(
    x = "Astragali",
    y = "Body Mass (Kg)",
    title = "Estimated Total Body Mass per Specimen"
  ) +
  theme_classic(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme( panel.grid.major = element_line(colour = "grey92")
  )



ggsave(("Astragali Body Mass Plot.png"))


citation("ggplot2")
