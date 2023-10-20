library(dplyr)
library(car)
#Clean growth data
growth_data = read.csv("data/growth_data.csv")

#inspect data
summary(growth_data)

#remove all specamens with negative length or area growth
plot(growth_data$Area_diff,growth_data$Length_diff)
growth_data = growth_data %>% filter(Length_diff >= 0, Area_diff>= 0)
growth_data = growth_data %>% filter(Area_diff< 1.4)


densityPlot(growth_data$Length_diff)


saveRDS(growth_data, "data/clean_growth_data.rds")


#Clean choice data
choice_experiment = read.csv("data/choice_data.csv")

#TODO fabi

saveRDS(choice_experiment, "data/clean_choice_data.rds")

