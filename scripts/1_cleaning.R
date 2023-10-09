
#Clean growth data
growth_data = read.csv("data/growth_data.csv")

#TODO Ben

saveRDS(growth_data, "data/clean_growth_data.rds")


#Clean choice data
choice_experiment = read.csv("data/choice_data.csv")

#TODO fabi

saveRDS(choice_experiment, "data/clean_choice_data.rds")

