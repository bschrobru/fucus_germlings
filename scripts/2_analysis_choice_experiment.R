
choice_data = readRDS("data/clean_choice_data.rds")



choice_data$wwdiff = choice_data$wetw_1_g-choice_data$wetw_2_g
choice_data$wwdiff = choice_data$wetw_2_g/choice_data$wetw_1_g


choice_data$Jar=as.factor(choice_data$Jar)

ggboxplot(choice_data, y="wwdiff",x="Size")

mod=lmer(wwdiff~Size+(1|Jar),choice_data)
summary(mod)
