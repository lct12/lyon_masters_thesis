#Author: Lauren Thomas
#Date Created: 31/05/2020
#Last Updated: 06/06/2020
#Purpose: summarize variables and create graphs/tables

#Summarise variables
#begin by selecting variables to be summarised
summary_data <-select(edited_data, priming_treatment, pol_aff, sex, race, hisp, age, hh_income, expend, educ,
                      rep_goals, dem_goals, cong_elect_str, sp_healthcare, pub_opt, free_college, milt_spending,
                      photo_ID, gnd, carbon_tax, gun_restrict, stu_loans, polt_q_avg, overall_pat, pol_aff2,
                      pol_aff3, pol_aff4, pol_group1, pol_group2, pol_group3, total_correct, instructions_flag,
                      rep_american, dem_american, rep_brit, dem_brit, rep_french, dem_french, pred_rep_american,
                      pred_dem_american, pred_rep_brit, pred_dem_brit, pred_rep_french, pred_dem_french
)

#stargazer is for outputting code for LaTeX
setDT(summary_data)
#summarise binary/categorical variables
str_list <- list("pol_aff", "pol_aff2", "pol_aff3", "pol_aff4",
                 "pol_group1", "pol_group2", "pol_group3", "sex", "race", "age", "hisp",
                 "hh_income", "expend", "educ", "priming_treatment", "total_correct",
                 "pred_rep_american", "pred_dem_american", "pred_rep_brit", 
                 "pred_dem_brit", "pred_rep_french", "pred_dem_french",
                 "rep_american", "dem_american", "rep_brit", 
                 "dem_brit", "rep_french", "dem_french"
                 )
dec_pred <- list(c("rep_american", "pred_rep_american"), c("dem_american", "pred_dem_american"),
             c("rep_brit", "pred_rep_brit"), c("dem_brit", "pred_dem_brit"), 
             c("rep_french", "pred_rep_french"), c("dem_french", "pred_dem_french"))

sum_data_obs <- function(a){
  obs <- summary_data[, .(Observations = .N), 
               by = a]
stargazer(obs, summary = F, out = paste("LaTeX/tables/descriptive/", a, ".tex"))
}
lapply(str_list, sum_data_obs)
lapply(dec_pred, sum_data_obs)





goals <- select(summary_data, rep_goals, dem_goals)
stargazer(goals, out = "LaTeX/tables/goals.tex")

pol_quest <- select(summary_data, sp_healthcare, pub_opt, free_college, milt_spending,
                    photo_ID, gnd, carbon_tax, gun_restrict, 
                    stu_loans, polt_q_avg, overall_pat)
stargazer(pol_quest, out = "LaTeX/tables/pol_quest.tex")

#log output to file
sink(file = "log_output/mw_test.txt")

#Mann-Whitney U test (for continous variables)
mw_list <-list(summary_data$rep_goals, summary_data$dem_goals, summary_data$polt_q_avg, 
               summary_data$overall_pat)
mw_test <- function(x) {
  wilcox.test(x ~priming_treatment, data = summary_data)
}
lapply(mw_list, mw_test)
sink()

#Fisher exact test. To use this, must first transform binary/categorical variables into matrices
#can't figure out how to make this a function so needs to be copy-paste :(

#sex
summ.1 <- summary_data[summary_data$priming_treatment == 1] %>% 
  group_by(sex) %>% summarise(obs = n())
summ.0 <- summary_data[summary_data$priming_treatment == 0] %>% 
  group_by(sex) %>% summarise(obs = n())
summ <- cbind(summ.0$obs, summ.1$obs)
sex <-as.matrix(summ)

#race
summ.1 <- summary_data[summary_data$priming_treatment == 1] %>% 
  group_by(race) %>% summarise(obs = n())
summ.0 <- summary_data[summary_data$priming_treatment == 0] %>% 
  group_by(race) %>% summarise(obs = n())
summ <- cbind(summ.0$obs, summ.1$obs)
race <-as.matrix(summ)

#hisp
summ.1 <- summary_data[summary_data$priming_treatment == 1] %>% 
  group_by(hisp) %>% summarise(obs = n())
summ.0 <- summary_data[summary_data$priming_treatment == 0] %>% 
  group_by(hisp) %>% summarise(obs = n())
summ <- cbind(summ.0$obs, summ.1$obs)
hisp <-as.matrix(summ)

#age
summ.1 <- summary_data[summary_data$priming_treatment == 1] %>% 
  group_by(age) %>% summarise(obs = n())
summ.0 <- summary_data[summary_data$priming_treatment == 0] %>% 
  group_by(age) %>% summarise(obs = n())
summ <- cbind(summ.0$obs, summ.1$obs)
age <-as.matrix(summ)

#hh_income
summ.1 <- summary_data[summary_data$priming_treatment == 1] %>% 
  group_by(hh_income) %>% summarise(obs = n())
summ.0 <- summary_data[summary_data$priming_treatment == 0] %>% 
  group_by(hh_income) %>% summarise(obs = n())
summ <- cbind(summ.0$obs, summ.1$obs)
hh_income <-as.matrix(summ)

#expend
summ.1 <- summary_data[summary_data$priming_treatment == 1] %>% 
  group_by(expend) %>% summarise(obs = n())
summ.0 <- summary_data[summary_data$priming_treatment == 0] %>% 
  group_by(expend) %>% summarise(obs = n())
summ <- cbind(summ.0$obs, summ.1$obs)
expend <-as.matrix(summ)

#educ
summ.1 <- summary_data[summary_data$priming_treatment == 1] %>% 
  group_by(educ) %>% summarise(obs = n())
summ.0 <- summary_data[summary_data$priming_treatment == 0] %>% 
  group_by(educ) %>% summarise(obs = n())
summ <- cbind(summ.0$obs, summ.1$obs)
educ <-as.matrix(summ)

#pol_aff
summ.1 <- summary_data[summary_data$priming_treatment == 1] %>% 
  group_by(pol_aff) %>% summarise(obs = n())
summ.0 <- summary_data[summary_data$priming_treatment == 0] %>% 
  group_by(pol_aff) %>% summarise(obs = n())
summ <- cbind(summ.0$obs, summ.1$obs)
pol_aff <-as.matrix(summ)

#cong_elect_str
summ.1 <- summary_data[summary_data$priming_treatment == 1] %>% 
  group_by(cong_elect_str) %>% summarise(obs = n())
summ.0 <- summary_data[summary_data$priming_treatment == 0] %>% 
  group_by(cong_elect_str) %>% summarise(obs = n())
summ <- cbind(summ.0$obs, summ.1$obs)
cong_elect_str <-as.matrix(summ)

#total correct
summ.1 <- summary_data[summary_data$priming_treatment == 1] %>% 
  group_by(total_correct) %>% summarise(obs = n())
summ.0 <- summary_data[summary_data$priming_treatment == 0] %>% 
  group_by(total_correct) %>% summarise(obs = n())
summ <- cbind(summ.0$obs, summ.1$obs)
total_correct <-as.matrix(summ)

#instructions flag
summ.1 <- summary_data[summary_data$priming_treatment == 1] %>% 
  group_by(instructions_flag) %>% summarise(obs = n())
summ.0 <- summary_data[summary_data$priming_treatment == 0] %>% 
  group_by(instructions_flag) %>% summarise(obs = n())
summ <- cbind(summ.0$obs, summ.1$obs)
instructions_flag <-as.matrix(summ)

#create matrices that represent cooperate vs. defect for reps vs. dems in all nationalities
#Republican vs. Democrat American, Brit, French, followed by prediction for Am, Brit, French
Am = matrix(c(27,30,37,20), nrow = 2)
Br = matrix(c(26,31,38,19), nrow =2)
Fr = matrix(c(29,28,35,22), nrow =2)
Am.pr = matrix(c(33,24,37,20), nrow =2)
Br.pr = matrix(c(29,28,37,20), nrow =2)
Fr.pr = matrix(c(33,24,38,19), nrow =2)

#break out into predictions vs. decisions
Am.rep = matrix(c(25,8,2,22), nrow =2)
Am.dem = matrix(c(33,4,4,16), nrow =2)
Br.rep = matrix(c(23,6,3,25), nrow=2)
Br.dem = matrix(c(31,6,7,13), nrow=2)
Fr.rep = matrix(c(27,6,2,22), nrow =2)
Fr.dem = matrix(c(32,6,3,16), nrow=2)

sink(file = "log_output/fisher_test.txt")
#fisher test function
fisher.list <- list(sex, race, age, hisp, hh_income, expend, educ, pol_aff, cong_elect_str,
                    total_correct, instructions_flag, Am, Br, Fr, Am.pr, Br.pr, Fr.pr,
                    Am.rep, Am.dem, Br.rep, Br.dem, Fr.rep, Fr.dem)
fisher <-function(x) {
  fisher.test(x)
}
lapply(fisher.list, fisher)
sink()


