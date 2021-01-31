#calculate points for everyone and draw lottery 
#begin by what i need from edited data--Response ID, total number of questions answered correctly,
#rep_american, dem_american, rep_british, dem_british, rep_french, dem_french, pred, etc.

points <- select(edited_data, ResponseId, total_correct, rep_american, dem_american,
                 pred_rep_american, pred_dem_american, pol_aff2
                 )
setDT(points)
points <- mutate(points, x= sample(1:57, 57, replace = F))
#sort
setorder(points, x)

#for all odd numbers, get value of pol_aff of person below, for all even numbers plus person 57, get value of 
#pol_aff of person above 
points <-mutate(points, pol_aff_opp = ifelse(mod(x,2) == 0 | x == 57, lag(x), lead(x)))

#export to Excel and run VBA program in Excelt to calc total # of points
write.xlsx(points, "data/points.xlsx")
#after running program, import for analysis
total_points <- read.xlsx("data/total_points.xlsx")
total_points <-mutate(total_points, prediction_correct = points_prediction/5)

setDT(total_points)
str_list <- list("points_game", "prediction_correct")
sum_data_obs <- function(a){
  obs <- total_points[, .(Observations = .N), 
                      by = a]
  stargazer(obs, summary = F, out = paste("LaTeX/tables/descriptive/", a, ".tex"))
}
lapply(str_list, sum_data_obs)

tot <- select(total_points, total_points)
stargazer(tot, out = "LaTeX/tables/descriptive/tot.tex")





