install.packages("advporgrammingpart1_1.0.tar.gz", repos = NULL, type = "source")
library(advporgrammingpart1)

play_tennis = read.csv("advporgrammingpart1/data/play_tennis.csv", header=TRUE)
fit_decision_stump_R_v2(play_tennis[c("Outlook","Temperature","Humidity","Wind")],play_tennis$PlayTennis)