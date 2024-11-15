play_tennis <- read.csv('play_tennis.csv', stringsAsFactors = TRUE)
# Save as .rda file in the 'data' directory of your project 
save(play_tennis, file = "data/play_tennis.rda") 

library(rpart)
library(rpart.plot)
model <- rpart(PlayTennis ~ ., play_tennis, control = rpart.control(cp = 0, maxdepth = 2, minsplit = 1, minbucket = 1)) 
rpart.plot(model) 

model_2 <- rpart(PlayTennis ~ ., play_tennis, control = rpart.control(cp = 0, maxdepth = 5, minsplit = 1, minbucket = 1)) 
rpart.plot(model_2) 
#when you use a little value for the maxdepth you make the tree more simplier. And increasing the value
#make the tree more complex.
# The complexity of the tree doesn't change when you put maxdepth <= 5 for this data 
# This is because of the distributions of the nodes. In the final nodes for the more complex 
# trees you have a perfect division. And in general this means that you probably will have a overfitting situation
# with new data because this tree makes a perfect distribution of results.

