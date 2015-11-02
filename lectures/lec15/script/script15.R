

####################
# Columbia images
library(jpeg)
pm <- read.csv("photoMetaData.csv")

table(pm$location)
table(pm$photographer)
table(pm$category)

system(paste0("open columbiaImages/",pm$name[300]))

img <- readJPEG(paste0("columbiaImages/",pm$name[300]))
class(img)
dim(img)

options(digits=4)



