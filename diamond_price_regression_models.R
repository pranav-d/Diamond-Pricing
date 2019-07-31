head(clean_main_diamond[c("carat", "depth", "table", "price", "cut2", "color2", "clarity2", "index2", "index1", "id")])



set.seed(101) # Set Seed so that same sample can be reproduced in future also
# Now Selecting 75% of data as sample from total 'n' rows of the data  
sample <- sample.int(n = nrow(clean_main_diamond), size = floor(0.7*nrow(clean_main_diamond)), replace = F)
train_main_diamond <- clean_main_diamond[sample, ]
test_main_diamond  <- clean_main_diamond[-sample, ]