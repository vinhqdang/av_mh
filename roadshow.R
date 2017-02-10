train = read.csv("train.csv")
test = read.csv("test.csv")

train$Surge_Pricing_Type = as.factor(train$Surge_Pricing_Type)

if (!require(h2o)) {
  install.packages("h2o")
}

library (h2o)

h2o.init(max_mem_size = "64g", nthreads = -1)

h_train = as.h2o(train)
h_test = as.h2o (test)