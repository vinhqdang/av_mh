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

pred = function (model, raw_test = test, h2o_test_data = h_test, filename = "pred.csv") {
  p1 = h2o.predict(model, newdata = test_data)
  v1 = as.vector(p1$predict)
  v2 = as.factor(v1)
  d1 = data.frame (raw_test$Trip_ID, v2)
  colnames(d1) = c("Trip_ID","Surge_Pricing_Type")
  write.csv(d1, file = filename, row.names = FALSE, quote = FALSE)
}