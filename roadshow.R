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
  p1 = h2o.predict(model, newdata = h2o_test_data)
  v1 = as.vector(p1$predict)
  v2 = as.factor(v1)
  d1 = data.frame (raw_test$Trip_ID, v2)
  colnames(d1) = c("Trip_ID","Surge_Pricing_Type")
  write.csv(d1, file = filename, row.names = FALSE, quote = FALSE)
}

build_dnn = function (train_data = h_train, hidden_layers = c(200,200),
                      l1 = 1e-5, l2=1e-5, dropout = c(0.2,0.2),
                      rho_value = 0.99,
                      eps_value = 1e-08,
                      out_filename = "pred_dnn.csv"){
  dnn_model = h2o.deeplearning(x=1:13, y = 14, training_frame = train_data,
                               score_validation_sampling = "Stratified", 
                               stopping_rounds = 5, stopping_metric = "misclassification", 
                               stopping_tolerance = 0.05,
                               hidden = hidden_layers,
                               l1=l1,
                               l2=l2,
                               activation = "RectifierWithDropout",
                               hidden_dropout_ratios = dropout,
                               rho = rho_value,
                               epsilon = eps_value
                               )
  pred (dnn_model, raw_test = test, h2o_test_data = h_test, filename = out_filename)
}