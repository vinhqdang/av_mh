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
  dnn_model
}

build_rf = function (train_data = h_train, ntree = 500,
                      out_filename = "pred_rf.csv",
                     maxdepth = 25,
                     minrows = 1,mtries = -1,
                     samplerate = 0.5,
                     nbincats=1024,
                     nbin=20,
                     col_sample_rate_per_tree = 1,
                     sample_rate_per_class = 1,
                     balance=FALSE){
  rf_model = h2o.randomForest(x=1:13, y = 14, training_frame = train_data,
                               ntrees = ntree,
                              max_depth = maxdepth,
                              min_rows = minrows,
                              stopping_rounds = 10,
                              stopping_metric = "misclassification",
                              stopping_tolerance = 0.02,
                              mtries = mtries,
                              sample_rate = samplerate,
                              nbins_cats = nbincats,
                              nbins = nbin,
                              col_sample_rate_per_tree = col_sample_rate_per_tree,
                              sample_rate_per_class = sample_rate_per_class,
                              balance_classes = balance
  )
  
  # build_rf (maxdepth=15, nbincats=512, nbin=10, out_filename="rf_1.csv")
  pred (rf_model, raw_test = test, h2o_test_data = h_test, filename = out_filename)
  rf_model
}

build_rf (maxdepth=15, nbincats=512, nbin=10, out_filename="rf_2.csv", ntree=2048)

build_gbm = function (train_data = h_train,
                     out_filename = "pred_gbm.csv",
                     max_depth = 25,
                     sample_rate = 0.5,
                     col_sample_rate = 0.5,
                     col_sample_rate_per_tree = 0.5){
  gbm_model = h2o.randomForest(x=1:13, y = 14, training_frame = train_data,
                              max_depth = max_depth,
                              sample_rate = sample_rate,
                              col_sample_rate = col_sample_rate,
                              col_sample_rate_per_tree = col_sample_rate_per_tree
  )
  pred (gbm_model, raw_test = test, h2o_test_data = h_test, filename = out_filename)
  gbm_model
}