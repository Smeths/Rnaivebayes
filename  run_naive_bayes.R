lens <- read.csv("data/data_frames/lens_data.csv",as.is = TRUE)
head(lens)
pred_lens <- naive_bayes(lens)
pred_lens