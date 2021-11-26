setdw("D:\\ai-heating-model")

training_data = read.table("ucnaSem1.txt", header=T, sep=",", stringsAsFactors=T)
summary(training_data)