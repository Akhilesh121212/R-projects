wbcd <- read.csv("wisc_bc_data.csv", stringsAsFactors = FALSE)
wbcd <- wbcd[-1]
wbcd$diagnosis <- factor(wbcd$diagnosis, levels = c("B", "M"),
                         labels = c("Benign", "Malignant"))
round(prop.table(table(wbcd$diagnosis)) * 100, digits = 1)

# Function to normalize(Making data in one measure of scale) the data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
#nomralized data new
wbcd_n <- as.data.frame(lapply(wbcd[2:31], normalize))

#Now first half of data will be used to train the model
#and then we'll evaluate the remaining.

wbcd_train <- wbcd_n[1:469, ]
wbcd_test <- wbcd_n[470:569, ]

#Including the first column in both the seperations.
wbcd_train_labels <- wbcd[1:469, 1]
wbcd_test_labels <- wbcd[470:569, 1]

install.packages("class")
install.packages("gmodels")
library("class")\
library("gmodels")

#The Knn algorithm 

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,
                      cl = wbcd_train_labels, k = 21)

CrossTable(x = wbcd_test_labels, y = wbcd_test_pred,
           prop.chisq = FALSE)

#for using z transformation Use scale function and no need to use lapply 


