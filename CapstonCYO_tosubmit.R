#
#PLEASE PUT .R .zip .rmd ALL IN SAME WORKING DIRECTORY TO RUN CODE
#


#Initializing
library(ggplot2)
library(tidyverse)
library(readr)
library(caret)

#Loading data
rm(list=ls()) 
df <- read_csv("nyc-property-sales.zip")

#Filter out nonsensical and cheap sales <$100000, filter out rows with missing price or square footage, filter out square footage below 1000
#Add price per square foot column, since that's what we're truly interested in. Otherwise the data is strongly affected by building size
#The full price can always be calculated afterwards since the square footage will be known
buildingSales <- df %>% select(BOROUGH,NEIGHBORHOOD,`BUILDING CLASS CATEGORY`,`LAND SQUARE FEET`,`GROSS SQUARE FEET`,`SALE PRICE`) %>% 
            filter(grepl("[0123456789]{6}",.$'SALE PRICE'), grepl("[0123456789]{3}",.$'LAND SQUARE FEET'), grepl("[0123456789]{3}",.$'GROSS SQUARE FEET')) %>% 
            mutate('SALE PRICE' = as.numeric(.$'SALE PRICE'),BOROUGH = as.factor(.$BOROUGH), NEIGHBORHOOD = as.factor(NEIGHBORHOOD), 'BUILDING CLASS CATEGORY' = as.factor(.$'BUILDING CLASS CATEGORY'), 'LAND SQUARE FEET' = as.numeric(.$`LAND SQUARE FEET`), 'GROSS SQUARE FEET' = as.numeric(.$'GROSS SQUARE FEET')) %>%
            mutate(PPSQFT = .$'SALE PRICE'/.$'GROSS SQUARE FEET')
head(buildingSales)

#Rename without spaces
names(buildingSales) <- c("BOROUGH", "NEIGHBORHOOD", "CLASS", "LAND_SQFT", "GROSS_SQFT", "PRICE", "PPSQFT")

#Divide land sizes into XSmall (smallest 5%) and XLarge (largest 5%). Divide middle 90% evenly into small, medium, and large
lotSizeQuintiles <- quantile(buildingSales$LAND_SQFT, probs =  c(0.0, 0.05, 0.35, 0.65, 0.95, 1.00))#c(0.0, 0.20, 0.40, 0.60, 0.80, 1.00))
sizeLots <- function(toBe, levs){
                        if(toBe <= levs[2]){'XSmall'}
                        else{if (toBe <= levs[3]){'Small'}
                          else{if (toBe <= levs[4]){'Medium'}
                            else{if (toBe <= levs[5]){'Large'}
                              else{'XLarge'}}}}
}
buildingSales <- buildingSales %>%  mutate(LOT_SIZE = sapply(LAND_SQFT, FUN = sizeLots, levs = lotSizeQuintiles)) %>% mutate(LOT_SIZE = as.factor(LOT_SIZE)) #rowwise() %>% mutate(LOT_SIZE = sizeLots(LAND_SQFT,lotSizeQuintiles))
head(buildingSales)

#Exploratory analysis
unique(buildingSales$BOROUGH)
unique(buildingSales$NEIGHBORHOOD)
unique(buildingSales$CLASS)

length(unique(buildingSales$BOROUGH))
length(unique(buildingSales$NEIGHBORHOOD))
length(unique(buildingSales$CLASS))

#Exploratory plots
#Many plots are hard to view due to outliers (e.g. billion+ dollar sales and million+ sqft lot sizes make regular sales impossible to see)
#So some are followed by xlim and ylim to zoom in
buildingSales %>% ggplot(aes(BOROUGH)) + geom_bar()
buildingSales %>% ggplot(aes(NEIGHBORHOOD)) + geom_bar()  + theme(axis.text.x=element_text(angle=90, hjust=1))
buildingSales %>% ggplot(aes(CLASS)) + geom_bar()  + theme(axis.text.x=element_text(angle=90, hjust=1))
buildingSales %>% ggplot(aes(PRICE)) + geom_histogram()
buildingSales %>% ggplot(aes(x = GROSS_SQFT, y = PRICE)) + geom_point()
buildingSales %>% filter(CLASS == '01 ONE FAMILY DWELLINGS') %>% ggplot(aes(x = GROSS_SQFT, y = PRICE)) + geom_point() + xlim(0,10000) + ylim(0,10000000)
buildingSales %>% filter(LAND_SQFT < 25000) %>% ggplot(aes(LAND_SQFT)) + geom_histogram()
buildingSales %>% filter(PRICE <= 1000000) %>% ggplot(aes(PRICE)) + geom_histogram()

buildingSales %>% group_by(BOROUGH) %>% ggplot(aes(x = BOROUGH, y = PRICE)) + geom_boxplot()
buildingSales %>% group_by(BOROUGH) %>% ggplot(aes(x = BOROUGH, y = PRICE)) + geom_boxplot() + ylim(0,10000000)
buildingSales %>% group_by(BOROUGH) %>% ggplot(aes(x = BOROUGH, y = PPSQFT)) + geom_boxplot()  + ylim(0,10000)

buildingSales %>% filter(BOROUGH == 1) %>% select(PRICE) %>% summary()
topTenNeighborhoods <- buildingSales %>% group_by(NEIGHBORHOOD) %>% summarize(countNeighborhood = n()) %>% arrange(desc(countNeighborhood)) %>% top_n(10) %>% pull(NEIGHBORHOOD)
topTenNeighborhoods
buildingSales %>% filter(NEIGHBORHOOD %in% topTenNeighborhoods) %>% group_by(NEIGHBORHOOD) %>% ggplot(aes(x = NEIGHBORHOOD, y = PPSQFT)) + geom_boxplot() + theme(axis.text.x=element_text(angle=90, hjust=1)) + ylim(0,5000)


#Set seed for repeatability, runs well with other seeds too
set.seed(1)

#Creating data partition and ensuring that all neighborhoods and building classes in test validation set are present in test set
test_index <- createDataPartition(y = buildingSales$PPSQFT, times = 1, p = 0.1, list = FALSE)

trainSet <- buildingSales[-test_index,]
testSet <- buildingSales[test_index,]

head(trainSet)
head(testSet)


unique(trainSet$BOROUGH)
unique(trainSet$NEIGHBORHOOD)
unique(trainSet$CLASS)

length(unique(trainSet$BOROUGH))
length(unique(trainSet$NEIGHBORHOOD))
length(unique(trainSet$CLASS))

unique(testSet$BOROUGH)
unique(testSet$NEIGHBORHOOD)
unique(testSet$CLASS)

length(unique(testSet$BOROUGH))
length(unique(testSet$NEIGHBORHOOD))
length(unique(testSet$CLASS))

validation <- testSet %>%
  semi_join(trainSet, by = "NEIGHBORHOOD") %>%
  semi_join(trainSet, by = "CLASS")

removed <- anti_join(testSet, validation)
trainSet <- rbind(trainSet, removed)

mean(validation$BOROUGH %in% trainSet$BOROUGH)
mean(validation$NEIGHBORHOOD %in% trainSet$NEIGHBORHOOD)
mean(validation$CLASS %in% trainSet$CLASS)
# ^^^ needs to be 1.00

mean(trainSet$BOROUGH %in% validation$BOROUGH)
mean(trainSet$NEIGHBORHOOD %in% validation$NEIGHBORHOOD)
mean(trainSet$CLASS %in% validation$CLASS)

unique(validation$BOROUGH)
unique(validation$NEIGHBORHOOD)
unique(validation$CLASS)

length(unique(validation$BOROUGH))
length(unique(validation$NEIGHBORHOOD))
length(unique(validation$CLASS))

#Exploratory analysis shows that boroughs, neighborhoods, land size, and building class could all be important
#Creating linear models predicting based off of above factors

linearFitB <- lm(PPSQFT ~ BOROUGH, data = buildingSales)
yhatB <- predict(linearFitB, validation)
RMSEB <- RMSE(yhatB,validation$PPSQFT)
results <- data.frame(fit = "BOROUGH", err = RMSEB, stringsAsFactors = FALSE)

linearFitBN <- lm(PPSQFT ~ BOROUGH + NEIGHBORHOOD, data = buildingSales)
yhatBN <- predict(linearFitBN, validation)
RMSEBN <- RMSE(yhatBN,validation$PPSQFT)
results <- rbind(results, c("BOROUGH + NEIGHBORHOOD", RMSEBN))

#Seems that adding factors beyond neighborhood doesn't help
linearFitBNL <- lm(PPSQFT ~ BOROUGH + NEIGHBORHOOD + LOT_SIZE, data = buildingSales)
yhatBNL <- predict(linearFitBNL, validation)
RMSEBNL <- RMSE(yhatBNL,validation$PPSQFT)
results <- rbind(results, c("BOROUGH + NEIGHBORHOOD + LOT_SIZE", RMSEBNL))

linearFitBNC <- lm(PPSQFT ~ BOROUGH + NEIGHBORHOOD + CLASS, data = buildingSales)
yhatBNC <- predict(linearFitBNC, validation)
RMSEBNC <- RMSE(yhatBNC,validation$PPSQFT)
results <- rbind(results, c("BOROUGH + NEIGHBORHOOD + CLASS", RMSEBNC))

results

