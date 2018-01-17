#Installation 
if(!("AutoAnnotation" %in% rownames(installed.packages())))
{
  if(!("devtools" %in% rownames(installed.packages())))
  {
    install.packages("devtools")
  }
  
  library(devtools)
  
  dir.create("C:\\R")
  
  .libPaths(c("C:\\R",.libPaths()))
  
  install_github("shihikoo/AutoAnnotation")
}
library(AutoAnnotation)

#
outputModel <- CountTermsInStudies(
  searchingData = 'S:/TRIALDEV/CAMARADES/Alexandra/For Jing/modelExtraction/full-pdfs-ALL-wDOIpdfLinks-04102017.csv'
  ,
  dictionary = 'S:/TRIALDEV/CAMARADES/Alexandra/For Jing/modelExtraction/list-of-models-for-R.csv'
  ,
  textSearchingHeaders = c("Title", "Abstract")
  ,
  linkSearchHeaders = c("PdfPath")
  ,
  dictionaryNameHeader = "dictionaryNameHeader"
  ,
  dictionaryRegexHeader = "dictionaryRegexHeader"
  ,
  ignoreCase = TRUE
  ,
  ignoreExistingTextFile = TRUE
  , 
  conversionSoftware = "C:/xpdfbin-win-3.04/bin64/pdftotext.exe"
) 

outputDrug <- CountTermsInStudies(
  searchingData = 'S:/TRIALDEV/CAMARADES/Alexandra/For Jing/modelExtraction/full-pdfs-ALL-wDOIpdfLinks-04102017.csv'
  ,
  dictionary = 'S:/TRIALDEV/CAMARADES/Alexandra/For Jing/modelExtraction/drug-regex.csv'
  ,
  textSearchingHeaders = c("Title", "Abstract")
  ,
  linkSearchHeaders = c("PdfPath")
  ,
  dictionaryNameHeader = "dictionaryNameHeader"
  ,
  dictionaryRegexHeader = "dictionaryRegexHeader"
  ,
  ignoreCase = TRUE
  ,
  ignoreExistingTextFile = TRUE
  , 
  conversionSoftware = "C:/xpdfbin-win-3.04/bin64/pdftotext.exe"
) 

#adding depression IDs to the output
allPDFs <- read.csv('S:/TRIALDEV/CAMARADES/Alexandra/For Jing/modelExtraction/full-pdfs-ALL-wDOIpdfLinks-04102017.csv', header=T, sep=',')
drug_categories <- read.csv("S:/TRIALDEV/CAMARADES/Alexandra/For Jing/modelExtraction/drug-regex.csv", header=T, sep=',')
model_categories <- read.csv("S:/TRIALDEV/CAMARADES/Alexandra/For Jing/modelExtraction/list-of-models-for-R.csv", header=T, sep=',')

outputModel$depID<-allPDFs$depID 
outputDrug$depID <-allPDFs$depID

#installing packages for data manipulation
install.packages("tidyverse")

library(tidyr)
library(stringr)
library(plyr)
library(dplyr)

unique(outputModel$PdfPathStatus)
length(which(outputModel$PdfPathStatus=="OK: File is read Successfully"))
length(which(outputDrug$PdfPathStatus=="OK: File is read Successfully"))


successfulModel<- filter(outputModel, PdfPathStatus=="OK: File is read Successfully")
successfulDrug <- filter(outputDrug, PdfPathStatus=="OK: File is read Successfully")

#making the datasets long
successfulModel_long <- gather(successfulModel, model, freq, -depID, -PdfPathStatus)
successfulDrug_long <- gather(successfulDrug, drug, freq, -depID, -PdfPathStatus)

#making the model variable a factor
successfulModel_long$model <- as.factor(successfulModel_long$model)
successfulDrug_long$drug <- as.factor(successfulDrug_long$drug)

#look at data
summary(successfulModel_long)
summary(successfulDrug_long)

#convert freq column to numeric
successfulModel_long$freq <- as.numeric(successfulModel_long$freq)
successfulDrug_long$freq <- as.numeric(successfulDrug_long$freq)

#remove regex where frequency is 0
glimpse(subset(successfulModel_long, freq==0))
glimpse(subset(successfulDrug_long, freq==0))

removeModel <- which(successfulModel_long$freq==0)
removeDrug <- which(successfulDrug_long$freq==0)

successfulModel_long_up <- successfulModel_long[-removeModel,]
successfulDrug_long_up <- successfulDrug_long[-removeDrug,]

glimpse(subset(successfulDrug_long_up))
glimpse(subset(successfulModel_long_up))


#making the model variable a factor
successfulModel_long_up$model <- as.factor(successfulModel_long_up$model)
successfulDrug_long_up$drug <- as.factor(successfulDrug_long_up$drug)


#grouping by model or drug to get a count of number of papers containing regex
Model_long_up_group <- successfulModel_long_up %>% group_by(model) %>% summarize(num_docs=n_distinct(depID), avg_freq=mean(freq))
Drug_long_up_group <- successfulDrug_long_up %>% group_by(drug) %>% summarize(num_docs=n_distinct(depID), avg_freq=mean(freq))

summary(Model_long_up_group)
summary(Drug_long_up_group)

#add model/drug category to long dataset
Drug_long_up_group$drugCategory <- drug_categories$drugCategory[match(Drug_long_up_group$drug, drug_categories$dictionaryNameHeader)]
Model_long_up_group$modelCategory <- model_categories$modelCategory[match(Model_long_up_group$model, model_categories$dictionaryNameHeader)]

Model_long_up_group$modelCategory <- as.factor(Model_long_up_group$modelCategory)
Drug_long_up_group$drugCategory <- as.factor(Drug_long_up_group$drugCategory)

# test1 <- summarize(group_by(successfulModel_long_up, model), num_docs=n_distinct(successfulModel_long_up$depID), avg_freq=mean(successfulModel_long_up$freq))

#Model_long_up_group$num_docs <- Model_long_up_group$n_distinct(depID)
#Model_long_up_group$avg_freq <- Model_long_up_group$mean(freq)
#Model_long_up_group$model <- as.factor(Model_long_up_group$model)

#trying to find average number of models/drugs mentioned (not 0) per paper
#length(unique(successfulModel_long_up$depID))
#length(unique(successfulDrug_long_up$depID))

#group_by_(successfulModel_long_up$model)
#count(unique(successfulDrug_long_up$model))

#find out whether any model or drug are NOT found in any documents
#count(unique(successfulDrug_long_up$model))

#other??
#successfulModel_long_up$freq/successfulModel_long$freq



#making the datasets wide
#successfulModel_wide <- spread(successfulModel_long, model, freq)
#successfulDrug_wide <- spread(successfulDrug_long, model, freq)


#making a tfidf column
Ndrug <- nrow(successfulDrug_long_up)
Nmodel <- nrow(successfulModel_long_up)

Drug_long_up_group$tfidf <- ((log(1 + Drug_long_up_group$avg_freq)) * (log(Ndrug * Drug_long_up_group$num_docs)))
Model_long_up_group$tfidf <- ((log(1 + Model_long_up_group$avg_freq)) * (log(Nmodel * Model_long_up_group$num_docs)))

#making log of number of documents
Drug_long_up_group$log_numdoc <- log(Drug_long_up_group$num_docs)
Model_long_up_group$log_numdoc <- log(Model_long_up_group$num_docs)

#making column to remove Regex
Drug_long_up_group$drugName <- sub("Regex.*", "", Drug_long_up_group$drug)
Model_long_up_group$modelName <- sub("Regex.*", "", Model_long_up_group$model)
  
summary(Drug_long_up_group)
glimpse(Drug_long_up_group)
glimpse(Model_long_up_group)

Drug_long_up_group$drugName <- as.factor(Drug_long_up_group$drugName)
Model_long_up_group$modelName <- as.factor(Model_long_up_group$modelName)

#install packages for visualisation
install.packages("plotly")
library(plotly)
packageVersion('plotly')




#create bubble plot
pModel <- plot_ly(
  data = Model_long_up_group, 
  x = ~log_numdoc, 
  y = ~avg_freq, 
  text = ~modelName, 
  type = 'scatter', 
  mode = 'markers',
  color = ~modelCategory,
  marker = list(size = ~tfidf, opacity = 0.5, line = list(width = 2, color = '#FFFFFF'))
) %>%
  layout(
    title = 'Frequency of Models in Depression Dataset',
    xaxis = list(showgrid = T, range = c(1, 10), title='log of Number of Docs Regex Occurs In'),
    yaxis = list(showgrid = T, range = c(1,150), title='Average Frequency of Regex Occurance per Doc'),
    paper_bgcolor = 'rgb(243, 243, 243)',
    plot_bgcolor = 'rgb(243, 243, 243)',
    hovermode="closest",
    legend = list(x = 1.02, y = 1, xanchor = "left", yanchor = "top" )
  )

pModel


pDrug <- plot_ly(
  data = Drug_long_up_group, 
  x = ~log_numdoc, 
  y = ~avg_freq, 
  text = ~drugName, 
  type = 'scatter', 
  mode = 'markers',
  color = ~drugCategory,
  marker = list(size = ~tfidf, opacity = 0.5, line = list(width = 2, color = '#FFFFFF'))
) %>%
  layout(
    title = 'Frequency of Drugs in Depression Dataset',
    xaxis = list(showgrid = T, range = c(0, 9), gridcolor = 'rgb(255, 255, 255)', title='log of Number of Docs Regex Occurs In'),
    yaxis = list(showgrid = T, range = c(1,150), gridcolor = 'rgb(255, 255, 255)', title='Average Frequency of Regex Occurance per Doc'),
    paper_bgcolor = 'rgb(243, 243, 243)',
    plot_bgcolor = 'rgb(243, 243, 243)',
    legend = list(x = 1.02, y = 1, xanchor = "left", yanchor = "top" )
  )

pDrug

install.packages("ggplot2")
install.packages("treemapify")

library(ggplot2) 
library(treemapify)


# plot

treeMapPlotDrug2 <- ggplot2::ggplot(Drug_long_up_group, 
                                   ggplot2::aes(area = num_docs,
                                                fill = avg_freq,
                                                label = drugName,
                                                subgroup = drugCategory)
                                   )  +
  geom_treemap() +
  geom_treemap_subgroup_border(colour = "grey") +
  geom_treemap_subgroup_text(place = "centre", grow = T, alpha = 0.5, colour =
                               "white", fontface = "italic", min.size = 0) +
  geom_treemap_text(colour = "lightyellow", place = "topleft", grow = T, reflow = T) +
  theme(legend.position = "bottom") +
  labs(
    title = "Frequency of Drugs in Depression Systematic Review Dataset",
    caption = "The area of each drug is proportional to the number of documents the term appears in. 
    The colour of each drug is proportional to the average frequency of the term in each document.",
    fill = "Average Frequency of Terms")


treeMapPlotDrug2+scale_fill_gradient(low= "white", high= "red")

print(treeMapPlotDrug2)

treeMapModel <- ggplot2::ggplot(Model_long_up_group, ggplot2::aes(area = num_docs,
                                                                 fill = avg_freq,
                                                                 label = modelName,
                                                                 subgroup = modelCategory))  +
  geom_treemap() +
  geom_treemap_subgroup_border(colour = "lightgrey") +
  geom_treemap_subgroup_text(place = "centre", grow = T, alpha = 0.5, colour =
                               "white", fontface = "italic", min.size = 0) +
  geom_treemap_text(colour = "lightgray", place = "topleft", grow = T, reflow = T) +
  theme(legend.position = "bottom") +
  labs(
    title = "Frequency of Models in Depression Systematic Review Dataset",
    caption = "The area of each outcome measure is proportional to the number of documents the term appears in. 
    The colour of each model is proportional to the average frequency of the term in each document.",
    fill = "Average Frequency of Terms")

treeMapModel+scale_fill_gradient(low="pink", high="darkred")

print(treeMapModel)

# install.packages("circlize")
# library(circlize)
# 
# #convert Model_long_up_group to wide format
# Model_wide <- spread(successfulModel_long_up, model, freq)
# Drug_wide <- spread(successfulDrug_long_up, drug, freq)
# 
# glimpse(Model_wide)
# 
# #remove NA values
# model_wide_up <- as.data.frame(Model_wide)
# model_wide_up[is.na(model_wide_up)] <- 0
# 
# drug_wide_up <- as.data.frame(Drug_wide)
# drug_wide_up[is.na(drug_wide_up)] <- 0
# 
# chrodModel <- chordDiagram(as.data.frame(model_wide_up), transparency = 0.5)
# 
# install.packages('venneuler')
# library(venneuler)
# library(reshape2)
# 
# modelSets <- melt(model_wide_up, id="depID")
# modelSets <- (subset(modelSets, value != 0))
# modelSets = NULL


#writing output to csv file
write.csv(outputModel, file="outputModel.csv")
write.csv(successfulModel_long, file="model_long.csv" )
write.csv(successfulModel_long_up, file="model_long_up.csv" )
write.csv(outputDrug, file="outputdrug.csv")
write.csv(successfulDrug_long, file="drug_long.csv")
write.csv(successfulDrug_long_up, file="model_long_up.csv" )

write.csv(Model_long_up_group, file="model-outputdata.csv")
write.csv(Drug_long_up_group, file="drug-outputedata.csv")
