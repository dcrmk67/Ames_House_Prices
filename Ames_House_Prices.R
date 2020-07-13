
library(tidyverse)
library(lubridate)
library(caret)
library(summarytools)
library(knitr)
library(sjlabelled)

train <- read_csv("./train.csv", col_names=TRUE) %>% 
  mutate(across(where(is.character),as.factor)) 
names(train) <- tolower(names(train))

validation <- read_csv("./test.csv", col_names=TRUE) %>% 
  mutate(across(where(is.character),as.factor))
names(validation) <- tolower(names(validation))

# Create a table of variable names to apply to the train set
labs <- readLines("./data_description.txt") %>% 
  tibble() %>% 
  separate('.', into=c("col1","col2","col3"), sep="\t") %>% 
  separate(col1, into=c("var","lab"),sep=":") %>% 
  filter(is.na(lab)==FALSE) %>% 
  select(lab) %>% 
  mutate(lab = str_trim(lab)) %>% 
  as_vector()
train <- set_label(train,label=labs)


###########################################
# Preprocessing
###########################################

# Data clean up
  train <- train %>% 
    mutate(across(where(is.numeric),~replace_na(.x,0))) %>% 
    mutate(lotfrontage = log(lotfrontage+1),
           lotarea = log(lotarea),           
           masvnrarea = ifelse(is.na(masvnrarea)==TRUE,0,masvnrarea),
           bsmtfinsf1 = log(bsmtfinsf1+1),
           bsmtunfsf = log(bsmtunfsf+1),
           totalbsmtsf = log(totalbsmtsf+1),
           masvnrarea = ifelse(is.na(masvnrarea)==TRUE,0,masvnrarea),
           alley = factor(ifelse(is.na(alley)==TRUE,'None',as.character(alley))),
           lotshape = factor(ifelse(lotshape=='Reg','Reg','Irreg')),
           masvnrtype = factor(ifelse(is.na(masvnrtype)==TRUE,'None',as.character(masvnrtype))),
           bsmtqual = factor(ifelse(is.na(bsmtqual)==TRUE,'NA',as.character(bsmtqual))),
           bsmtexposure = factor(ifelse(is.na(bsmtexposure)==TRUE,'NA',as.character(bsmtexposure))),
           bsmtfintype1 = factor(ifelse(is.na(bsmtfintype1)==TRUE,'NA',as.character(bsmtfintype1))),
           fireplacequ = factor(ifelse(is.na(fireplacequ)==TRUE,"None",as.character(fireplacequ))),
           garagetype = factor(ifelse(is.na(garagetype)==TRUE,"None",as.character(garagetype))),
           garageyrblt = factor(ifelse(is.na(garageyrblt)==TRUE,'None',as.character(garageyrblt))),
           garagefinish = factor(ifelse(is.na(garagefinish)==TRUE, 'None',as.character(garagefinish))),
           poolqc = factor(ifelse(is.na(poolqc)==TRUE,"None",as.character(poolqc))),
           fence = factor(ifelse(is.na(fence)==TRUE, 'None',as.character(fence))),
           electrical = factor(ifelse(is.na(electrical)==TRUE, "NA",as.character(electrical))))
  
  validation <- validation %>% 
    mutate(across(where(is.numeric),~replace_na(.x,0))) %>% 
    mutate(lotfrontage = log(lotfrontage+1),
           lotarea = log(lotarea),           
           masvnrarea = ifelse(is.na(masvnrarea)==TRUE,0,masvnrarea),
           bsmtfinsf1 = log(bsmtfinsf1+1),
           bsmtunfsf = log(bsmtunfsf+1),
           totalbsmtsf = log(totalbsmtsf+1),
           masvnrarea = ifelse(is.na(masvnrarea)==TRUE,0,masvnrarea),
           alley = factor(ifelse(is.na(alley)==TRUE,'None',as.character(alley))),
           lotshape = factor(ifelse(lotshape=='Reg','Reg','Irreg')),
           masvnrtype = factor(ifelse(is.na(masvnrtype)==TRUE,'None',as.character(masvnrtype))),
           bsmtqual = factor(ifelse(is.na(bsmtqual)==TRUE,'NA',as.character(bsmtqual))),
           bsmtexposure = factor(ifelse(is.na(bsmtexposure)==TRUE,'NA',as.character(bsmtexposure))),
           bsmtfintype1 = factor(ifelse(is.na(bsmtfintype1)==TRUE,'NA',as.character(bsmtfintype1))),
           fireplacequ = factor(ifelse(is.na(fireplacequ)==TRUE,"None",as.character(fireplacequ))),
           garagetype = factor(ifelse(is.na(garagetype)==TRUE,"None",as.character(garagetype))),
           garageyrblt = factor(ifelse(is.na(garageyrblt)==TRUE,'None',as.character(garageyrblt))),
           garagefinish = factor(ifelse(is.na(garagefinish)==TRUE, 'None',as.character(garagefinish))),
           poolqc = factor(ifelse(is.na(poolqc)==TRUE,"None",as.character(poolqc))),
           fence = factor(ifelse(is.na(fence)==TRUE, 'None',as.character(fence))),
           electrical = factor(ifelse(is.na(electrical)==TRUE, "NA",as.character(electrical))))
  
  # Find variables in the data that have near zero variance and drop those
  # from botht the train and validation sets
  nzv = nearZeroVar(train, names=TRUE)
  
  #print list of deleted columns
  print(nzv)
  
  train <- train %>% select(-c(all_of(nzv)),-mssubclass)
  validation <- validation %>% select(-c(all_of(nzv)),-mssubclass)
  
# Summary of final train data
view(dfSummary(train, plain.ascii = FALSE, style = "grid", 
               graph.magnif = 0.75, valid.col = FALSE, tmp.img.dir = "/tmp"))



#########################################
# data charts
#########################################


ggplot(train, aes(reorder(mszoning,saleprice,median),saleprice)) +
    geom_boxplot(fill='steelblue') +
  coord_flip() +
  scale_y_log10()

ggplot(train, aes(reorder(lotshape,saleprice,median),saleprice)) +
  geom_boxplot(fill='steelblue') +
  coord_flip() +
  scale_y_log10()

ggplot(train, aes(reorder(lotconfig,saleprice,median),saleprice)) +
  geom_boxplot(fill='steelblue') +
  coord_flip() +
  scale_y_log10()

ggplot(train, aes(reorder(neighborhood,saleprice,median),saleprice)) +
  geom_boxplot(fill='steelblue') +
  coord_flip() +
  scale_y_log10()

ggplot(train, aes(reorder(condition1,saleprice,median),saleprice)) +
  geom_boxplot(fill='steelblue') +
  coord_flip() +
  scale_y_log10()

ggplot(train, aes(reorder(bldgtype,saleprice,median),saleprice)) +
  geom_boxplot(fill='steelblue') +
  coord_flip() +
  scale_y_log10()

ggplot(train, aes(reorder(housestyle,saleprice,median),saleprice)) +
  geom_boxplot(fill='steelblue') +
  coord_flip() +
  scale_y_log10()

ggplot(train, aes(reorder(roofstyle,saleprice,median),saleprice)) +
  geom_boxplot(fill='steelblue') +
  coord_flip() +
  scale_y_log10()

ggplot(train, aes(reorder(exterior1st,saleprice,median),saleprice)) +
  geom_boxplot(fill='steelblue') +
  coord_flip() +
  scale_y_log10()

ggplot(train, aes(reorder(exterior2nd,saleprice,median),saleprice)) +
  geom_boxplot(fill='steelblue') +
  coord_flip() +
  scale_y_log10()

ggplot(train, aes(reorder(masvnrtype,saleprice,median),saleprice)) +
  geom_boxplot(fill='steelblue') +
  coord_flip() +
  scale_y_log10()

ggplot(train, aes(reorder(exterqual,saleprice,median),saleprice)) +
  geom_boxplot(fill='steelblue') +
  coord_flip() +
  scale_y_log10()

ggplot(train, aes(reorder(foundation,saleprice,median),saleprice)) +
  geom_boxplot(fill='steelblue') +
  coord_flip() +
  scale_y_log10()

ggplot(train, aes(reorder(bsmtqual,saleprice,median),saleprice)) +
  geom_boxplot(fill='steelblue') +
  coord_flip() +
  scale_y_log10()

ggplot(train, aes(reorder(bsmtexposure,saleprice,median),saleprice)) +
  geom_boxplot(fill='steelblue') +
  coord_flip() +
  scale_y_log10()

ggplot(train, aes(reorder(bsmtfintype1,saleprice,median),saleprice)) +
  geom_boxplot(fill='steelblue') +
  coord_flip() +
  scale_y_log10()

ggplot(train, aes(reorder(heatingqc,saleprice,median),saleprice)) +
  geom_boxplot(fill='steelblue') +
  coord_flip() +
  scale_y_log10()

ggplot(train, aes(reorder(centralair,saleprice,median),saleprice)) +
  geom_boxplot(fill='steelblue') +
  coord_flip() +
  scale_y_log10()

ggplot(train, aes(reorder(electrical,saleprice,median),saleprice)) +
  geom_boxplot(fill='steelblue') +
  coord_flip() +
  scale_y_log10()

ggplot(train, aes(reorder(kitchenqual,saleprice,median),saleprice)) +
  geom_boxplot(fill='steelblue') +
  coord_flip() +
  scale_y_log10()

ggplot(train, aes(reorder(fireplacequ,saleprice,median),saleprice)) +
  geom_boxplot(fill='steelblue') +
  coord_flip() +
  scale_y_log10()

ggplot(train, aes(reorder(garagetype,saleprice,median),saleprice)) +
  geom_boxplot(fill='steelblue') +
  coord_flip() +
  scale_y_log10()

ggplot(train, aes(reorder(garagefinish,saleprice,median),saleprice)) +
  geom_boxplot(fill='steelblue') +
  coord_flip() +
  scale_y_log10()
  
ggplot(train, aes(reorder(paveddrive,saleprice,median),saleprice)) +
  geom_boxplot(fill='steelblue') +
  coord_flip() +
  scale_y_log10()

ggplot(train, aes(reorder(fence,saleprice,median),saleprice)) +
  geom_boxplot(fill='steelblue') +
  coord_flip() +
  scale_y_log10()

ggplot(train, aes(reorder(saletype,saleprice,median),saleprice)) +
  geom_boxplot(fill='steelblue') +
  coord_flip() +
  scale_y_log10()

ggplot(train, aes(reorder(salecondition,saleprice,median),saleprice)) +
  geom_boxplot(fill='steelblue') +
  coord_flip() +
  scale_y_log10()