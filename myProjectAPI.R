#myProjectAPI.R
#load libraries
library(tidyverse)
library(caret)
#read in data
diabetes1 <- read_csv("diabetes.csv", show_col_types = FALSE)
#convert to factors
diabetes1  <- diabetes1 |>
  mutate(Diabetes_binary = as.factor(Diabetes_binary),
         Age = factor(Age,ordered = TRUE, levels = 
                        c("Age_18to24", "Age_25to29", 
                          "Age_30to34", "Age_35to39",
                          "Age_40to44", "Age_45to49", 
                          "Age_50to54", "Age_55to59", 
                          "Age_60to64", "Age_65to69", 
                          "Age_70to74", "Age_75to79", 
                          "Age_80_or_above")),
         Education = factor(Education, ordered = TRUE, levels =
                              c("No_School", "Primary_and_Middle",
                                "Some_High_School", "Graduated_High_School",
                                "Some_College", "Graduated_College")),
         Sex = as.factor(Sex),
         Income = factor(Income, ordered = TRUE, levels = 
                           c("Less_than_10K",
                             "From_10K_to_under_15K",
                             "From_15K_to_under_20K",
                             "From_20K_to_under_25K", 
                             "From_25K_to_under_35K",
                             "From_35K_to_under_50K", 
                             "From_50K_to_under_75K",
                             "From_75K_or_more")),
         DiffWalk = as.factor(DiffWalk),
         NoDocbcCost = as.factor(NoDocbcCost),
         AnyHealthcare = as.factor(AnyHealthcare),
         HvyAlcoholConsump = as.factor(HvyAlcoholConsump),
         Smoker = as.factor(Smoker),
         CholCheck = as.factor(CholCheck),
         PhysActivity = as.factor(PhysActivity),
         HeartDiseaseorAttack = as.factor(HeartDiseaseorAttack),
         Stroke = as.factor(Stroke),
         HighBP = as.factor(HighBP),
         HighChol = as.factor(HighChol),
         Fruits = as.factor(Fruits),
         Veggies = as.factor(Veggies),
         GenHlth = factor(GenHlth, ordered = TRUE, levels = 
                            c("Excellent", "Very_Good", "Good", 
                              "Fair", "Poor")))
#Set seed for reproducibility
set.seed(100)
#partition data
trainIndex <- createDataPartition(diabetes1$Diabetes_binary, 
                                  p = 0.7, 
                                  list = FALSE)
#create training set
diabetesTrain1 <- diabetes1[trainIndex, ]
#create test set
diabetesTest1 <- diabetes1[-trainIndex, ]

#Set seed for reproducibility
set.seed(100)
#Run logistic regression on Model 1
logRegFit_M1 <- train(Diabetes_binary ~ ., 
                      data = diabetesTrain1,
                      method = "glm",
                      family = "binomial",
                      metric = "logLoss",
                      trControl = trainControl(method = "cv", 
                                               number = 5,
                                               classProbs = TRUE,
                                               summaryFunction = mnLogLoss))



#Info
#* @get /Info
function() {
  "Melanie Beebe - https://mbb477.github.io/FinalProjectMB/"
}

#Get prediction for logistic regression based on user input
#* @param HighBP Input one option (Yes, No)
#* @param HighChol Input one option (Yes, No)
#* @param CholCheck Input one option (Yes, No)
#* @param Smoker Input one option (Yes, No)
#* @param Stroke Input one option (Yes, No)
#* @param HeartDiseaseorAttack Input one option (Yes, No)
#* @param PhysActivity Input one option (Yes, No)
#* @param Fruits Input one option (None, One_or_more_per_day)
#* @param Veggies Input one option (None, One_or_more_per_day)
#* @param HvyAlcoholConsump Input one option (Yes, No)
#* @param AnyHealthcare Input one option (Yes, No)
#* @param DiffWalk Input one option (Yes, No)
#* @param Sex Input one option (Female, Male)
#* @param NoDocbcCost Input one option (Yes, No)
#* @param BMI A number between 15 and 100
#* @param GenHlth Input one option (Excellent, Very_Good, Good, Fair, Poor)
#* @param MentHlth A number between 0 and 30
#* @param PhysHlth A number between 0 and 30
#* @param Age Input one option (Age_18to24, Age_25to29, Age_30to34, Age_35to39, Age_40to44, Age_45to49, Age_50to54, Age_55to59, Age_60to64, Age_65to69, Age_70to74, Age_75to79, Age_80_or_above)
#* @param Education Input one option (No_School, Primary_and_Middle, Some_High_School, Graduated_High_School, Some_College, Graduated_College) 
#* @param Income Input one option (Less_than_10K, From_10K_to_under_15K, From_15K_to_under_20K, From_20K_to_under_25K, From_25K_to_under_35K, From_35K_to_under_50K, From_50K_to_under_75K, From_75k_or_more)
#* @get /pred
function(HighBP = "No", HighChol = "No", CholCheck = "Yes", Smoker = "No", Stroke = "No",
         HeartDiseaseorAttack = "No", PhysActivity = "Yes", Fruits = "One_or_more_per_day",
         Veggies = "One_or_more_per_day", HvyAlcoholConsump = "No", AnyHealthcare = "Yes",
         DiffWalk = "No", Sex = "Female", NoDocbcCost = "No", BMI = 28.38, GenHlth = "Very_Good",
         MentHlth = 3.18, PhysHlth = 4.24, Age = "Age_60to64", Education = "Graduated_College",
         Income = "From_75K_or_more") {
  user_input <- tibble(
    Age = factor(Age,ordered = TRUE, levels = 
                   c("Age_18to24", "Age_25to29", 
                     "Age_30to34", "Age_35to39",
                     "Age_40to44", "Age_45to49", 
                     "Age_50to54", "Age_55to59", 
                     "Age_60to64", "Age_65to69", 
                     "Age_70to74", "Age_75to79", 
                     "Age_80_or_above")),
    Education = factor(Education, ordered = TRUE, levels =
                         c("No_School", "Primary_and_Middle",
                           "Some_High_School", "Graduated_High_School",
                           "Some_College", "Graduated_College")),
    Sex = factor(Sex, levels = c("Female", "Male")),
    Income = factor(Income, ordered = TRUE, levels = 
                      c("Less_than_10K",
                        "From_10K_to_under_15K",
                        "From_15K_to_under_20K",
                        "From_20K_to_under_25K", 
                        "From_25K_to_under_35K",
                        "From_35K_to_under_50K", 
                        "From_50K_to_under_75K",
                        "From_75K_or_more")),
    DiffWalk = factor(DiffWalk, levels = c("No", "Yes")),
    NoDocbcCost = factor(NoDocbcCost, levels = c("No", "Yes")),
    AnyHealthcare = factor(AnyHealthcare, levels = c("No", "Yes")),
    HvyAlcoholConsump = factor(HvyAlcoholConsump, levels = c("No", "Yes")),
    Smoker = factor(Smoker, levels = c("No", "Yes")),
    CholCheck = factor(CholCheck, levels = c("No", "Yes")),
    PhysActivity = factor(PhysActivity, levels = c("No", "Yes")),
    HeartDiseaseorAttack = factor(HeartDiseaseorAttack, levels = c("No", "Yes")),
    Stroke = factor(Stroke, levels = c("No", "Yes")),
    HighBP = factor(HighBP, levels = c("No", "Yes")),
    HighChol = factor(HighChol, levels = c("No", "Yes")),
    Fruits = factor(Fruits, levels = c("None", "One_or_more_per_day")),
    Veggies = factor(Veggies, levels = c("None", "One_or_more_per_day")),
    GenHlth = factor(GenHlth, ordered = TRUE, levels = 
                       c("Excellent", "Very_Good", "Good", 
                         "Fair", "Poor")),
    BMI = as.numeric(BMI),
    PhysHlth = as.numeric(PhysHlth),
    MentHlth = as.numeric(MentHlth))
  
  #obtain predictions using test data
  pred <- predict(logRegFit_M1, newdata = user_input, type = "prob")
  #Return prediction
  list(pred = pred)
}

# Some API call URLs

#Call 1
#  http://127.0.0.1:8000/pred?HighBP=No&HighChol=No&CholCheck=Yes&Smoker=No&Stroke=No&HeartDiseaseorAttack=No&PhysActivity=Yes&Fruits=None&Veggies=One_or_more_per_day&HvyAlcoholConsump=No&AnyHealthcare=Yes&DiffWalk=No&Sex=Male&NoDocbcCost=No&BMI=24&GenHlth=Very_Good&MentHlth=0&PhysHlth=0&Age=Age_55to59&Education=Graduated_High_School&Income=From_15K_to_under_20K

#Call 2
#  http://127.0.0.1:8000/pred?HighBP=Yes&HighChol=Yes&CholCheck=Yes&Smoker=No&Stroke=No&HeartDiseaseorAttack=No&PhysActivity=No&Fruits=One_or_more_per_day&Veggies=One_or_more_per_day&HvyAlcoholConsump=No&AnyHealthcare=Yes&DiffWalk=Yes&Sex=Female&NoDocbcCost=Yes&BMI=28&GenHlth=Poor&MentHlth=30&PhysHlth=30&Age=Age_60to64&Education=Graduated_High_School&Income=From_75K_or_more

#Call 3
#  http://127.0.0.1:8000/pred?HighBP=Yes&HighChol=Yes&CholCheck=Yes&Smoker=Yes&Stroke=Yes&HeartDiseaseorAttack=Yes&PhysActivity=No&Fruits=None&Veggies=One_or_more_per_day&HvyAlcoholConsump=No&AnyHealthcare=Yes&DiffWalk=Yes&Sex=Male&NoDocbcCost=No&BMI=37&GenHlth=Poor&MentHlth=0&PhysHlth=0&Age=Age_65to69&Education=Graduated_College&Income=From_25K_to_under_35K


