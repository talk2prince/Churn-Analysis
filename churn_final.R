library(caretEnsemble)
library(shinyWidgets)
library(shiny)
library(shinycssloaders)
library(shinydashboardPlus)
library(tidyverse)
library(janitor)
library(DT)
library(caret)
library(rpart)
library(rpart.plot)
library(randomForest)
library(ROSE)
library(plotly)
library(IRdisplay)
library(e1071)
library(readr)
library(shinyjs)
library(rhandsontable)
library(shinydashboardPlus)
library(shinydashboard)
library(LiblineaR)
femb_transaction<-data.frame('User_Id' = c('1','',''),'TimeStamp' = c('11-04-2019','',''),'Data' = c('Churn Master','',''),'Resampling' = c('cv','',''),'Model' = c('Random Forest','Decision Tree','Log Reg'))

hypothesis_DF <- data.frame(Crisp.ID = c(1,2),UID = c("CM1-1","CM1-2"),Description = c("Desc1","Desc2"),Status = c("High","Medium"),Created.By = c("ABC","XYZ"),Created.On = c("2019-04-10","2019-04-11"))
hypothesis_DF$Created.On <- as.Date(hypothesis_DF$Created.On)
hypothesis_DF$Created.By <- as.character(hypothesis_DF$Created.By)
hypothesis_DF$Description <- as.character(hypothesis_DF$Description)
hypothesis_DF$Status <- as.character(hypothesis_DF$Status)
hypothesis_DF$UID <- as.character(hypothesis_DF$UID)
hypothesis_DF$Crisp.ID <- as.integer(hypothesis_DF$Crisp.ID)
db_churn_master<-read_csv('db_churn.csv')
db_churn_master_SC <- db_churn_master %>% filter(SeniorCitizen==1)
db_churn_master_NSC <- db_churn_master %>% filter(SeniorCitizen==0)
db_churn2<-db_churn_master

ui<-fluidPage(
  dashboardPage(
    dashboardHeader(disable = T),
    dashboardSidebar(disable = T),
    dashboardBody(
      setBackgroundColor(color = "#F5F5DC", gradient = c("linear",
                                                            "radial"), direction = c("bottom", "top", "right", "left")),
  # useShinydashboard(),
  # useShinydashboardPlus(),
  tabsetPanel(
    
    tabPanel(title = 'CRISP Modeler',
      
 
  
      h3('Churn Analysis'),
      br(),
  # boxPlus(
  #   collapsible = F,closable = F,width = 12,
  
  
    h3(strong('Business Understanding')),
  
  
  shinyWidgets::panel(heading = 'Data Description',status = 'primary',
    
                      tabsetPanel(
                        tabPanel(
                          title = h4('Main'),
                          textAreaInput(width = '850px',height = '150px',inputId = 'main_description',label = '',value = 'Customer churn analysis refers to the customer attrition rate in a company.Churn rate (sometimes called attrition rate), in its broadest sense, is a measure of the number of individuals or items moving out of a collective group over a specific period.')
                        ),
                        tabPanel(
                          title = h4('Notes'),
                          textAreaInput(width = '850px',height = '150px',inputId = 'notes_description',label = '',value = '')
                        )
                        
                      )
  ),
  

  shinyWidgets::panel(heading = 'Hypothesis Master',status = 'primary',
    fluidRow(
      
      column(
        width = 12,
        actionBttn("edit_rhandsontable",label = "Edit Table",icon = icon("fas fa-edit"),no_outline = T,color = "primary",style = "gradient",size = "sm"),
        actionBttn("new_rhandsontable",label = "New Row",icon = icon("fas fa-plus-circle"),no_outline = T,color = "success",style = "gradient",size = "sm"),
        br()
       
      )
    ),
    fluidRow(
      column(
        width = 12,
        rHandsontableOutput("hypothesis_table")
      )
    )
    
  ),
  

  br(),
    h3(strong('Data Understanding')),
    
  shinyWidgets::panel(heading = 'Realization',status = 'primary',
                      textAreaInput(width = '850px',height = '150px',inputId = 'data_realization',label = '',value = '')
                      
  ),
  
  
  shinyWidgets::panel(heading = 'Import Data',status = 'primary',
                      
                      fluidRow(
                        column(
                          width=4,
                          fileInput("choose_file",label = "Choose File",placeholder = "churn.csv")
                        ),
                        column(
                          width = 4,
                          br(),
                          actionBttn('submit_churn',"Submit",color = 'default',size = 'sm',no_outline = TRUE,style='gradient')
                        ),
                        column(
                          width = 4,
                          br(),
                          actionBttn('show_churn_data',"Show Data",color = 'default',size = 'sm',no_outline = TRUE,style='gradient')
                        )
                        
                        
                      ),
                      
                      conditionalPanel(condition = 'input.show_churn_data%2==1',
                                       boxPlus(
                                         width = 12,closable = F,collapsible = F,
                                         fluidRow(
                                           column(
                                             width = 12,
                                             br(),
                                             h4('Dataset File'),
                                             dataTableOutput('churn_data'),style = "height:500px; overflow-y: scroll;"
                                           ))
                                       ))
  ),
  conditionalPanel(condition = 'input.submit_churn%2==1',
                   
                   shinyWidgets::panel(heading = 'Filter Data',status = 'primary',
                                       fluidRow(
                                         column(
                                           width = 12,
                                           #tags$style(type="text/css", HTML("#ms>*{float: left; margin-right: 15px; height: 20px;} #test {height: 20px;}"))
                                           prettyCheckboxGroup(inputId = 'attribute_group',label = 'Attributes',choices = colnames(db_churn_master),inline = TRUE,selected = NULL,status = 'primary')
                                           
                                         ) ),
                                       fluidRow(
                                         column(
                                           width = 4,
                                           actionBttn(inputId = 'apply_filter',label = 'Apply',size = 'sm',no_outline = TRUE,style='gradient')
                                           
                                         ),
                                         column(
                                           width = 4,
                                           actionBttn(inputId = 'clear_filter',label = 'Clear',size = 'sm',no_outline = TRUE,style='gradient')
                                           
                                         )
                                       ),
                                       fluidRow(
                                         
                                         column(
                                           
                                           width = 12,
                                           br(),
                                           #conditionalPanel(condition = 'input.apply_filter%2 == 1',
                                           dataTableOutput('filter_churn_table')
                                         )
                                         
                                         
                                         
                                         
                                       ))),
  
                   shinyWidgets::panel(heading = 'Exploratory Data Analysis',status = 'primary',
                                       
                                       fluidRow(
                                         column(   
                                           width = 12,
                                           plotOutput('plot1'),
                                           br()
                                         )
                                       ),
                                       # fluidRow(
                                       #   column(   
                                       #     width = 12,
                                       #     plotOutput('plot2'),
                                       #     br()
                                       #   )
                                       # ),
                                       # fluidRow(
                                       #   column(   
                                       #     width = 12,
                                       #     plotOutput('plot3'),
                                       #     br()
                                       #   )
                                       # ),
                                       # fluidRow(
                                       #   column(   
                                       #     width = 12,
                                       #     plotlyOutput('plot4'),
                                       #     br()
                                       #   )
                                       # ),
                                       # fluidRow(
                                       #   column(   
                                       #     width = 12,
                                       #     plotlyOutput('plot5'),
                                       #     br()
                                       #   )
                                       # )
                                       fluidRow(
                                         column(
                                           width = 6,
                                           plotlyOutput("MF_pie_chart")      
                                         ),
                                         column(
                                           width = 6,
                                           plotlyOutput("senior_citizen_pie_chart")
                                         )
                                       ),
                                       fluidRow(
                                         column(
                                           width = 6,
                                           plotlyOutput("partner_pie_chart")
                                         ),
                                         column(
                                           width = 6,
                                           plotlyOutput("phone_service_pie_chart")
                                         )
                                       ),
                                       fluidRow(
                                         column(
                                           width = 12,
                                           plotlyOutput("tenure_donut_chart",height = 600)
                                         )
                                       ),
                                       fluidRow(
                                         column(
                                           width = 6,
                                           h4(strong("Contract Ratio")),
                                           plotlyOutput("contract_HR_bar_chart")
                                         ),
                                         column(
                                           width = 6,
                                           h4(strong("Payment Method Ratio")),
                                           plotlyOutput("payment_method_HR_bar_chart")
                                         )
                                       ),
                                       fluidRow(
                                         column(
                                           width = 12,
                                           plotlyOutput("monthly_charges_box_plot")
                                         )
                                       ),
                                       fluidRow(
                                         column(
                                           width = 12,
                                           plotlyOutput("total_charges_box_plot")
                                         )
                                       ),
                                       fluidRow(
                                         column(
                                           width = 12,
                                           plotlyOutput("churn_rate_pie_chart")
                                         )
                                       ),
                                       fluidRow(
                                         column(
                                           width = 12,
                                           h3(strong("Senior Citizen"))
                                         )
                                       ),
                                       fluidRow(
                                         column(
                                           width = 12,
                                           plotlyOutput("MF_pie_chart_SC")      
                                         )
                                       ),
                                       fluidRow(
                                         column(
                                           width = 6,
                                           plotlyOutput("partner_pie_chart_SC")
                                         ),
                                         column(
                                           width = 6,
                                           plotlyOutput("phone_service_pie_chart_SC")
                                         )
                                       ),
                                       fluidRow(
                                         column(
                                           width = 12,
                                           plotlyOutput("tenure_donut_chart_SC",height = 600)
                                         )
                                       ),
                                       fluidRow(
                                         column(
                                           width = 6,
                                           h4(strong("Contract Ratio")),
                                           plotlyOutput("contract_HR_bar_chart_SC")
                                         ),
                                         column(
                                           width = 6,
                                           h4(strong("Payment Method Ratio")),
                                           plotlyOutput("payment_method_HR_bar_chart_SC")
                                         )
                                       ),
                                       fluidRow(
                                         column(
                                           width = 12,
                                           plotlyOutput("monthly_charges_box_plot_SC")
                                         )
                                       ),
                                       fluidRow(
                                         column(
                                           width = 12,
                                           plotlyOutput("total_charges_box_plot_SC")
                                         )
                                       ),
                                       fluidRow(
                                         column(
                                           width = 12,
                                           plotlyOutput("churn_rate_pie_chart_SC")
                                         )
                                       ),
                                       fluidRow(
                                         column(
                                           width = 12,
                                           h3(strong("Non Senior Citizen"))
                                         )
                                       ),
                                       fluidRow(
                                         column(
                                           width = 12,
                                           plotlyOutput("MF_pie_chart_NSC")      
                                         )
                                       ),
                                       fluidRow(
                                         column(
                                           width = 6,
                                           plotlyOutput("partner_pie_chart_NSC")
                                         ),
                                         column(
                                           width = 6,
                                           plotlyOutput("phone_service_pie_chart_NSC")
                                         )
                                       ),
                                       fluidRow(
                                         column(
                                           width = 12,
                                           plotlyOutput("tenure_donut_chart_NSC",height = 600)
                                         )
                                       ),
                                       fluidRow(
                                         column(
                                           width = 6,
                                           h4(strong("Contract Ratio")),
                                           plotlyOutput("contract_HR_bar_chart_NSC")
                                         ),
                                         column(
                                           width = 6,
                                           h4(strong("Payment Method Ratio")),
                                           plotlyOutput("payment_method_HR_bar_chart_NSC")
                                         )
                                       ),
                                       fluidRow(
                                         column(
                                           width = 12,
                                           plotlyOutput("monthly_charges_box_plot_NSC")
                                         )
                                       ),
                                       fluidRow(
                                         column(
                                           width = 12,
                                           plotlyOutput("total_charges_box_plot_NSC")
                                         )
                                       ),
                                       fluidRow(
                                         column(
                                           width = 12,
                                           plotlyOutput("churn_rate_pie_chart_NSC")
                                         )
                                       ) 
                                       
                                       
                   ),
                   shinyWidgets::panel(heading = 'Model Building',status = 'primary',
                                       h3('Resampling'),
                                       fluidRow(
                                         column(
                                           width = 4,
                                           selectInput('method_selection','Method:',choices = c('cv','repeatedcv','LOOCV','LGOCV','BOOT'),selected = 'cv')  
                                         ),
                                         column(
                                           width = 4,
                                           textInput('test_train','Train/Test Ratio')  
                                         ),
                                         column(
                                           width = 4,
                                           selectInput('Model_Type','Model Type',c('Random Forest','Log Reg','Decision Tree'))
                                         )
                                         
                                       ),
                                       fluidRow(
                                         column(
                                           width = 4,
                                           textInput('number_times',"Number")
                                         ),
                                         column(
                                           width = 4,
                                           br(),
                                           actionBttn(inputId = 'apply_resample',label = 'Apply',size = 'sm',no_outline = TRUE,style='gradient')
                                         )
                                       ),
                                       fluidRow(
                                         
                                         column(
                                           h3('Resampling Result'),
                                           width = 12,
                                           dataTableOutput('result_resample')
                                         )
                                       ),
                                       fluidRow(
                                         column(
                                         h3('Variable Importance'),
                                         width = 12,
                                         plotOutput('variable_importance')
                                       )
                                       )
                   ),
                   shinyWidgets::panel(heading = 'Model Performance',status = 'primary',
                                       fluidRow(
                                         column(
                                           width = 12,
                                           h3('Train Data'),
                                           plotOutput('model_output_train')
                                         )
                                       ),
                                       fluidRow(
                                         column(
                                           width = 12,
                                           h3('Test Data'),
                                           plotOutput('model_output')
                                         )
                                       ),
                                       fluidRow(
                                         h3(''),
                                         column(
                                           width = 12,
                                           dataTableOutput('rf_result')
                                         )
                                       ),
                                       fluidRow(
                                         column(
                                           width = 12,
                                           h3('Save Model'),
                                           br(),
                                           actionBttn(inputId = 'save_model',label = 'Save',size = 'sm',no_outline = TRUE,style='gradient')
                                         )
                                       )
                                       
                   ),
                   shinyWidgets::panel(heading = 'Model Selection',status = 'primary',
                                       fluidRow(
                                         column(
                                           width = 12,
                                           h3('Ensemble Prediction'),
                                           prettyCheckboxGroup(inputId = 'model_select_checkbox',label = 'Models',choices = c('rf','regLogistic'),inline = TRUE,selected = NULL,status = 'primary'),
                                           actionBttn('Combining_model',label = 'Start',color = 'default',size = 'sm',no_outline = TRUE,style='gradient'),
                                           plotOutput('merge_plot'),
                                           br(),
                                           h3('Random Forest'),
                                           dataTableOutput('model_result_table'),
                                           br(),
                                           h3('Regression Model'),
                                           dataTableOutput('model_result_table2')
                                           
                                         ),
                                         
                                         fluidRow(
                                           column(
                                             width = 12,
                                             h3('Combined Prediction'),
                                             dataTableOutput('combined_prediction')
                                           )
                                         )
                                       )),
                   
                   shinyWidgets::panel(heading = 'Model Evaluation',status = 'primary',
                                       h3('Analytic Framework'),
                                       textAreaInput(width = '850px',height = '150px',inputId = 'analyic_textinput',label = '',value = ''),
                                       rHandsontableOutput('model_result_output'),
                                       br(),
                                       h3('Result'),
                                       dataTableOutput('rf_output')
                                       
                   
                                       ),
  
                   shinyWidgets::panel(heading = 'Prediction',status = 'primary',
                                       tabsetPanel(
                                         tabPanel(
                                           title = h4('Individual'),
                                       fluidRow(
                                         column(
                                           width = 3,
                                           textInput('customer_id','Customer ID')
                                         ),
                                         column(
                                           width = 3,
                                           selectInput('gender','Gender',choices = c('Male','Female'))
                                         ),
                                         column(
                                           width = 3,
                                           selectInput('seniorCitizen','Senior Citizen',choices = c('Yes','No'))
                                         ),
                                         column(
                                           width = 3,
                                           selectInput('partner','Partner',choices = c('Yes','No'))
                                           
                                         )
                                       ),
                                       fluidRow(
                                         column(
                                           width = 3,
                                           selectInput('dependents','Dependents',choices = c('Yes','No'))
                                         ),
                                         column(
                                           width = 3,
                                           textInput('tenure','Tenure',placeholder = 34)
                                         ),
                                         column(
                                           width = 3,
                                           selectInput('phoneService','PhoneService',choices = c('Yes','No'))
                                         ),
                                         column(
                                           width = 3,
                                           selectInput('MultipleLines','MultipleLines',choices = c('Yes','No'))
                                           
                                         )
                                         
                                       ),
                                       fluidRow(
                                         column(
                                           width = 3,
                                           selectInput('InternetService','InternetService',choices = c(unique(db_churn2$InternetService)))
                                           
                                         ),
                                         column(
                                           width = 3,
                                           selectInput('OnlineSecurity','OnlineSecurity',choices = c('Yes','No'))
                                           
                                         ),
                                         column(
                                           width =3,
                                           selectInput('OnlineBackup','OnlineBackup',choices = c('Yes','No'))
                                           
                                         ),
                                         column(
                                           width =3,
                                           selectInput('DeviceProtection','DeviceProtection',choices = c('Yes','No'))
                                           
                                         )
                                       ),
                                       fluidRow(
                                         column(
                                           width = 3,
                                           selectInput('TechSupport','TechSupport',choices = c('Yes','No'))
                                           
                                         ),
                                         column(
                                           width = 3,
                                           selectInput('StreamingTV','StreamingTV',choices = c('Yes','No'))
                                         ),
                                         
                                         column(
                                           width = 3,
                                           selectInput('StreamingMovies','StreamingMovies',choices = c('Yes','No'))
                                           
                                         ),
                                         column(
                                           width = 3,
                                           selectInput('Contract','Contract',choices = c(unique(db_churn2$Contract)))
                                           
                                         )
                                       ),
                                       fluidRow(
                                         column(
                                           width = 3,
                                           selectInput('PaperlessBilling','PaperlessBilling',choices = c('Yes','No'))
                                           
                                         ),
                                         column(
                                           width = 3,
                                           selectInput('PaymentMethod','PaymentMethod',choices = c(unique(db_churn2$PaymentMethod)))
                                           
                                         ),
                                         column(
                                           width = 3,
                                           textInput('MonthlyCharges','MonthlyCharges',placeholder = 75.5)
                                           
                                         ),
                                         column(
                                           width = 3,
                                           textInput('TotalCharges','TotalCharges',placeholder = 305.5)
                                           
                                         )
                                         
                                       ),
                                       fluidRow(
                                         column(
                                           width = 12,
                                           actionBttn('detect_churn',label = 'Submit',color = 'default',size = 'sm',no_outline = TRUE,style='gradient'),
                                           #dataTableOutput('data_test'),style = "height:500px; overflow-x: scroll;",
                                           br(),
                                           h3('Final Prediction'),
                                           tags$head(tags$style("#final_prediction{color: green;
                                                                font-size: 20px;
                                                                font-style: italic;
                                                                }"
                         )
                                           ),
                         useShinyjs(),verbatimTextOutput('final_prediction') #%>% withSpinner(color="#0dc5c1")
                                           )
                                           )),
                         
                         tabPanel(
                           title = h4('Group'),
                           
                           sidebarLayout(
                             
                            fluid = TRUE,
                             # Sidebar panel for inputs ----
                             sidebarPanel(
                               
                               # Input: Select the random distribution type ----
                               textInput('custinsegment','Customer in Segment'),
                               br(),
                               textInput('custlikelytchurn','Customer Likely to Churn'),
                               br(),
                               textInput('cltv','CLTV@Risk (INR, Cr)'),
                               br(),
                               textInput('cltv2','CLTV@Protected (INR, Cr)'),
                               br(),
                               verbatimTextOutput(outputId = 'probofchurn')
                               
                             ),
                             
                             # Main panel for displaying outputs ----
                             mainPanel(
                               fluidRow(
                                 column(
                                   width = 5,
                               h4('Segment'),
                               # Output: Tabset w/ plot, summary, and table ----
                               selectInput('age_category','Age Category',c('All','Senior Citizen','Non-Senior Citizen'),width = '200px'),
                               
                               selectInput('gender','Gender',c('All','Male','Female'),width = '200px'),
                               
                             
                               h4('Measures'),
                               textInput('Total_charges','Total Charges',width = '200px'),
                               
                               textInput('monthly_charges','Monthly Charges',width = '200px'),
                               
                               textInput('tenure','Tenure',width = '200px'),
                               actionBttn('submit_group_churn',label = 'Submit',color = 'default',size = 'sm',no_outline = TRUE,style='gradient')
                               
                             ),
                             column(
                               width = 4,
                               br(),
                               br(),
                               #br(),
                               textInput('sr_cit','',value = 17.43),
                               textInput('gender1','',value = 17.44),
                               br(),
                               br(),
                               
                               
                               textInput('total_charges','',value = 152.80),
                               textInput('mcharges','',value = 122.19),
                               textInput('tenure1','',value = 151.79 )
                               
                             )))
                           ),
 
                           fluidRow(
                             column(
                               h3('Sensitivity of Variables'),
                               width = 12,
                               img(src = "sens_plot.PNG",height = 550, width = 750)
                               
                             )
                           )
                           
                         ),
                         tabPanel(
                           title = h4('List'),
                         
                           fluidRow(
                             h3(''),
                             column(
                               width = 12,
                               dataTableOutput('end_list'),style = "height:500px; overflow-y: scroll;overflow-x: scroll;"
                             )
                           )
                         )
                         
                        )
                   
  )  
    ),
  tabPanel(title = 'Experiment'
    
  ),
  tabPanel(title = 'Data'
    
  )
  )
  ) ))

server<-function(input,output,session){
  
  ############analytic function############
  analytic_function <- function(fir1,fir2){
    
    path = "https://community.watsonanalytics.com/wp-content/uploads/2015/03/WA_Fn-UseC_-Telco-Customer-Churn.csv?cm_mc_uid=58920755505115141495567&cm_mc_sid_50200000=1514149556&cm_mc_sid_52640000=1514149556"
    db_churn<-read_csv(path)
    
    prop = tabyl(db_churn$Churn)
    #prop
    
    n_NA = db_churn %>%
      filter(is.na(TotalCharges)) %>%
      select(Churn)
    #11 NA, that 0.16% of our database and none of them decode to churn
    db_churn = db_churn %>%
      filter(!is.na(TotalCharges))
    set.seed(7)
    trainId = createDataPartition(db_churn$Churn, 
                                  p=0.7, list=FALSE,times=1)
    db_train = db_churn[trainId,]
    db_test = db_churn[-trainId,]
    gather_train =gather(db_train %>% 
                           select(customerID, MonthlyCharges,TotalCharges, tenure),
                         variable, value,
                         -customerID)
    normalize = function(x) {
      result = (x - min(x, na.rm = TRUE)
      ) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
      return(result)
    }
    norm.train = lapply(db_train %>% 
                          select(MonthlyCharges, TotalCharges, tenure),
                        normalize)
    norm.train = do.call(cbind, norm.train) %>%
      as.data.frame()
    factor.train = lapply(db_train %>% 
                            select(-customerID,-MonthlyCharges, 
                                   -TotalCharges, -tenure), 
                          function(x){
                            x = gsub("No internet service", "No", x)
                            x = gsub("No phone service", "No", x)
                            return(x)
                          })
    factor.train = do.call(cbind, factor.train) %>% 
      as.data.frame()
    db_train = cbind( customerID = db_train[,1], 
                      factor.train, norm.train)
    tree = rpart(Churn ~., data = db_train %>% 
                   select(-customerID), method="class")
    
    ctrl = trainControl(method = fir1, number=fir2, 
                        classProbs = TRUE, summaryFunction = twoClassSummary)
    model.rf = train(Churn ~., data = db_train %>% select(-customerID),
                     method = "rf",
                     ntree = 75,
                     tuneLength = 5,
                     metric = "ROC",
                     trControl = ctrl)
    #model.rf
    
    
    
    db_train$ChurnNum = ifelse(db_train$Churn == "Yes",1,0)
    good_model = step(glm(ChurnNum ~.,data = db_train %>% 
                            select(-customerID, -Churn ), 
                          family=binomial(link='logit')), 
                      direction="both")
    
    #summary(good_model)
    
    
    norm.test = lapply(db_test %>% select(MonthlyCharges,TotalCharges, tenure), normalize)
    norm.test = do.call(cbind, norm.test) %>%
      as.data.frame()
    factor.test = lapply(db_test %>% 
                           select(-customerID,-MonthlyCharges, 
                                  -TotalCharges, -tenure), 
                         function(x){
                           x = gsub("No internet service", "No", x)
                           x = gsub("No phone service", "No", x)
                           return(x)
                         })
    factor.test = do.call(cbind, factor.test) %>% as.data.frame()
    db_test = cbind( customerID = db_test[,1], factor.test , norm.test)
    db_test$ChurnNum  = ifelse(db_test$Churn == "Yes", 1, 0)
    pred_tree = predict(tree, db_test %>% 
                          select(-customerID), type = c("prob"))[,2]
    #######added###########
    pred_tree1 = predict(tree, db_train %>% 
                           select(-customerID), type = c("prob"))[,2]
    
    #############
    
    pred_rf = predict(object=model.rf, db_test %>% 
                        select(-customerID), type='prob')[,2]
    ##########added#########
    pred_rf1 = predict(object=model.rf, db_train %>% 
                         select(-customerID), type='prob')[,2]
    
    ############
    pred_logistic = predict(good_model, db_test, type="response")
    #######added###########
    pred_logistic1 = predict(good_model, db_train, type="response")
    
    
    #head(pred_logistic,5)
    
    ##          1          2          3          4          5 
    ## 0.62513394 0.32512336 0.52812027 0.31273422 0.01264661
    comp = cbind.data.frame(answer = db_test$ChurnNum, 
                            pred=pred_logistic) %>%
      arrange(desc(pred))
    indic_perf = function(x){
      compare = comp %>%
        mutate(pred = ifelse(pred>x,1,0))
      
      if(ncol(table(compare))>1){
        
        mat = confusionMatrix(table(compare), positive = "1")
        
        #acuracy 
        acc = mat$overall["Accuracy"]
        
        #Kappa
        kap = mat$overall["Kappa"]
        
        #sensitivity
        sen = mat$byClass["Sensitivity"]
        
        #F1
        f1_stat = mat$byClass["F1"]
        
        #Precision
        prec = mat$byClass["Precision"]
        
        
      }else{
        acc = NA
        prec = NA
        sen = NA 
        kap = NA
        f1_stat = NA
      }
      return(data.frame(threshold = x, accuracy = acc, 
                        precision = prec, sensitivity = sen, 
                        kappa = kap, f1= f1_stat))
    }
    indics = do.call(rbind, lapply(seq(0.05,0.95, by=0.001), 
                                   indic_perf)) %>%
      filter(!is.na(accuracy))
    gather_indics = tidyr::gather(indics, variable, 
                                  value, -threshold) %>%
      group_by(variable) %>%
      mutate(color =  (max(value) == value), 
             threshold = as.numeric(threshold) )
    
    max_indics = indics %>%
      filter(accuracy == max(accuracy, na.rm=TRUE) | precision == max(precision, na.rm = TRUE) | sensitivity == max(sensitivity, na.rm = TRUE) | kappa == max(kappa, na.rm = TRUE) | f1 == max(f1, na.rm = TRUE) )
    gather_indics = tidyr::gather(indics, variable, 
                                  value, -threshold) %>%
      group_by(variable) %>%
      mutate(color =  (max(value) == value), 
             threshold = as.numeric(threshold) )
    
    max_indics = indics %>%
      filter(accuracy == max(accuracy, na.rm=TRUE) | precision == max(precision, na.rm = TRUE) | sensitivity == max(sensitivity, na.rm = TRUE) | kappa == max(kappa, na.rm = TRUE) | f1 == max(f1, na.rm = TRUE) )
    max_indics %>%
      filter( threshold %in% c("0.057", "0.339", "0.584", "0.585", "0.799"))
    # threshold  accuracy  precision sensitivity      kappa         f1
    # 1     0.057 0.5237192 0.97857143   0.3558442 0.21671651 0.52190476
    # 2     0.339 0.7718216 0.75178571   0.5517693 0.47581274 0.63643235
    # 3     0.584 0.8026565 0.47142857   0.6875000 0.43782054 0.55932203
    # 4     0.585 0.8026565 0.46964286   0.6884817 0.43710748 0.55838641
    # 5     0.799 0.7405123 0.02321429   1.0000000 0.03372764 0.04537522
    compare = comp %>%
      mutate(pred = ifelse(pred>0.339,1,0))
    confusionMatrix(table(compare), positive = "1")
    # Confusion Matrix and Statistics
    # 
    # pred
    # answer    0    1
    # 0 1206  342
    # 1  139  421
    # 
    # Accuracy : 0.7718
    # 95% CI : (0.7533, 0.7896)
    # No Information Rate : 0.638
    # P-Value [Acc > NIR] : < 2.2e-16
    # 
    # Kappa : 0.4758
    # Mcnemar's Test P-Value : < 2.2e-16
    # 
    # Sensitivity : 0.5518          
    # Specificity : 0.8967          
    # Pos Pred Value : 0.7518          
    # Neg Pred Value : 0.7791          
    # Prevalence : 0.3620          
    # Detection Rate : 0.1997          
    # Detection Prevalence : 0.2657          
    # Balanced Accuracy : 0.7242          
    # 
    # 'Positive' Class : 1               
    
    fivtile = nrow(comp)/20
    step = floor(fivtile * 1:20)
    pct = sapply(step, function(x){
      return(mean(comp[1:x,1]))})
    #paste(seq(from = 5, to = 100, by=5), "%",sep=" ")
    lift = data.frame(label= seq(from = 5, to = 100, by=5), score = pct*100)
    return(list(gather_train,tree,db_test,pred_tree,pred_rf,pred_logistic,gather_indics,lift,model.rf$resample,model.rf$results,tree$cptable,good_model$anova,good_model,model.rf,pred_rf1,pred_tree1,pred_logistic1,db_train))
  }
  
  ##########################################
  
  
  
  
  output$churn_data<-renderDataTable({
    db_churn_master
  })
  #############################################
  
  observeEvent(input$apply_filter,{ 
    
    output$filter_churn_table<-renderDataTable({
      db_churn_master[,c(input$attribute_group),drop = FALSE]
    })
    ###########################calling function###########
    
    eda_plot_list<-analytic_function('cv',5)
    
    output$plot1<-renderPlot({
      gather_train<-as.data.frame(eda_plot_list[[1]])
      ggplot(gather_train , aes(value)) + facet_wrap(~variable, scales = 'free_x') +
        geom_histogram() + theme_bw()
      
    })
    
    # output$plot2<-renderPlot({
    #   tree<-eda_plot_list[[2]]
    #   rpart.plot(tree)
    #   
    # })
    # 
    # output$plot3<-renderPlot({
    #   db_test<-eda_plot_list[[3]]
    #   pred_tree<-eda_plot_list[[4]]
    #   pred_rf<-eda_plot_list[[5]]
    #   pred_logistic<-eda_plot_list[[6]]
    #   roc_tree = roc.curve(response = db_test$ChurnNum, pred_tree,
    #                        col = "#0d84da")
    #   roc_rf = roc.curve(response = db_test$ChurnNum, pred_rf,
    #                      col = "#ef0a30", add.roc=TRUE)
    #   roc_logistic = roc.curve(response = db_test$ChurnNum, pred_logistic,
    #                            col = "#45a163", add.roc=TRUE)
    #   legend("bottomright", legend=c("Decision Tree", "Random Forest",
    #                                  "Logistic Regression"),
    #          lwd = 2, col = c("#0d84da", "#ef0a30", "#45a163"))
    #   head(pred_logistic,5)
    # })
    # 
    # output$plot4<-renderPlotly({
    #   gather_indics<-eda_plot_list[[7]]
    #   q=ggplot(gather_indics , aes(x= threshold, y=value)) +
    #     ggtitle("Indicator values by thresholds")+
    #     geom_point(aes(color = color), size=0.5) +
    #     facet_wrap(~variable, scales = 'free_x') +
    #     scale_color_manual(values = c(NA, "tomato")) +
    #     labs(x="thresholds", y=" ") +
    #     geom_line(color="navy") + theme_bw()+
    #     theme( legend.position="none")
    #   ggplotly(q) %>% config(displayModeBar = F)
    # })
    # 
    # output$plot5<-renderPlotly({
    #   lift<-eda_plot_list[[8]]
    #   q<-ggplot(lift, aes(x=label, y=score))+
    #     geom_bar(stat="identity",position="stack",color="navy", fill="navy")+
    #     ggtitle("Churn rate per cumulative percentile of \n customers  with the highest probability to leave")+
    #     coord_cartesian(xlim = c(5,100), ylim = c(0,100))+
    #     scale_y_continuous(breaks = seq(from = 0, to = 100, by=25), labels = function(x) paste0(x,"%", sep = ""))+
    #     scale_x_continuous(breaks = c(5, 25, 50, 75, 100), labels = function(x) paste0(x,"%", sep = ""))+
    #     labs(x="cumulative percentile ", y="churn rate") +
    #     geom_line(aes(y=score[20]),linetype=2, size=1, color="tomato")+
    #     theme_minimal()+
    #     theme(
    #       panel.grid.major.x = element_blank(),
    #       panel.grid.minor.x = element_blank(),
    #       plot.title = element_text(size=17,face="bold", hjust=0.5),
    #       axis.text.x = element_text(vjust=1),
    #       axis.ticks.x = element_blank(),
    #       axis.title.x = element_text(size=13, face="bold", vjust=-0.4),
    #       axis.title.y = element_text(size=13,face="bold")#,
    #       #strip.text.x = element_text(face="italic", size=11)
    #     )
    #   ggplotly(q) %>% config(displayModeBar = F)
    # })
    
    
    output$MF_pie_chart <- renderPlotly({
      temp <- as.data.frame(table(db_churn_master$gender))
      plot_ly(temp, labels = ~Var1, values = ~Freq, type = 'pie') %>%
        layout(title = 'Male and Female %',
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) %>%
        config(displayModeBar = F)
    })
    
    output$senior_citizen_pie_chart <- renderPlotly({
      temp <- as.data.frame(table(db_churn_master$SeniorCitizen))
      temp$Var1 <- c("No","Yes")
      plot_ly(temp, labels = ~Var1, values = ~Freq, type = 'pie') %>%
        layout(title = 'Senior Citizen %',
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) %>%
        config(displayModeBar = F)
    })
    
    output$partner_pie_chart <- renderPlotly({
      temp <- as.data.frame(table(db_churn_master$Partner))
      plot_ly(temp, labels = ~Var1, values = ~Freq, type = 'pie') %>%
        layout(title = 'Partner %',
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) %>%
        config(displayModeBar = F)
    })
    
    output$phone_service_pie_chart <- renderPlotly({
      temp <- as.data.frame(table(db_churn_master$PhoneService))
      plot_ly(temp, labels = ~Var1, values = ~Freq, type = 'pie') %>%
        layout(title = 'Phone Service %',
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) %>%
        config(displayModeBar = F)
    })
    
    output$tenure_donut_chart <- renderPlotly({
      temp <- as.data.frame(table(db_churn_master$tenure))
      plot_ly(temp,labels = ~Var1, values = ~Freq) %>%
        add_pie(hole = 0.6) %>%
        layout(title = "Tenure Chart",  showlegend = T,
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) %>%
        config(displayModeBar = F)
    })
    
    output$contract_HR_bar_chart <- renderPlotly({
      temp <- as.data.frame(table(db_churn_master$Contract))
      temp$width <- c(0.3)
      plot_ly(x = temp$Freq, y = temp$Var1) %>%
        add_bars(width = ~temp$width) %>%
        config(displayModeBar = F)
    })
    
    output$payment_method_HR_bar_chart <- renderPlotly({
      temp <- as.data.frame(table(db_churn_master$PaymentMethod))
      temp$width <- c(0.3)
      plot_ly(x = temp$Freq, y = temp$Var1) %>%
        add_bars(width = ~temp$width) %>%
        config(displayModeBar = F)
    })
    
    output$monthly_charges_box_plot <- renderPlotly({
      plot_ly(x = db_churn_master$MonthlyCharges, type = "box",name = "Monthly Charges") %>%
        config(displayModeBar = F)
    })
    
    output$total_charges_box_plot <- renderPlotly({
      plot_ly(x = db_churn_master$TotalCharges, type = "box",name = "Total Charges",color = "green") %>%
        config(displayModeBar = F)
    })
    
    output$churn_rate_pie_chart <- renderPlotly({
      temp <- as.data.frame(table(db_churn_master$Churn))
      plot_ly(temp, labels = ~Var1, values = ~Freq, type = 'pie') %>%
        layout(title = 'Churn %',
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) %>%
        config(displayModeBar = F)
    })
    
    #############SC#################
    output$MF_pie_chart_SC <- renderPlotly({
      temp <- as.data.frame(table(db_churn_master_SC$gender))
      plot_ly(temp, labels = ~Var1, values = ~Freq, type = 'pie') %>%
        layout(title = 'Male and Female %',
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) %>%
        config(displayModeBar = F)
    })
    
    output$partner_pie_chart_SC <- renderPlotly({
      temp <- as.data.frame(table(db_churn_master_SC$Partner))
      plot_ly(temp, labels = ~Var1, values = ~Freq, type = 'pie') %>%
        layout(title = 'Partner %',
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) %>%
        config(displayModeBar = F)
    })
    
    output$phone_service_pie_chart_SC <- renderPlotly({
      temp <- as.data.frame(table(db_churn_master_SC$PhoneService))
      plot_ly(temp, labels = ~Var1, values = ~Freq, type = 'pie') %>%
        layout(title = 'Phone Service %',
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) %>%
        config(displayModeBar = F)
    })
    
    output$tenure_donut_chart_SC <- renderPlotly({
      temp <- as.data.frame(table(db_churn_master_SC$tenure))
      plot_ly(temp,labels = ~Var1, values = ~Freq) %>%
        add_pie(hole = 0.6) %>%
        layout(title = "Tenure Chart",  showlegend = T,
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) %>%
        config(displayModeBar = F)
    })
    
    output$contract_HR_bar_chart_SC <- renderPlotly({
      temp <- as.data.frame(table(db_churn_master_SC$Contract))
      temp$width <- c(0.3)
      plot_ly(x = temp$Freq, y = temp$Var1) %>%
        add_bars(width = ~temp$width) %>%
        config(displayModeBar = F)
    })
    
    output$payment_method_HR_bar_chart_SC <- renderPlotly({
      temp <- as.data.frame(table(db_churn_master_SC$PaymentMethod))
      temp$width <- c(0.3)
      plot_ly(x = temp$Freq, y = temp$Var1) %>%
        add_bars(width = ~temp$width) %>%
        config(displayModeBar = F)
    })
    
    output$monthly_charges_box_plot_SC <- renderPlotly({
      plot_ly(x = db_churn_master_SC$MonthlyCharges, type = "box",name = "Monthly Charges") %>%
        config(displayModeBar = F)
    })
    
    output$total_charges_box_plot_SC <- renderPlotly({
      plot_ly(x = db_churn_master_SC$TotalCharges, type = "box",name = "Total Charges",color = "green") %>%
        config(displayModeBar = F)
    })
    
    output$churn_rate_pie_chart_SC <- renderPlotly({
      temp <- as.data.frame(table(db_churn_master_SC$Churn))
      plot_ly(temp, labels = ~Var1, values = ~Freq, type = 'pie') %>%
        layout(title = 'Churn %',
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) %>%
        config(displayModeBar = F)
    })
    
    #############NSC###############
    output$MF_pie_chart_NSC <- renderPlotly({
      temp <- as.data.frame(table(db_churn_master_NSC$gender))
      plot_ly(temp, labels = ~Var1, values = ~Freq, type = 'pie') %>%
        layout(title = 'Male and Female %',
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) %>%
        config(displayModeBar = F)
    })
    
    output$partner_pie_chart_NSC <- renderPlotly({
      temp <- as.data.frame(table(db_churn_master_NSC$Partner))
      plot_ly(temp, labels = ~Var1, values = ~Freq, type = 'pie') %>%
        layout(title = 'Partner %',
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) %>%
        config(displayModeBar = F)
    })
    
    output$phone_service_pie_chart_NSC <- renderPlotly({
      temp <- as.data.frame(table(db_churn_master_NSC$PhoneService))
      plot_ly(temp, labels = ~Var1, values = ~Freq, type = 'pie') %>%
        layout(title = 'Phone Service %',
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) %>%
        config(displayModeBar = F)
    })
    
    output$tenure_donut_chart_NSC <- renderPlotly({
      temp <- as.data.frame(table(db_churn_master_NSC$tenure))
      plot_ly(temp,labels = ~Var1, values = ~Freq) %>%
        add_pie(hole = 0.6) %>%
        layout(title = "Tenure Chart",  showlegend = T,
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) %>%
        config(displayModeBar = F)
    })
    
    output$contract_HR_bar_chart_NSC <- renderPlotly({
      temp <- as.data.frame(table(db_churn_master_NSC$Contract))
      temp$width <- c(0.3)
      plot_ly(x = temp$Freq, y = temp$Var1) %>%
        add_bars(width = ~temp$width) %>%
        config(displayModeBar = F)
    })
    
    output$payment_method_HR_bar_chart_NSC <- renderPlotly({
      temp <- as.data.frame(table(db_churn_master_NSC$PaymentMethod))
      temp$width <- c(0.3)
      plot_ly(x = temp$Freq, y = temp$Var1) %>%
        add_bars(width = ~temp$width) %>%
        config(displayModeBar = F)
    })
    
    output$monthly_charges_box_plot_NSC <- renderPlotly({
      plot_ly(x = db_churn_master_NSC$MonthlyCharges, type = "box",name = "Monthly Charges") %>%
        config(displayModeBar = F)
    })
    
    output$total_charges_box_plot_NSC <- renderPlotly({
      plot_ly(x = db_churn_master_NSC$TotalCharges, type = "box",name = "Total Charges",color = "green") %>%
        config(displayModeBar = F)
    })
    
    output$churn_rate_pie_chart_NSC <- renderPlotly({
      temp <- as.data.frame(table(db_churn_master_NSC$Churn))
      plot_ly(temp, labels = ~Var1, values = ~Freq, type = 'pie') %>%
        layout(title = 'Churn %',
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) %>%
        config(displayModeBar = F)
    })
    
    
  })
  empty<-data.frame(warning = c("Please select some attribute"))
  observe(
    if(input$clear_filter >0){
      shinyWidgets::updatePrettyCheckboxGroup(session=session,inputId = 'attribute_group',label = 'Attributes',choices = colnames(db_churn_master),inline = TRUE,selected = NULL)
      output$filter_churn_table<-renderDataTable({
        empty
      },escape = TRUE)
    }
    
  )
  ###########round function##########
  round_df <- function(x, digits) {
    # round all numeric variables
    # x: data frame 
    # digits: number of digits to round
    numeric_columns <- sapply(x, mode) == 'numeric'
    x[numeric_columns] <-  round(x[numeric_columns], digits)
    x
  }
  
  ###################################
  
  
  observeEvent(input$apply_resample,{
    eda_plot_list2<-analytic_function(as.character(input$method_selection),as.numeric(input$number_times))
    
    output$result_resample<-renderDataTable({
      model_table<-as.data.frame(eda_plot_list2[9])
      round_df(model_table,2)
    })
    if(input$Model_Type=="Random Forest"){
      output$rf_result<-renderDataTable({
        #eda_list_4<-analytic_function(as.character(input$method_selection),as.numeric(input$number_times))
        model_result<-eda_plot_list2[[10]]
        round_df(model_result,2)
      })
      
      output$model_output<-renderPlot({
        db_test<-eda_plot_list2[[3]]
        pred_rf<-eda_plot_list2[[5]]
        roc_rf = roc.curve(response = db_test$ChurnNum, pred_rf, 
                           col = "#ef0a30", add.roc=FALSE)
        legend("bottomright", legend=c("Random Forest"), 
               lwd = 2, col = c("#ef0a30"))
        
      })
      
      ##############train output plot#########
      output$model_output_train<-renderPlot({
        db_train<-eda_plot_list2[[18]]
        pred_rf1<-eda_plot_list2[[15]]
        roc_rf = roc.curve(response = db_train$ChurnNum, pred_rf1, 
                           col = "#ef0a30", add.roc=FALSE)
        legend("bottomright", legend=c("Random Forest"), 
               lwd = 2, col = c("#ef0a30"))
        
      })
      #######save model for future use##########
      
      model.rf<-eda_plot_list2[[14]]
      saveRDS(model.rf,file =  "rf_model.rds")
      
    } else if(input$Model_Type=="Decision Tree"){
      output$rf_result<-renderDataTable({
        model_result2<-eda_plot_list2[[11]]
        round_df(model_result2,2)
      })
      output$model_output<-renderPlot({
        db_test<-eda_plot_list2[[3]]
        pred_tree<-eda_plot_list2[[4]]
        roc_tree = roc.curve(response = db_test$ChurnNum, pred_tree, 
                             col = "#0d84da",add.roc = FALSE)
        legend("bottomright", legend=c("Decision Tree"), 
               lwd = 2, col = c("#0d84da"))
      })
      
      ##############train output plot#########
      output$model_output_train<-renderPlot({
        db_train<-eda_plot_list2[[18]]
        pred_tree1<-eda_plot_list2[[16]]
        roc_rf = roc.curve(response = db_train$ChurnNum, pred_tree1, 
                           col = "#0d84da", add.roc=FALSE)
        legend("bottomright", legend=c("Random Forest"), 
               lwd = 2, col = c("#0d84da"))
        
      })
      #######save model for future use##########
      
      tree<-eda_plot_list2[[2]]
      saveRDS(tree, file = "dt_model.rds")
      
    } else{
      output$rf_result<-renderDataTable({
        model_result3<-eda_plot_list2[[12]]
        round_df(model_result3,2)
      })
      
      
      output$model_output<-renderPlot({
        db_test<-eda_plot_list2[[3]]
        pred_logistic<-eda_plot_list2[[6]]
        roc_logistic = roc.curve(response = db_test$ChurnNum, pred_logistic, 
                                 col = "#45a163", add.roc=FALSE)
        legend("bottomright", legend=c("Logistic Regression"), 
               lwd = 2, col = c("#45a163"))
      })
      ##############train output plot#########
      output$model_output_train<-renderPlot({
        db_train<-eda_plot_list2[[18]]
        pred_logistic1<-eda_plot_list2[[17]]
        roc_rf = roc.curve(response = db_train$ChurnNum, pred_logistic1, 
                           col = "#45a163", add.roc=FALSE)
        legend("bottomright", legend=c("Random Forest"), 
               lwd = 2, col = c("#45a163"))
        
      })
      
      #######save model for future use##########
      good_model<-eda_plot_list2[[13]]
      saveRDS(good_model,file = "reg_model.rds")
    }
    
    output$variable_importance<-renderPlot({
      model.rf_var<-eda_plot_list2[[14]]
      fm<-model.rf_var$finalModel
      varImpPlot(fm,main="Important variables")
    })
    
  })
  
  observeEvent(input$save_model,{
    # model_save_data<-analytic_function(as.character(input$method_selection),as.numeric(input$number_times))
    # if(input$Model_Type=="Random Forest"){
    #   good_model<-model_save_data[[13]]
    #   saveRDS(good_model,file = "reg_model.rds")
    # } else if(input$Model_Type=='Decision Tree'){
    #   tree<-model_save_data[[2]]
    #   saveRDS(tree, file = "dt_model.rds")
    # } else{
    #   model.rf<-model_save_data[[14]]
    #   saveRDS(model.rf,file =  "rf_model.rds")
    # }
    sendSweetAlert(
      session = session,
      title = "Success !!",
      text = "Model saved successfully",
      type = "success"
    )
    
  })
  observeEvent(input$Combining_model,{
    ##########
    ##combining prediction of multiple models#####
    trainId = createDataPartition(db_churn2$Churn, 
                                  p=0.7, list=FALSE,times=1)
    db_train = db_churn2[trainId,]
    db_test = db_churn2[-trainId,]
    
    normalize1 = function(x) {
      result = (x - min(x, na.rm = TRUE)
      ) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
      return(result)
    }
    drops <- c("X1","customerID","InternetService","Contract","PaymentMethod")
    
    db_train<-db_train[ , !(names(db_train) %in% drops)]
    db_train$tenure<-normalize1(db_train$tenure)
    db_train$MonthlyCharges<-normalize1(db_train$MonthlyCharges)
    db_train$TotalCharges<-normalize1(db_train$TotalCharges)
    
    db_train$gender<-ifelse(db_train$gender=='Male','1',0)
    db_train$Partner<-ifelse(db_train$Partner=='Yes','1',0)
    db_train$Dependents<-ifelse(db_train$Dependents=='Yes','1',0)
    db_train$PhoneService<-ifelse(db_train$PhoneService=='Yes','1',0)
    db_train$MultipleLines<-ifelse(db_train$MultipleLines=='Yes','1',0)
    db_train$OnlineSecurity<-ifelse(db_train$OnlineSecurity=='Yes','1',0)
    db_train$OnlineBackup<-ifelse(db_train$OnlineBackup=='Yes','1',0)
    db_train$DeviceProtection<-ifelse(db_train$DeviceProtection=='Yes','1',0)
    db_train$TechSupport<-ifelse(db_train$TechSupport=='Yes','1',0)
    db_train$StreamingTV<-ifelse(db_train$StreamingTV=='Yes','1',0)
    db_train$StreamingMovies<-ifelse(db_train$StreamingMovies=='Yes','1',0)
    db_train$PaperlessBilling<-ifelse(db_train$PaperlessBilling=='Yes','1',0)
    
    row.has.na <- apply(db_train, 1, function(x){any(is.na(x))})
    db_train <- db_train[!row.has.na, ]
    ################train control############
    trainControl <- trainControl(method="repeatedcv", 
                                 number=10, 
                                 repeats=3,
                                 savePredictions=TRUE, 
                                 classProbs=TRUE)
    
    algorithmList <- c(input$model_select_checkbox)
    models <- caretList(Churn ~ ., data=db_train, trControl=trainControl, methodList=algorithmList) 
    results <- resamples(models)
    #summary(results)
    ##########combining prediction##############
    stackControl <- trainControl(method="repeatedcv", 
                                 number=10, 
                                 repeats=3,
                                 savePredictions=TRUE, 
                                 classProbs=TRUE)
    
    # Ensemble the predictions of `models` to form a new combined prediction based on glm
    stack.glm <- caretStack(models, method="glm", metric="Accuracy", trControl=stackControl)
    
    ####################saving the model output##########
    saveRDS(results,file = "ensemble.rds")
    saveRDS(stack.glm,file = 'stackensemle.rds')
    
    
    ##########################
    output$merge_plot<-renderPlot({
      scales <- list(x=list(relation="free"), y=list(relation="free"))
      bwplot(results, scales=scales)
      
    })
    
    output$combined_prediction<-renderDataTable({
      stack.glm$error
    })
    
    
    output$model_result_table<-renderDataTable({
      round_df(stack.glm$models$rf$results,3)
    })
    
    output$model_result_table2<-renderDataTable({
      stack.glm$models$regLogistic$results
    })
    
    
    
    
  })
  observeEvent(input$submit_churn,{
    sendSweetAlert(
      session = session,
      title = "Success !!",
      text = "csv submitted successfully",
      type = "success"
    )
  })
  #################################################
  
  
  #################detect churn####################
  final_prediction<-eventReactive(input$detect_churn,{
    
    
    shinyjs::disable("detect_churn")
    Sys.sleep(3)
    shinyjs::enable("detect_churn")
    ##########Prince normalization#############
    normalize_tenure = function(x) {
      result = (x - 0
      ) / (72 - 0)
      return(result)
    }
    
    normalize_MonthlyCharges = function(x) {
      result = (x - 18.25
      ) / (118.75 - 18.25)
      return(result)
    }
    normalize_TotalCharges = function(x){
      result = (x - 18.8
      ) / (8684.8 - 18.8)
      return(result)
    }
    ###########################################
    test4<-readRDS(file = 'stackensemle.rds')
    tenure<-normalize_tenure(as.numeric(input$tenure))
    MonthlyCharges<-normalize_MonthlyCharges(as.numeric(input$MonthlyCharges))
    TotalCharges<-normalize_TotalCharges(as.numeric(input$TotalCharges))
    
    
    SeniorCitizen<-ifelse(as.character(input$seniorCitizen)=='Yes',1,0)
    gender<-ifelse(as.character(input$gender)=='Male',1,0)
    gender<-as.factor(gender)
    Partner<-ifelse(as.character(input$partner)=='Yes',1,0)
    Partner<-as.factor(Partner)
    Dependents<-ifelse(as.character(input$dependents)=='Yes',1,0)
    Dependents<-as.factor(Dependents)
    PhoneService<-ifelse(as.character(input$phoneService)=='Yes',1,0)
    PhoneService<-as.factor(PhoneService)
    MultipleLines<-ifelse(as.character(input$MultipleLines)=='Yes',1,0)
    MultipleLines<-as.factor(MultipleLines)
    OnlineSecurity<-ifelse(as.character(input$OnlineSecurity)=='Yes',1,0)
    OnlineSecurity<-as.factor(OnlineSecurity)
    OnlineBackup<-ifelse(as.character(input$OnlineBackup)=='Yes',1,0)
    OnlineBackup<-as.factor(OnlineBackup)
    DeviceProtection<-ifelse(as.character(input$DeviceProtection)=='Yes',1,0)
    DeviceProtection<-as.factor(DeviceProtection)
    TechSupport<-ifelse(as.character(input$TechSupport)=='Yes',1,0)
    TechSupport<-as.factor(TechSupport)
    StreamingTV<-ifelse(as.character(input$StreamingTV)=='Yes',1,0)
    StreamingTV<-as.factor(StreamingTV)
    StreamingMovies<-ifelse(as.character(input$StreamingMovies)=='Yes',1,0)
    StreamingMovies<-as.factor(StreamingMovies)
    PaperlessBilling<-ifelse(as.character(input$PaperlessBilling)=='Yes',1,0)
    PaperlessBilling<-as.factor(PaperlessBilling)
    
    final_data_frame<-data.frame('gender' = gender,'SeniorCitizen' = SeniorCitizen,'Partner' = Partner,'Dependents' = Dependents,'tenure' = tenure, 'PhoneService' = PhoneService,
                                 'MultipleLines' =MultipleLines,'OnlineSecurity' = OnlineSecurity,'OnlineBackup'=OnlineBackup,'DeviceProtection'=DeviceProtection,'TechSupport'=TechSupport,
                                 'StreamingTV' =StreamingTV, StreamingMovies =StreamingMovies,'PaperlessBilling'=PaperlessBilling ,'MonthlyCharges' =MonthlyCharges, 'TotalCharges' = TotalCharges,'Churn' = 'Yes')
    
    
    predict_final <- predict(test4, newdata=final_data_frame)
    predict_final<-as.numeric(predict_final)
    predict_final<-ifelse(predict_final==1,'Yes','No')
    
    # output$final_prediction<-renderText({
    #   predict_final
    # })
    predict_final
    
    
  })
  
  output$final_prediction<-renderText({
    final_prediction()
  })
  
  observeEvent(input$submit_group_churn,{
    
    if(input$age_category=='All'){
      updateTextInput(session,'custinsegment',value = 7043)
      updateTextInput(session,'custlikelytchurn',value = '1869 (26.5%)')
      updateTextInput(session,'cltv',value = '18.7')
      updateTextInput(session,'cltv2',value = 0)
    }
    else if(input$age_category=='Senior Citizen' & input$monthly_charges==''){
    updateTextInput(session,'custinsegment',value = 1142)
    updateTextInput(session,'custlikelytchurn',value = '476 (41.7%)')
    updateTextInput(session,'cltv',value = 6.76)
    updateTextInput(session,'cltv2',value = 0)
    }else if(input$age_category=='Senior Citizen' & input$monthly_charges==-5){
      updateTextInput(session,'custinsegment',value = 1142)
      updateTextInput(session,'custlikelytchurn',value = '395 (34.6%)')
      updateTextInput(session,'cltv',value = 4.05)
      updateTextInput(session,'cltv2',value = 2.71)
    }else{
      updateTextInput(session,'custinsegment',value = 1142)
      updateTextInput(session,'custlikelytchurn',value = '267 (23.38%)')
      updateTextInput(session,'cltv',value = 2.7)
      updateTextInput(session,'cltv2',value = 4.06)
    }
    
    
    # output$sensitivity_plot<-renderPlot({
    #   eda_plot_list3<-analytic_function('cv',4)
    #   model.rf_var_imp<-eda_plot_list3[[14]]
    #   fm1<-model.rf_var_imp$finalModel
    #   varImpPlot(fm1,main="Important variables")
    #   
    # })
    
    output$probofchurn<-renderText({
      paste0('Prob of churn : 0.65')
    })
  })
  
  output$end_list<-renderDataTable({
    db_churn_master
  })
  
  ############hypothesis result master##############
  react <- reactiveValues()
  react$hypothesis_DF <- hypothesis_DF
  react$editing <- T
  
  observeEvent(input$edit_rhandsontable,{
    react$editing <- F
  })
  
  observeEvent(input$new_rhandsontable,{
    react$hypothesis_DF <- rbind(react$hypothesis_DF,data.frame(Crisp.ID = c(nrow(react$hypothesis_DF)+1),UID = c(""),Description = c(""),Status = c(""),Created.By=c(""),Created.On = c(Sys.time())))
  })
  
  output$hypothesis_table <- renderRHandsontable({
    rhandsontable(react$hypothesis_DF,rowHeaders = F,readOnly = react$editing,selectCallback = T) %>%
      hot_cols(manualColumnResize = T) %>%
      hot_col(col = "Crisp.ID",format = 0)
  })
  
  ##################################################
  output$model_result_output<-renderRHandsontable({
    rhandsontable(femb_transaction,selectCallback = TRUE,readOnly = FALSE)
  })
  
  observeEvent(input$model_result_output_select,{
    x<-input$model_result_output_select$data[[
      input$model_result_output_select$select$r]][[input$model_result_output_select$select$c]]

    if(x=='Random Forest'){
      output$rf_output<-renderDataTable({
        rf_output_data<-readRDS(file = 'rf_model.rds')
        rf_output_data$results
      })
    } else if(x=='Decision Tree'){
      output$rf_output<-renderDataTable({
        rf_output_data<-readRDS(file = 'dt_model.rds')
        rf_output_data$cptable
      })
    } else{
      output$rf_output<-renderDataTable({
        rf_output_data<-readRDS(file = 'reg_model.rds')
        rf_output_data$anova
        
      })
    }
    
  })
  
}


shinyApp(ui, server)

