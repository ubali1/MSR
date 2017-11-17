library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)

## ui.R file for MSR ShinyApp

shinyUI(pageWithSidebar(
  headerPanel("Control Charts and MSRs"),
  sidebarPanel(
    tabsetPanel(
      tabPanel("Enter CSV file", 
        fileInput("datafile",
                  "Choose CSV files from directory",
                  multiple = TRUE,
                  accept=c('text/csv', 
                           'text/comma-separated-values,text/plain', 
                           '.csv'))
      ), # end of tabPanel 1
  
      tabPanel("Compound Input",
        textInput('compound_id', "Enter compound ID for a single compound","")
        #actionButton("submit","Apply")
      ) # end of tabPanel 2
    ) # end of tabsetPanel
  ), # end of sidebarPanel
  
  mainPanel(
    tabsetPanel(
      tabPanel("Introduction",
      br(), br(),
      h1('Introduction'),
        p('This application is designed to display control charts and repeated measures and control compound MSRs calculated from a user input data table.'),          
        p('The application is seperated into intuitively labelled tabs. Users can input a data as a csv file and a selected compound as a reference compound.'),
        p('The tabs on main page then display the respective tables and graphs along with calculated MSR values.'),
      h2('Instructions'),
        p('In order for the application to work properly, it is important for the user to follow these guidelines:'),
        p('The input data should be entered as a comma separated file (*.csv) and can include any number of rows and columns, however, it must include the following column names: \'COMPOUND_ID\', \'REFERENCE\' and \'IC50\'.'),
        p('The Repeated measures MSR and the control compound MSRs are calculated on the values entered in the IC50 column.'),
        p('The application does not substitute missing values from a data set and these are usually omitted from the analysis.'),
        p('In the Compound Input tab on the sidebar, the user should include a value for a single compound ID defined in the COMPOUND_ID column. There should be an exact match as the app is not designed for pattern matching or for displaying information for more than one compound selected.'),
      h2('MSR Calculations'),
        p('Potency measurements from concentration-response assays are usually log-normally distributed and as such, log10AC50 is preferred for statistical analysis and modeling, including estimation of the MSR.'), 
        p('The MSR is defined as the smallest ratio between the potencies of two compounds that is statistically significant and is calculated as MSR = 10^2sqrt2s , where s is an estimate of the standard deviation of a log potency for one compound. The variability estimate s can be estimated in different ways depending on available data and associated analysis method e.g., within-run, between-run, and other sources of variability'), 
        p('The common ways to estimate assay variability are detailed below.'),
      h4('Replicate-Experiment MSR'),      
        p('Replicate-Experiment MSR is a diagnostic and decision tool used to establish that an assay is ready to go into production. It is estimated from two independent runs of 20-30 compounds and is calculated as MSR = 102Sd.'), 
        p('where sd is the standard deviation of the paired differences in log potency across the two runs.'),
        p('Two runs are not considered adequate to estimate between-run variability so the Replicate-Experiment MSR focuses only on within-run variability.'), 
        p('Use of the standard deviation of the paired differences factors out the between-run variability.'),
      h4('Control Compound MSR'),
        p('A minimum of six runs is considered adequate to estimate between-run variability, and the Control Compound MSR can be calculated from six or more runs as MSR = 10^2sqrt2s'),
        p('where s is the standard deviation of the log10AC50 values across runs, assuming one AC50 result per run.'),
      
      h2('Next version'),
        p('Plans are in place to impove upon this application by adding the following updates:'),
        p('1. Selction of data points either via the table or the graphical display for subsequent MSR analysis.'),
        p('2. Wildcard search capability for specific compounds'),
        p('3. Option to perform MSR calculations for variables in multiple columns defined by the user'),
        p('This is a work in progress so if you have further commments, please do not hesitate to get in touch')
      ), # end of tabPanel 1      
      
      tabPanel("Scatter plot of input data",
        dataTableOutput("data_table"),
        plotlyOutput("data_plot")
      ), # end of tabPanel 2
      
      tabPanel("Frequency of Runs",
        dataTableOutput("freq_table"),
        plotlyOutput("frequency_plot")
      ),# end of tabPanel 3
      
      tabPanel("Potency Log Difference and Replicate Experiment MSR", align = "center",
        plotlyOutput('log_diff_plot'),              
        br(), br(),
        strong('The mean log difference in potencies is:'), 
        strong(textOutput('mean_log_diff')),
        br(),
        strong('The sd of log difference in potencies is:'),
        strong(textOutput('sd_log_diff')),
        br(),
        strong('The mean ratio is:'),
        strong(textOutput('MR')),
        br(),
        strong('The upper Limit of Agreement (LsA) is:'),
        strong(textOutput('Upper_LsA')),
        br(),
        strong('The lower Limit of Agreement (LsA) is:'),
        strong(textOutput('Lower_LsA')),
        br(),
        strong('The Replicate Experiment 95% confidence MSR is:'),
        strong(h4(textOutput('MSR_95'))),  
        br(), br()
      ),# end of tabPanel 4
  
      tabPanel("Control Compound MSR",
        p('Data shall be collated for the following compound:'),
        textOutput('value'),
        p('Table listing the cumulative mean and standard error of input compound'),
        dataTableOutput("compound_table"),
        plotlyOutput('error_plot')
        )#end of tabPanel 5
      )#end of tabsetPanel
    )#end of mainPanel
  )#end of pageWithSidebar
)#end of ShinyUI
