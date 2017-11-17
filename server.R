library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)

shinyServer(function(input, output) {
  
  output$data_table <- renderDataTable({
    
    inFile <- input$datafile
    if (is.null(inFile)) {
      return(NULL)
    } else {
      inFile %>%
        rowwise() %>%
        do({
          read.csv(.$datapath, sep = ",", na.strings = c("", "NA"), head = TRUE)
        })
    }
  })# end of renderDataTable fct 
  
  
## A new reactive expression for loading user input datafile  
  
    filedata <- reactive({
      user <- input$datafile
      if (is.null(user)) {
        # User has not uploaded a file yet
        return(NULL)
      } # end of if statement
      read.csv(user$datapath, sep = ",", na.strings = c("", "NA"), head = TRUE)
    })# end of reactive expression for loading user input datafile


## reactive expression for displaying a plot of all data from the loaded input file
    
  output$data_plot <- renderPlotly({
    df <-filedata()
    if (is.null(df)) return(NULL)
    
    data <- mutate(df, pIC50 = -log10(as.numeric(IC50)/1000000000))
    
    # let's identify and remove rows that contain either the ">" or the "<" symbol
    
    #if (is.null(grep("<|>", data$IC50))){
      #row_id <- grep("<|>", data$IC50)
      #data <- data[-(row_id),]
    
    # Let's plot the data table to list all compounds and the associated IC50s
    f <- ggplot(data, aes(x = COMPOUND_ID, y = as.numeric(IC50))) + geom_point(size = 3, alpha = 0.2, color = "blue")    
    f <- f + xlab ("Compound ID") + ylab ("Measured IC50") + ylim(min(as.numeric(data$IC50))/10, max(as.numeric(data$IC50))+50)
    f <- f + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    f <- f + ggtitle ("Scatterplot of test compound IC50s")
    ggplotly(f) %>% layout(margin=list(l = 100), yaxis=list(tickprefix=" "))
    
  })#end of renderPlot expression for displaying data_plot

    
#The following set of functions display the frequency table and the frequency plot
    output$freq_table <- renderDataTable({
      df <-filedata()
      if (is.null(df)) return(NULL)
      
      data <- mutate(df, pIC50 = -log10(as.numeric(IC50)/1000000000))
      
      
      # let's identify and remove rows that contain either the ">" or the "<" symbol
      #row_id <- grep("<|>", data$IC50)
      #data <- data[-(row_id),]
      
      ## a data frame that outputs a table with frequency of each entry
      frequency_table <- data.frame(table(data$COMPOUND_ID))
      
      return(frequency_table)
      #items=names(df)
      #names(items)=items
      #selectInput("to", "To:",items)
      
    })#end of renderDataTable expression for displaying frequency table and frequency plot

## Reactive expression for displaying frequency_plot
    
  output$frequency_plot <- renderPlotly({
    df <-filedata()
    if (is.null(df)) return(NULL)
    
    data <- mutate(df, pIC50 = -log10(as.numeric(IC50)/1000000000))
    
    # let's identify and remove rows that contain either the ">" or the "<" symbol
    #row_id <- grep("<|>", data$IC50)
    #data <- data[-(row_id),]
    
    ## a data frame that outputs a table with frequency of each entry
    frequency_table <- data.frame(table(data$COMPOUND_ID))
    
    g <- ggplot(frequency_table, aes(x = Var1, y = Freq)) + geom_point(position = position_jitter(w = 0.3, h = 0), size = frequency_table$Freq, alpha = 0.2, color = frequency_table$Freq)
    g <- g + xlab ("Compound ID") + ylab ("Frequency of tests") + ylim(1, max(frequency_table$Freq)) 
    g <- g + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    g <- g + ggtitle ("Scatterplot of compound test frequencies (jitter plot)")
    ggplotly(g)
    
  })#end of renderPlot expression for displaying frequency_plot
  

## The following set of functions display the log difference plot and associated MSR values

  output$log_diff_plot <- renderPlotly({
    df <-filedata()
    if (is.null(df)) return(NULL)
    
    data <- mutate(df, pIC50 = -log10(as.numeric(IC50)/1000000000))
    
    # let's identify and remove rows that contain either the ">" or the "<" symbol
    #row_id <- grep("<|>", data$IC50)
    #data <- data[-(row_id),]
    
    ## a data frame that outputs a table with frequency of each entry
    frequency_table <- data.frame(table(data$COMPOUND_ID))
    
    ## let's capture all entries with duplicate measurements
    duplicate_entries <- frequency_table[frequency_table$Freq == 2,]
    
    ## let's extract the names of all duplicate entries
    duplicate_cmps <- as.character(duplicate_entries[,1])
    
    ## let's find if our vector containing duplicate cmp names is present in the original data frame 'data'
    ## and then let's extract the entire row that matches the name
    duplicate_data <- data[is.element(data$COMPOUND_ID, c(duplicate_cmps)),]
    
    # Let's group the data by compound name
    grouped_data <- group_by(duplicate_data, COMPOUND_ID)
    
    # Let's summarize the grouped data by subtracting the first value from the second
    #summary <- summarize(grouped_data, log_diff = with(grouped_data, pIC50[-1] - pIC50[-length(pIC50)]))
    summary <- summarize(grouped_data, log_diff = last(pIC50) - first(pIC50))
    
    k <- ggplot(summary, aes(x = COMPOUND_ID, y = log_diff)) + geom_point(position = position_jitter(w = 0.3, h = 0), size = 3, alpha = 0.2, color = "blue")
    k <- k + xlab ("Compound ID") + ylab ("log difference in potencies") + ylim(-1, 1) 
    k<- k + geom_hline(data = summary, aes(yintercept = mean(summary$log_diff), linetype = "Mean"), show.legend = FALSE, size = 0.5) 
    k <- k + geom_hline(data = summary, aes(yintercept = sd(summary$log_diff), color = "Red", linetype = "sd"), show.legend = FALSE, size = 0.2)
    k <- k + geom_hline(data = summary, aes(yintercept = -sd(summary$log_diff), color = "Red", linetype = "sd"), show.legend = FALSE, size = 0.2)
    k <- k + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    k <- k + ggtitle ("Scatterplot of compound vs log difference in potencies of repeat measures (jitter plot)")
    ggplotly(k)
    
  })#end of renderPlot expression for displaying log-difference plot


## The following set of functions display the mean_log_diff values

output$mean_log_diff <- renderText({
  df <-filedata()
  if (is.null(df)) return(NULL)
  
  data <- mutate(df, pIC50 = -log10(as.numeric(IC50)/1000000000))
  
  # let's identify and remove rows that contain either the ">" or the "<" symbol
  #row_id <- grep("<|>", data$IC50)
  #data <- data[-(row_id),]
  
  frequency_table <- data.frame(table(data$COMPOUND_ID))
  duplicate_entries <- frequency_table[frequency_table$Freq == 2,]
  duplicate_cmps <- as.character(duplicate_entries[,1])
  duplicate_data <- data[is.element(data$COMPOUND_ID, c(duplicate_cmps)),]
  grouped_data <- group_by(duplicate_data, COMPOUND_ID)
  #summary <- summarize(grouped_data, log_diff = with(grouped_data, pIC50[-1] - pIC50[-length(pIC50)]))
  summary <- summarize(grouped_data, log_diff = last(pIC50) - first(pIC50))
  
  #calculations of repeated measure MSRs
  mean_log_diff <- round(mean(summary$log_diff),2)
  
  return(mean_log_diff)
  
})#end of renderText expression for displaying mean log-difference

## The following set of functions display the sd_log_diff values

output$sd_log_diff <- renderText({
  df <-filedata()
  if (is.null(df)) return(NULL)
  
  data <- mutate(df, pIC50 = -log10(as.numeric(IC50)/1000000000))
  
  # let's identify and remove rows that contain either the ">" or the "<" symbol
  #row_id <- grep("<|>", data$IC50)
  #data <- data[-(row_id),]
  
  frequency_table <- data.frame(table(data$COMPOUND_ID))
  duplicate_entries <- frequency_table[frequency_table$Freq == 2,]
  duplicate_cmps <- as.character(duplicate_entries[,1])
  duplicate_data <- data[is.element(data$COMPOUND_ID, c(duplicate_cmps)),]
  grouped_data <- group_by(duplicate_data, COMPOUND_ID)
  #summary <- summarize(grouped_data, log_diff = with(grouped_data, pIC50[-1] - pIC50[-length(pIC50)]))
  summary <- summarize(grouped_data, log_diff = last(pIC50) - first(pIC50))
  
  #calculations of repeated measure MSRs
  mean_log_diff <- round(mean(summary$log_diff),2)
  sd_log_diff <- round(sd(summary$log_diff),2)
  MR = round(10^(mean_log_diff),2)
  n = round(length(summary$log_diff),2)
  Upper_LsA = round(10^(mean_log_diff+(2*(sd_log_diff))),2)
  Lower_LsA = round(10^(mean_log_diff-(2*(sd_log_diff))),2)
  MSR_95 = round(10^(2*sd_log_diff),2)
  #values <- list(mean_log_diff = mean_log_diff, sd_log_diff = sd_log_diff, MR = MR, Upper_LsA = Upper_LsA, Lower_LsA = Lower_LsA, MSR_95 = MSR95)
  return(sd_log_diff)
  
})#end of renderText expression for displaying sd log-difference plot


## The following set of functions display the mean ratio values

output$MR <- renderText({
  df <-filedata()
  if (is.null(df)) return(NULL)
  
  data <- mutate(df, pIC50 = -log10(as.numeric(IC50)/1000000000))
  
  # let's identify and remove rows that contain either the ">" or the "<" symbol
  #row_id <- grep("<|>", data$IC50)
  #data <- data[-(row_id),]
  
  frequency_table <- data.frame(table(data$COMPOUND_ID))
  duplicate_entries <- frequency_table[frequency_table$Freq == 2,]
  duplicate_cmps <- as.character(duplicate_entries[,1])
  duplicate_data <- data[is.element(data$COMPOUND_ID, c(duplicate_cmps)),]
  grouped_data <- group_by(duplicate_data, COMPOUND_ID)
  #summary <- summarize(grouped_data, log_diff = with(grouped_data, pIC50[-1] - pIC50[-length(pIC50)]))
  summary <- summarize(grouped_data, log_diff = last(pIC50) - first(pIC50))
  
  #calculations of repeated measure MSRs
  mean_log_diff <- round(mean(summary$log_diff),2)
  sd_log_diff <- round(sd(summary$log_diff),2)
  MR = round(10^(mean_log_diff),2)
  n = round(length(summary$log_diff),2)
  Upper_LsA = round(10^(mean_log_diff+(2*(sd_log_diff))),2)
  Lower_LsA = round(10^(mean_log_diff-(2*(sd_log_diff))),2)
  MSR_95 = round(10^(2*sd_log_diff),2)
  return(MR)
  
})#end of renderText expression for displaying MR

## The following set of functions display the UpperLsA values

output$Upper_LsA <- renderText({
  df <-filedata()
  if (is.null(df)) return(NULL)
  
  data <- mutate(df, pIC50 = -log10(as.numeric(IC50)/1000000000))
  
  # let's identify and remove rows that contain either the ">" or the "<" symbol
  #row_id <- grep("<|>", data$IC50)
  #data <- data[-(row_id),]
  
  frequency_table <- data.frame(table(data$COMPOUND_ID))
  duplicate_entries <- frequency_table[frequency_table$Freq == 2,]
  duplicate_cmps <- as.character(duplicate_entries[,1])
  duplicate_data <- data[is.element(data$COMPOUND_ID, c(duplicate_cmps)),]
  grouped_data <- group_by(duplicate_data, COMPOUND_ID)
  #summary <- summarize(grouped_data, log_diff = with(grouped_data, pIC50[-1] - pIC50[-length(pIC50)]))
  summary <- summarize(grouped_data, log_diff = last(pIC50) - first(pIC50))
  
  #calculations of repeated measure MSRs
  mean_log_diff <- round(mean(summary$log_diff),2)
  sd_log_diff <- round(sd(summary$log_diff),2)
  MR = round(10^(mean_log_diff),2)
  n = round(length(summary$log_diff),2)
  Upper_LsA = round(10^(mean_log_diff+(2*(sd_log_diff))),2)
  Lower_LsA = round(10^(mean_log_diff-(2*(sd_log_diff))),2)
  MSR_95 = round(10^(2*sd_log_diff),2)
  return(Upper_LsA)
  
})#end of renderText expression for displaying Upper_LsA values

## The following set of functions display the Lower_LsA values

output$Lower_LsA <- renderText({
  df <-filedata()
  if (is.null(df)) return(NULL)
  
  data <- mutate(df, pIC50 = -log10(as.numeric(IC50)/1000000000))
  
  # let's identify and remove rows that contain either the ">" or the "<" symbol
  #row_id <- grep("<|>", data$IC50)
  #data <- data[-(row_id),]
  
  frequency_table <- data.frame(table(data$COMPOUND_ID))
  duplicate_entries <- frequency_table[frequency_table$Freq == 2,]
  duplicate_cmps <- as.character(duplicate_entries[,1])
  duplicate_data <- data[is.element(data$COMPOUND_ID, c(duplicate_cmps)),]
  grouped_data <- group_by(duplicate_data, COMPOUND_ID)
  #summary <- summarize(grouped_data, log_diff = with(grouped_data, pIC50[-1] - pIC50[-length(pIC50)]))
  summary <- summarize(grouped_data, log_diff = last(pIC50) - first(pIC50))
  
  #calculations of repeated measure MSRs
  mean_log_diff <- round(mean(summary$log_diff),2)
  sd_log_diff <- round(sd(summary$log_diff),2)
  MR = round(10^(mean_log_diff),2)
  n = round(length(summary$log_diff),2)
  Upper_LsA = round(10^(mean_log_diff+(2*(sd_log_diff))),2)
  Lower_LsA = round(10^(mean_log_diff-(2*(sd_log_diff))),2)
  MSR_95 = round(10^(2*sd_log_diff),2)
  return(Lower_LsA)
  
})#end of renderText expression for displaying Lower_LsA values

## The following set of functions display the MSR_95 values

output$MSR_95 <- renderText({
  df <-filedata()
  if (is.null(df)) return(NULL)
  
  data <- mutate(df, pIC50 = -log10(as.numeric(IC50)/1000000000))
  
  # let's identify and remove rows that contain either the ">" or the "<" symbol
  #row_id <- grep("<|>", data$IC50)
  #data <- data[-(row_id),]
  
  frequency_table <- data.frame(table(data$COMPOUND_ID))
  duplicate_entries <- frequency_table[frequency_table$Freq == 2,]
  duplicate_cmps <- as.character(duplicate_entries[,1])
  duplicate_data <- data[is.element(data$COMPOUND_ID, c(duplicate_cmps)),]
  grouped_data <- group_by(duplicate_data, COMPOUND_ID)
  #summary <- summarize(grouped_data, log_diff = with(grouped_data, pIC50[-1] - pIC50[-length(pIC50)]))
  summary <- summarize(grouped_data, log_diff = last(pIC50) - first(pIC50))
  
  #calculations of repeated measure MSRs
  mean_log_diff <- round(mean(summary$log_diff),2)
  sd_log_diff <- round(sd(summary$log_diff),2)
  MR = round(10^(mean_log_diff),2)
  n = round(length(summary$log_diff),2)
  Upper_LsA = round(10^(mean_log_diff+(2*(sd_log_diff))),2)
  Lower_LsA = round(10^(mean_log_diff-(2*(sd_log_diff))),2)
  MSR_95 = round(10^(2*sd_log_diff),2)
  
  #MSRList <- list("mean_log_diff" = mean_log_diff, "sd_log_diff" = sd_log_diff, "MR" = MR, "n" = n, "Upper_LsA" = Upper_LsA, "Lower_LsA" = Lower_LsA, "MSR_95" = MSR_95)
  
  return(MSR_95)
  
})#end of renderPlot expression for displaying MSR_95

## reactive expression returns user input value for compound ID  
output$value <- reactive({
  #input$submit
  name <- input$compound_id
  if (is.null(name)){
    return(NULL)}
  else {return(name)}
})

## RenderDataTable expression return all entries for user selected compound from the data frame 
output$compound_table <- renderDataTable({
  
  df <-filedata()
  if (is.null(df)) return(NULL)
  
  data <- mutate(df, pIC50 = -log10(as.numeric(IC50)/1000000000))
  
  # let's identify and remove rows that contain either the ">" or the "<" symbol
  #row_id <- grep("<|>", data$IC50)
  #data <- data[-(row_id),]
  
  compound_id <- input$compound_id
  if (is.null(compound_id)) return(NULL)
  
  ## let's find if the data set contains the input compound name 
  compound_data <- data[is.element(data$COMPOUND_ID, compound_id),]
  compound_data$REFERENCE <- as.factor(compound_data$REFERENCE)
  compound_data$pIC50 <- as.numeric(compound_data$pIC50)
  
  
  ## Because we wish to plot test ocassion vs pIC50 values for the selected compound, we are going to use
  ## grouped data frame, grouped by test occassion as there are multiple runs of the compound in question
  # Let's group the data by REFERENCE
  
  # Let's group the data by compound name
  compound_data <- group_by(compound_data, REFERENCE)
  compound_data <- summarize(compound_data, mean_pIC50 = mean(pIC50))
  
  ## function for calculating length
  replicates <- function(x) sapply(sapply(seq_along(x), head, x=x), length)
  compound_data$length <- ave(compound_data$mean_pIC50, FUN=replicates)
  
  
  cummean <- function(x) sapply(sapply(seq_along(x), head, x=x), mean)
  compound_data$cum_mean <- ave(compound_data$mean_pIC50, FUN=cummean)
  
  ## Let's calculate the cumulative standard deviation of the pIC50 value
  ## The function loops along the pIC50 column and calculates the sd between head and the x value
  ## sapply(seq_along(compound_data$pIC50), head, x = compound_data$pIC50) - basically creates a list of elements with increasing number of pIC50 values
  ## the second sapply applies the sd function to this list
  cumsd <- function(x) sapply(sapply(seq_along(x), head, x=x), sd)
  compound_data$cum_sd <- ave(compound_data$mean_pIC50, FUN=cumsd)
  
  compound_data <- mutate(compound_data, t_critical = abs(qt(0.05, (compound_data$length - 1))))
  compound_data <- mutate(compound_data, SEM = compound_data$cum_sd/sqrt(compound_data$length))
  compound_data <- mutate(compound_data, error = t_critical * SEM)
  
  return(compound_data)  
})#end of renderDataTable expression for displaying compound data


## The following set of functions display the error plot for user selected compound values

output$error_plot <- renderPlotly({
  df <-filedata()
  if (is.null(df)) return(NULL)
  
  data <- mutate(df, pIC50 = -log10(as.numeric(IC50)/1000000000))
  
  
  # let's identify and remove rows that contain either the ">" or the "<" symbol
  #row_id <- grep("<|>", data$IC50)
  #data <- data[-(row_id),]
  
  compound_id <- input$compound_id
  if (is.null(compound_id)) return(NULL)
  
  ## let's find if the data set contains the input compound name 
  compound_data <- data[is.element(data$COMPOUND_ID, compound_id),]
  compound_data$REFERENCE <- as.factor(compound_data$REFERENCE)
  compound_data$pIC50 <- as.numeric(compound_data$pIC50)
  
  
  ## Because we wisht to plot test ocassion vs pIC50 values for the selected compound, we are going to use
  ## grouped data frame, grouped by test occassion as there are multiple runs of the compound in question
  # Let's group the data by REFERENCE
  
  # Let's group the data by compound name
  compound_data <- group_by(compound_data, REFERENCE)
  compound_data <- summarize(compound_data, mean_pIC50 = mean(pIC50))
  
  
  ## Let's calculate the cumulative standard deviation of the pIC50 value
  ## The function loops along the pIC50 column and calculates the sd between head and the x value
  ## sapply(seq_along(compound_data$pIC50), head, x = compound_data$pIC50) - basically creates a list of elements with increasing number of pIC50 values
  ## the second sapply applies the sd function to this list
  cumsd <- function(x) sapply(sapply(seq_along(x), head, x=x), sd)
  compound_data$cum_sd <- ave(compound_data$mean_pIC50, FUN=cumsd)
  
  ## similar function to the one above but calculating length instead of sd
  replicates <- function(x) sapply(sapply(seq_along(x), head, x=x), length)
  compound_data$length <- ave(compound_data$mean_pIC50, FUN=replicates)
  
  cummean <- function(x) sapply(sapply(seq_along(x), head, x=x), mean)
  compound_data$cum_mean <- ave(compound_data$mean_pIC50, FUN=cummean)
  
  compound_data <- mutate(compound_data, t_critical = abs(qt(0.05, (compound_data$length - 1))))
  compound_data <- mutate(compound_data, SEM = compound_data$cum_sd/sqrt(compound_data$length))
  compound_data <- mutate(compound_data, error = t_critical * SEM)
  
  l <- ggplot(compound_data, aes(x = REFERENCE, y = cum_mean)) + geom_point(size = 3, alpha = 0.2, color = "blue")
  l <- l + xlab ("REFERENCE") + ylab ("cumulative mean pIC50")
  l <- l + geom_hline(aes(yintercept = tail(compound_data$cum_mean,1)), show.legend = FALSE, size = 0.2) + geom_text(aes( 0, tail(compound_data$cum_mean,1), label = "cumulative mean"), size = 3)
  l <- l +  geom_errorbar(aes(ymin = cum_mean - error, ymax = cum_mean + error), size = 0.01, width=.1, color = "blue")
  l <- l + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  l <- l + ggtitle ("Scatterplot of cumulative mean +/- 95% CI")
  ggplotly(l)
  
})#end of renderPlot expression for displaying error plot
  
})#End of shinyServer

