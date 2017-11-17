#Introduction

This application is designed to display control charts and repeated measures and control compound MSRs calculated from a user input data table.

The application is seperated into intuitively labelled tabs. Users can input a data as a csv file and a selected compound as a reference compound.

The tabs on main page then display the respective tables and graphs along with calculated MSR values.

#Instructions

In order for the application to work properly, it is important for the user to follow these guidelines:

The input data should be entered as a comma separated file (*.csv) and can include any number of rows and columns, however, it must include the following column names: 'COMPOUND_ID', 'REFERENCE' and 'IC50'.

The Repeated measures MSR and the control compound MSRs are calculated on the values entered in the IC50 column.

The application does not substitute missing values from a data set and these are usually omitted from the analysis.

In the Compound Input tab on the sidebar, the user should include a value for a single compound ID defined in the COMPOUND_ID column. There should be an exact match as the app is not designed for pattern matching or for displaying information for more than one compound selected.

#MSR Calculations

Potency measurements from concentration-response assays are usually log-normally distributed and as such, log10AC50 is preferred for statistical analysis and modeling, including estimation of the MSR.

The MSR is defined as the smallest ratio between the potencies of two compounds that is statistically significant and is calculated as MSR = 10^2sqrt2s , where s is an estimate of the standard deviation of a log potency for one compound. The variability estimate s can be estimated in different ways depending on available data and associated analysis method e.g., within-run, between-run, and other sources of variability

The common ways to estimate assay variability are detailed below.

##Replicate-Experiment MSR

Replicate-Experiment MSR is a diagnostic and decision tool used to establish that an assay is ready to go into production. It is estimated from two independent runs of 20-30 compounds and is calculated as MSR = 102Sd.

where sd is the standard deviation of the paired differences in log potency across the two runs.

Two runs are not considered adequate to estimate between-run variability so the Replicate-Experiment MSR focuses only on within-run variability.

Use of the standard deviation of the paired differences factors out the between-run variability.

##Control Compound MSR

A minimum of six runs is considered adequate to estimate between-run variability, and the Control Compound MSR can be calculated from six or more runs as MSR = 10^2sqrt2s

where s is the standard deviation of the log10AC50 values across runs, assuming one AC50 result per run.

#Next version

Plans are in place to impove upon this application by adding the following updates:

1. Selction of data points either via the table or the graphical display for subsequent MSR analysis.

2. Wildcard search capability for specific compounds

3. Option to perform MSR calculations for variables in multiple columns defined by the user

This is a work in progress so if you have further commments, please do not hesitate to get in touch
