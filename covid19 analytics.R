install.packages("covid19.analytics") #installing
library(covid19.analytics) 

#This function is used to read "live" data from reported Covid'19 cases.
mydata = covid19.data(case = "aggregated", local.data = FALSE, debrief = FALSE) 

View(mydata)
names(mydata)
tsc = covid19.data("ts-confirmed")

#This function is used to summarize the current situation, it will first download the latest data and then summarize the top provinces/cities per case. It results on screen table and static plots (pie and bar plots) with reported information. It can also output the tables into a text file.
report.summary(cases.to.process="TS", saveReport = FALSE, graphical.output = TRUE, geo.loc = NULL)

#It compute totals per region and plot time series for that specific region/country. It provides static plots: data + models (exp/linear, Poisson, Gamma), mosaic and histograms when more than one location are selected.
tots.per.location(tsc, geo.loc ="India", confBnd = FALSE, nbr.plts = 1, info=" ")

#It compute changes and growth rates per region and plot time series for that specific region/country. It displays list containing two dataframes: one reporting changes on daily basis and a second one reporting growth rates, for the indicated regions. It also produces static plots: data + models (linear,Poisson,Exp), mosaic and histograms when more than one location are selected.
growth.rate(data0=tsc, geo.loc ="India", stride=1, info="")

#It is a function to visualize different indicators for trends in daily changes of cases reported as time series data. It is composed of static plots: total number of cases vs time, daily changes vs total changes in different representations.
single.trend(tsc, confBnd = TRUE, info = "")

#This function is used to plot total number of cases per day for different groups. It produces static and interactive plot
totals.plt(data0 = tsc, geo.loc0 ="India", one.plt.per.page = FALSE,  log.plt = FALSE, with.totals = FALSE, interactive.fig = TRUE, fileName = NULL)

#It is a function to visualize trends in daily changes in time series data interactively.
itrends(ts.data =tsc, geo.loc ="INDIA", with.totals = FALSE, fileName = NULL)

#It generates an interactive map displaying cases around the world.
live.map(data = tsc, select.projctn = TRUE, projctn = "orthographic", title = "", no.legend = FALSE, szRef = 0.2, fileName = NULL)

#It generates a SIR (Susceptible-Infected-Recovered) model based on the actual data of the Covid-19 cases. It provides list containing the fits for the SIR model.
model = generate.SIR.model(data = tsc, geo.loc = "India",
                           t0 = NULL, t1 = NULL, deltaT = NULL,
                           tfinal = 90, fatality.rate = 0.02, tot.population = 130*10^9,
                           staticPlt = TRUE, interactiveFig = FALSE)

#It is a function to plot the results from the SIR model function. It results static and interactive plots
plt.SIR.model(SIR.model=model, geo.loc = "India",
              interactiveFig = FALSE, fileName = NULL)

