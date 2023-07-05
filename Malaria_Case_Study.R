
## List of indicators from the website
library(jsonlite)
url <- "https://ghoapi.azureedge.net/api/Indicator"
# Send GET request and retrieve the JSON data
response <- jsonlite::fromJSON(url)
# Convert the JSON data to a dataframe
indicators <- as.data.frame(response$value)


#####################  01. MALARIA SITUATION   ########################
#######################################################################

###### 01) a. Malaria deaths by world region
mal_deaths_region <- read.csv("Deaths_Estimated_Region.csv")
str(mal_deaths_region)

#remove uneeded variables
library(dplyr)
mal_deaths_region <- mal_deaths_region[,-c(1:7, 9, 11:23, 25:34)]
str(mal_deaths_region)
colnames(mal_deaths_region) <- c("region", "period", "no_of_deaths")
mal_deaths_region$no_of_deaths <- as.numeric(mal_deaths_region$no_of_deaths)
mal_deaths_region$period <- as.Date(paste0(mal_deaths_region$period, "-01-01"))
mal_deaths_region$region <- as.factor(mal_deaths_region$region)

#plot the graph

library(ggplot2)
library(plotly)
library(dplyr)

str(mal_deaths_region)
p <- ggplot(mal_deaths_region, aes(x = period, y = no_of_deaths, fill = region)) +
        geom_area(position = "identity", colour = "black", size = 0.2, alpha = 0.4) +
        scale_fill_brewer(palette = "Accent") +
        scale_y_continuous(breaks = seq(0, max(mal_deaths_region$no_of_deaths), by = 200000),
                           labels = function(x) format(x, big.mark = ",", scientific = FALSE)) +
        labs(x = "Year", y = "Number of Deaths", title = "Fig 01: Malaria Deaths by Region")

# Add vertical lines at each year
p <- ggplotly(p, tooltip = "all", dynamicTicks = TRUE)

p <- p %>% layout(hovermode = "x", hoverdistance = 100)
p

##################
mal_deaths_region2 <- mal_deaths_region
mal_deaths_region2$period <- format(as.Date(mal_deaths_region2$period), "%Y")
colnames(mal_deaths_region2) <- c("Region", "Period", "Deaths")


a <- ggplot(mal_deaths_region2, aes(x = Period, y = Deaths, fill = Region)) +
        geom_point(position = "identity", shape = 21, size = 2.5, color = "black", stroke = 0.3) +
        scale_fill_brewer(palette = "Accent") +
        scale_y_continuous(breaks = seq(0, max(mal_deaths_region2$Deaths), by = 200000),
                           labels = function(x) format(x, big.mark = ",", scientific = FALSE)) +
        labs(x = "Year", y = "Number of Deaths", title = "Fig 01: Malaria Deaths by Region")

# Add vertical lines at each year
a <- ggplotly(a, tooltip = "all", dynamicTicks = TRUE)


# Set layout options
a <- a %>% layout(hovermode = "x", hoverdistance = 100)

# Print the plot
a

############################

a <- ggplot(mal_deaths_region2, aes(x = Period, y = Deaths, fill = Region, text = paste("Deaths:", Deaths, "<br>Region:", Region))) +
        geom_point(position = "identity", shape = 21, size = 2.5, color = "black", stroke = 0.3) +
        scale_fill_brewer(palette = "Accent") +
        scale_y_continuous(breaks = seq(0, max(mal_deaths_region2$Deaths), by = 200000),
                           labels = function(x) format(x, big.mark = ",", scientific = FALSE)) +
        labs(x = "Year", y = "Number of Deaths", title = "Fig 01: Malaria Deaths by Region")

# Calculate the sum of Deaths by Region
sum_deaths <- aggregate(Deaths ~ Region, mal_deaths_region2, sum)

# Add the sum of Deaths to the tooltip
a <- a + geom_text(data = sum_deaths, aes(label = paste("Sum of Deaths:", Deaths)), 
                   vjust = -1, color = "black", size = 4)

# Convert the plot to interactive mode with tooltips
a <- ggplotly(a, tooltip = "text", dynamicTicks = TRUE)

# Set layout options
a <- a %>% layout(hovermode = "x", hoverdistance = 100)

# Print the plot
a










































###### 01) b. Malaria deaths in SSA --> Top Five Countries

mal_deaths_africa <- read.csv("Deaths_Estimated_Africa.csv")
str(mal_deaths_africa)

mal_deaths_africa <- mal_deaths_africa[,-c(1:7, 9, 11:23, 25:34)]
str(mal_deaths_africa)
colnames(mal_deaths_africa) <- c("country", "period", "no_of_deaths")
mal_deaths_africa$no_of_deaths <- as.numeric(mal_deaths_africa$no_of_deaths)
mal_deaths_africa$period <- as.Date(paste0(mal_deaths_africa$period, "-01-01"))
mal_deaths_africa$country <- as.factor(mal_deaths_africa$country)

table(is.na(mal_deaths_africa$no_of_deaths)) #check for NAs
mal_deaths_africa <- na.omit(mal_deaths_africa) #remove NAs

top_five <- mal_deaths_africa %>%
        group_by(period) %>%
        top_n(5, no_of_deaths) %>% 
        group_by(no_of_deaths) %>% 
        ungroup()

str(top_five)
table(top_five$country)

# filter years 2011 - 2020
top_five_filtered <- top_five %>%
        filter(period >= as.Date("2011-01-01") & period <= as.Date("2020-01-01"))
   
# plot the top 5 countries
ggplot(top_five_filtered, aes(x = period, y = no_of_deaths, fill = reorder(country, no_of_deaths))) +
        geom_bar(stat = "identity") +
        labs(x = "Period", y = "Number of Deaths", title = "Fig 02: Top Five Countries with Most Malaria Deaths") +
        theme_minimal() +
        scale_y_continuous(labels = scales::comma)

library(scales)
library(RColorBrewer)

# Create a vector of colors for each country
country_colors <- ifelse(top_five_filtered$country == "United Republic of Tanzania", "red", scales::brewer_pal(palette = "Greens")(length(unique(top_five_filtered$country))))

# Plot top 5 countries
ggplot(top_five_filtered, aes(x = period, y = no_of_deaths, fill = reorder(country, no_of_deaths))) +
        geom_bar(stat = "identity") +
        labs(x = "Period", y = "Number of Deaths", title = "Fig 02: Top Five Countries with Most Malaria Deaths") +
        theme_minimal() +
        scale_fill_manual(values = country_colors, guide = guide_legend(title = "Countries")) +
        scale_y_continuous(labels = scales::comma) +
        theme(
                plot.title = element_text(size = 14, hjust = 0.5)
        )


###### 01) c. Malaria Incidence and Mortality --> Tanzania and Global

# Incidence #
#Import data - Global incidence
global_incidence <- read.csv("Incidence_Estimated_Global.csv")
str(global_incidence)

global_incidence <- global_incidence[,c(10, 24)]
str(global_incidence)
colnames(global_incidence) <- c("year", "incidence")
global_incidence$incidence <- as.numeric(global_incidence$incidence)
global_incidence$year <- as.Date(paste0(global_incidence$year, "-01-01"))
global_incidence$location <- c(rep("global"))

#Import data - Tanzania incidence
tz_incidence <- read.csv("Incidence_Estimated_Tanzania.csv")
str(tz_incidence)

tz_incidence <- tz_incidence[,c(10, 24)]
str(tz_incidence)
colnames(tz_incidence) <- c("year", "incidence")
tz_incidence$incidence <- as.numeric(tz_incidence$incidence)
tz_incidence$year <- as.Date(paste0(tz_incidence$year, "-01-01"))
tz_incidence$location <- c(rep("tanzania"))

#Merge incidence dataframes and plot!

mal_incidence <- rbind(global_incidence, tz_incidence)
str(mal_incidence)
ggplot(mal_incidence, aes(x = year, y = incidence, color = location, group = location)) +
        geom_point(shape = 21, size = 2, stroke = 1.5) +
        geom_line(size = 1, position = "identity") +
        geom_hline(yintercept = 0, color = "black", size = 0.1) +
        labs(x = "Year", y = "Malaria Case Incidence (cases per 1,000 population at risk)",
             title = "Fig 3a: Trends in Malaria Incidence - Tanzania and Globally") +
        scale_color_manual(values = c('global' = 'grey80', 'tanzania' = 'springgreen3')) +
        theme(panel.background = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.title.y = element_text(size = 9),
              plot.title = element_text(size = 16, hjust = 0.5)  # Adjust the size here
        )

# Mortality #
#Import data - Global mortality
global_mortality <- read.csv("Mortality_Estimated_Global.csv")
str(global_mortality)

global_mortality <- global_mortality[,c(10, 24)]
str(global_mortality)
colnames(global_mortality) <- c("year", "mortality")
global_mortality$mortality <- as.numeric(global_mortality$mortality)
global_mortality$year <- as.Date(paste0(global_mortality$year, "-01-01"))
global_mortality$location <- c(rep("global"))

#Import data - Tanzania incidence
tz_mortality <- read.csv("Mortality_Estimated_Tanzania.csv")
str(tz_mortality)

tz_mortality <- tz_mortality[,c(10, 24)]
str(tz_mortality)
colnames(tz_mortality) <- c("year", "mortality")
tz_mortality$mortality <- as.numeric(tz_mortality$mortality)
tz_mortality$year <- as.Date(paste0(tz_mortality$year, "-01-01"))
tz_mortality$location <- c(rep("tanzania"))

#Merge mortality dataframes and plot!

mal_mortality <- rbind(global_mortality, tz_mortality)
str(mal_mortality)
ggplot(mal_mortality, aes(x = year, y = mortality, color = location, group = location)) +
        geom_point(shape = 21, size = 2, stroke = 1.5) +
        geom_line(size = 1, position = "identity") +
        geom_hline(yintercept = 0, color = "black", size = 0.1) +
        labs(x = "Year", y = "Malaria Mortality (deaths per 100,000 population at risk)",
             title = "Fig 3b: Trends in Malaria Mortality - Tanzania and Globally") +
        scale_color_manual(values = c('global' = 'grey80', 'tanzania' = 'red2')) +
        theme(panel.background = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.title.y = element_text(size = 9),
              plot.title = element_text(size = 16, hjust = 0.5)  # Adjust the size here
        )


###### 01) d. Malaria prevalence in under 5's --> Spatial distribution Tanzania

## 02) Create a map for malaria prevalence in under 5's

#import prevelence dataframe
library(readxl)
prevalence_under5s <- read_excel("prevalence_under5s.xlsx")
colnames(prevalence_under5s) <- c("region", "prevalence", "category")
prevalence_under5s$category <- as.factor(prevalence_under5s$category)
str(prevalence_under5s)

library(sf)
geo_data <- st_read("/Users/janenyandele/Desktop/Capstone Project/Possibility 02/GIS_Maps/Regions.shp")
prevalence_merge <- merge(geo_data, prevalence_under5s, by.x = "Region_Nam", by.y = "region", all.x = TRUE)
prevalence_merge <- prevalence_merge[-c(7,8,12,13,18),]
str(prevalence_merge)

install.packages("ggpointdensity")
library(ggpointdensity)
install.packages("lwgeom")
library(lwgeom)

ggplot() +
        geom_sf(data = prevalence_merge, aes(fill = factor(category, levels = c("<1", "1-5", "6-10", "11-17", "18-23")))) +
        scale_fill_manual(values = c("lightyellow", "khaki", "gold", "orange2", "red3"),
                          labels = paste0(levels(prevalence_merge$category), "%"),
                          guide = guide_legend(title = "")) +
        geom_sf_label(data = prevalence_merge, aes(label = paste0(Region_Nam, "\n", prevalence, "%")), size = 1.5, fontface = "bold") +
        theme_void() +
        labs(title = "Fig 04: Malaria Prevalence in Under 5's")



######################  02. MALARIA INTERVENTIONS   ######################
##########################################################################

# 2.1: Nets, Sprays and Treatment

#Import data

nets <- read.csv("2. Nets_Tanzania.csv")
nets <- nets[,c(10, 24)]
nets$variable <- rep("Population with access_to ITNs (%)")
colnames(nets) <- c("year", "coverage", "variable")
nets$coverage <- as.numeric(nets$coverage)
nets$year <- as.factor(nets$year)
str(nets)

irs <- read.csv("1. IRS_Tanzania.csv")
irs <- irs[,c(10, 24)]
irs$variable <- rep("Number of people protected_from malaria by IRS")
colnames(irs) <- c("year", "coverage", "variable")
irs$coverage <- as.numeric(irs$coverage)
irs$year <- as.factor(irs$year)
str(irs)

treat <- read.csv("3. Treated_any_therapy_Tanzania.csv")
treat <- treat[,c(10, 24)]
treat$variable <- rep("Number of malaria_cases treated")
colnames(treat) <- c("year", "coverage", "variable")
treat$coverage <- as.numeric(treat$coverage)
treat$year <- as.factor(treat$year)
str(treat)


#Plot the plot
library(scales)
interventions <- rbind(nets, irs, treat)
colnames(interventions) <- c("Year", "Coverage", "variable")

ggplot(interventions, aes(x = Year, y = Coverage, group = variable)) +
        geom_point(shape = 21, size = 2, stroke = 2, aes(color = variable)) +
        geom_line(aes(color = variable), size = 1) +
        facet_wrap(~ variable, nrow = 1, scales = "free_y", 
                   labeller = labeller(variable = function(x) gsub("_", "\n", x))) +
        labs(title = "Fig 05: Three-year Trend in Coverage of Malaria Interventions") +
        theme_minimal() +
        theme(
                plot.title = element_text(size = 16, hjust = 0.5),
                legend.position = "none"
        ) +
        scale_y_continuous(
                breaks = function(x) pretty(x, n = 5),
                labels = function(x) format(x, big.mark = ",", scientific = FALSE)
        )


# 2.2: mRDT and Microscopy diagnosis

#Import datasets for mRDT, Microscopy and modeled estimates
cases_rdt <- read.csv("cases_rdt.csv")
cases_rdt <- cases_rdt[,c(10, 24)]
colnames(cases_rdt) <- c("year", "cases_rdt")
cases_rdt$cases_rdt <- as.numeric(cases_rdt$cases_rdt)
cases_rdt$year <- as.factor(cases_rdt$year)
str(cases_rdt)

cases_micr <- read.csv("cases_microscopy.csv")
cases_micr <- cases_micr[,c(10, 24)]
colnames(cases_micr) <- c("year", "cases_micr")
cases_micr$cases_micr <- as.numeric(cases_micr$cases_micr)
cases_micr$year <- as.factor(cases_micr$year)
str(cases_micr)

cases_estim <- read.csv("cases_estimated.csv")
cases_estim <- cases_estim[,c(10, 24)]
colnames(cases_estim) <- c("year", "cases_estim")
cases_estim$cases_estim <- as.numeric(cases_estim$cases_estim)
cases_estim$year <- as.factor(cases_estim$year)
cases_estim <- cases_estim[-c(1),]
str(cases_estim)

#merge the above into a single dataframe

cases_all <- merge(cases_rdt, cases_micr, by = "year")
cases_all <- merge(cases_all, cases_estim, by = "year")

#reshape the dataframe from wide to long format
cases_combined_long <- cases_all %>%
        tidyr::pivot_longer(-year, names_to = "category", values_to = "cases")


# Create separate data frames for each category
cases_estim2 <- subset(cases_combined_long, category == "cases_estim")
cases_micr2 <- subset(cases_combined_long, category == "cases_micr")
cases_rdt2 <- subset(cases_combined_long, category == "cases_rdt")

diagnostics <- rbind(cases_micr2, cases_rdt2)

# Plot the plot

r <- ggplot() +
        geom_bar(data = cases_rdt2, aes(x = as.factor(year), y = cases, fill = category),
                 stat = "identity", width = 0.2, position = position_dodge(width = 1.3)) +
        geom_bar(data = cases_micr2, aes(x = as.factor(year), y = cases, fill = category),
                 stat = "identity", width = 0.2, position = position_dodge(width = 1.3))
r +
        geom_bar(data = cases_rdt2, aes(x = as.factor(year), y = cases, fill = category),
                 stat = "identity", width = 0.4, position = position_nudge(x = 0.17)) +
        geom_bar(data = cases_micr2, aes(x = as.factor(year), y = cases, fill = category),
                 stat = "identity", width = 0.4, position = position_nudge(x = 0.17)) +
        geom_bar(data = cases_estim2, aes(x = as.factor(year), y = cases, fill = category),
                 stat = "identity", width = 0.4, position = position_nudge(x = -0.15)) +
        scale_x_discrete(name = "Year") +
        labs(title = "Trends in Malaria Diagnoses") +
        theme_minimal() +
        scale_fill_manual(values = c("indianred1", "steelblue1", "aquamarine3"),
                          labels = c("Estimated Malaria Cases", "Microscopy Cases", "mRDT Cases")) +
        scale_y_continuous(labels = scales::comma, name = "Number of Cases") +
        theme(plot.title = element_text(hjust = 0.5))


################  03. INCIDENCE AND MORTALITY FORECASTS   ################
##########################################################################

##### 3.1: Plot Incidence time series

library(forecast)

tz_incidence
str(tz_incidence)
#re-order date column so that dataframe starts with oldest observation to newest
tz_incidence2 <- tz_incidence[order(tz_incidence$year),] 
tz_incidence2 <- tz_incidence2[-c(1:5),]

incidence_ts <- ts(tz_incidence2$incidence, start = c(2010), #make the time series object
                   end = c(2020), frequency = 1)
par(mar = c(3, 3, 2, 1))  # Adjust the margin values as needed
plot(incidence_ts) # make an exploratory plot
acf(incidence_ts) # check correlations

avg_inc_model <- Arima(incidence_ts, c(0,0,0))
str(avg_inc_model)

avg_forecast <- forecast(avg_inc_model)
str(avg_forecast)
avg_forecast$mean #extract chunk of data representing the predictions from the model
avg_forecast = forecast(avg_inc_model, 10, level = c(50, 95))
avg_forecast$mean
forecast_time_index <- ts(rep(NA, 10), start = end(incidence_ts) + 1, frequency = frequency(incidence_ts))


# use two-step process to plot forecast (***did not work in our case!)
plot(incidence_ts)
lines(avg_forecast$mean, col = "pink")

plot(avg_forecast) #plot the forecast directly
autoplot(avg_forecast) #you can do the same plot, but with ggplot2

arima_model <- auto.arima(incidence_ts, seasonal = FALSE)
arima_forecast <- forecast(arima_model)
plot(arima_forecast)


# 3.1b: Plot Forecast Incidence

forecast_df1 <- as.data.frame(arima_forecast)
str(forecast_df1)
#Adjust the dataframe
forecast_df1$year <- c(2021:2030)
forecast_df1 <- forecast_df1[,-c(2:5)]
colnames(forecast_df1) <- c("incidence", "year")
forecast_df1$type <- c(rep("forecast"))
forecast_df1 <- forecast_df1[,c(2,1,3)]
str(forecast_df1)

tz_incidence3 <- tz_incidence2
tz_incidence3$location <- c(rep("actual"))
colnames(tz_incidence3) <- c("year", "incidence", "type")
tz_incidence3$year <- as.integer(format(tz_incidence3$year, "%Y"))

incidence_forecast <- rbind(tz_incidence3, forecast_df1)

# Create the plot using ggplot2

y <- ggplot(incidence_forecast, aes(x = year, y = incidence)) +
        geom_point(data = subset(incidence_forecast, year <= 2021), shape = 21, size = 2, stroke = 1.5, color = "dodgerblue2", aes(fill = "Before 2021")) +
        geom_point(data = subset(incidence_forecast, year >= 2021), shape = 21, size = 2, stroke = 1.5, color = "lightskyblue3", aes(fill = "After 2021")) +
        geom_line(data = subset(incidence_forecast, year <= 2021), size = 1, position = "identity", color = "dodgerblue2", aes(group = 1)) +
        geom_line(data = subset(incidence_forecast, year >= 2021), size = 1, position = "identity", color = "lightskyblue3", linetype = "dotted", aes(group = 2)) +
        geom_hline(yintercept = 0, color = "black", size = 0.1) +
        labs(x = "Year", y = "Malaria Case Incidence (cases per 1,000 population at risk)",
             title = "Forecasts of Progress in Malaria Incidence - Tanzania") +
        theme(panel.background = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.title.y = element_text(size = 9)) +
        scale_fill_manual(values = c("Before 2021" = "dodgerblue2", "After 2021" = "lightskyblue3"),
                          labels = c("Forecasted Incidence Estimates", "Current Incidence Estimates"),
                          guide = guide_legend(title = "", override.aes = list(shape = 21, size = 2.5, stroke = 1.5)))

y + 
        guides(fill = guide_legend(order = 1, override.aes = list(shape = 21, size = 2.5, stroke = 1.5))) +
        geom_point(data = data.frame(year = c(2025, 2030), incidence = c(35.1, 14)), aes(color = factor(year)), shape = 20, 
                   size = 3.5, stroke = 1.5) +
        scale_color_manual(values = c("red2", "aquamarine3"), labels = c("GTS target 2025", "GTS target 2030")) +
        geom_text(data = data.frame(year = c(2025, 2030), incidence = c(35.1, 14)), aes(label = incidence), 
                  vjust = 2.1, color = "black", size = 3.5) +
        guides(color = guide_legend(order = 2, title = "", override.aes = list(shape = 20)),
               fill = guide_legend(title = NULL))




##### 3.2: Plot Mortality time series
tz_mortality
str(tz_mortality)

#re-order date column so that dataframe starts with oldest observation to newest
tz_mortality2 <- tz_mortality[order(tz_mortality$year),] 
tz_mortality2 <- tz_mortality2[-c(1:10),]

mortality_ts <- ts(tz_mortality2$mortality, start = c(2010), #make the time series object
                   end = c(2020), frequency = 1)
par(mar = c(3, 3, 2, 1))  # Adjust the margin values as needed
plot(mortality_ts) # make an exploratory plot
acf(mortality_ts) # check correlations

avg_mort_model <- Arima(mortality_ts, c(0,0,0))
str(avg_mort_model)

avg_forecast_mort <- forecast(avg_mort_model)
str(avg_forecast_mort)
avg_forecast_mort$mean #extract chunk of data representing the predictions from the model
avg_forecast_mort = forecast(avg_mort_model, 10, level = c(50, 95))
avg_forecast_mort$mean
forecast_time_index_mort <- ts(rep(NA, 10), start = end(mortality_ts) + 1, frequency = frequency(mortality_ts))


# use two-step process to plot forecast (***did not work in our case!)
plot(mortality_ts)
lines(avg_forecast_mort$mean, col = "pink")

plot(avg_forecast_mort) #plot the forecast directly
autoplot(avg_forecast_mort) #you can do the same plot, but with ggplot2

arima_model_mort <- auto.arima(mortality_ts, seasonal = FALSE)
arima_forecast_mort <- forecast(arima_model_mort)
plot(arima_forecast_mort)

# 3.2b: Plot Forecast Mortality

forecast_df2 <- as.data.frame(arima_forecast_mort)
str(forecast_df2)
#Adjust the dataframe
forecast_df2$year <- c(2021:2030)
forecast_df2 <- forecast_df2[,-c(2:5)]
colnames(forecast_df2) <- c("mortality", "year")
forecast_df2$type <- c(rep("forecast"))
forecast_df2 <- forecast_df2[,c(2,1,3)]
str(forecast_df2)

tz_mortality3 <- tz_mortality2
tz_mortality3$location <- c(rep("actual"))
colnames(tz_mortality3) <- c("year", "mortality", "type")
tz_mortality3$year <- as.integer(format(tz_mortality3$year, "%Y"))

mortality_forecast <- rbind(tz_mortality3, forecast_df2)

# Create the Plot using ggplot2

z <- ggplot(mortality_forecast, aes(x = year, y = mortality)) +
        geom_point(data = subset(mortality_forecast, year <= 2021), shape = 21, size = 2, stroke = 1.5, color = "red3", aes(fill = "Before 2021")) +
        geom_point(data = subset(mortality_forecast, year >= 2021), shape = 21, size = 2, stroke = 1.5, color = "lightskyblue3", aes(fill = "After 2021")) +
        geom_line(data = subset(mortality_forecast, year <= 2021), size = 1, position = "identity", color = "red3", aes(group = 1)) +
        geom_line(data = subset(mortality_forecast, year >= 2021), size = 1, position = "identity", color = "lightskyblue3", linetype = "dotted", aes(group = 2)) +
        geom_hline(yintercept = 0, color = "black", size = 0.1) +
        labs(x = "Year", y = "Malaria Mortality Rate ( per 100,000 population at risk)",
             title = "Forecasts of Progress in Malaria Mortality - Tanzania") +
        theme(panel.background = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.title.y = element_text(size = 9)) +
        scale_fill_manual(values = c("Before 2021" = "red3", "After 2021" = "lightskyblue3"),
                          labels = c("Forecasted Mortality Estimates", "Current Mortality Estimates"),
                          guide = guide_legend(title = "", override.aes = list(shape = 21, size = 2.5, stroke = 1.5)))

z + 
        guides(fill = guide_legend(order = 1, override.aes = list(shape = 21, size = 2.5, stroke = 1.5))) +
        geom_point(data = data.frame(year = c(2025, 2030), mortality = c(10.87, 4.35)), aes(color = factor(year)), shape = 20, 
                   size = 3.5, stroke = 1.5) +
        scale_color_manual(values = c("chartreuse3", "dodgerblue2"), labels = c("GTS target 2025", "GTS target 2030")) +
        geom_text(data = data.frame(year = c(2025, 2030), mortality = c(10.87, 4.35)), aes(label = mortality), 
                  vjust = 2.1, color = "black", size = 3.5) +
        guides(color = guide_legend(order = 2, title = "", override.aes = list(shape = 20)),
               fill = guide_legend(title = NULL))











































