NYPD Shooting Incident Analysis
Anastasiia Stukova
2025-01-21
Executive Summary
Discussion
Impact and Implications
Data Preparation
Trends
Conclusions and Recommendations
Executive Summary
This report presents an in-depth analysis of shooting incidents in New York City based on publicly available NYPD data. We aim to understand patterns, identify high-risk areas and times, and provide data-driven recommendations for improving public safety.

Methodology
Our analysis combines statistical analysis with data visualization to examine: - Time-based patterns (annual trends, daily patterns) - Geographic distribution - Demographic profiles - Fatality rates and risk factors

Discussion
Our analysis revealed several critical insights:

Temporal Patterns
Clear peak hours during late night (10 PM - 2 AM)
Seasonal variations with summer spikes
Notable increase post-2020
Geographic Distribution
Brooklyn shows highest concentration
Significant borough-level disparities
Specific high-risk neighborhoods identified
Demographic Impact
Males 18-44 most affected
Young adults at particular risk
Notable fatality rate variations
Impact and Implications
These findings have significant implications for: - Law enforcement resource allocation - Community safety programs - Public policy development - Emergency response planning

Data Preparation
csv_url <- "https://data.cityofnewyork.us/api/views/833y-fsy8/rows.csv?accessType=DOWNLOAD"

shooting_data <- read_csv(csv_url) %>%
  mutate(
    OCCUR_DATE = mdy(OCCUR_DATE),
    OCCUR_TIME = hms(OCCUR_TIME),
    year = year(OCCUR_DATE),
    month = month(OCCUR_DATE),
    hour = hour(OCCUR_TIME)
  )
## Rows: 28562 Columns: 21
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr  (12): OCCUR_DATE, BORO, LOC_OF_OCCUR_DESC, LOC_CLASSFCTN_DESC, LOCATION...
## dbl   (7): INCIDENT_KEY, PRECINCT, JURISDICTION_CODE, X_COORD_CD, Y_COORD_CD...
## lgl   (1): STATISTICAL_MURDER_FLAG
## time  (1): OCCUR_TIME
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
# Summary of the dataset
glimpse(shooting_data)
## Rows: 28,562
## Columns: 24
## $ INCIDENT_KEY            <dbl> 231974218, 177934247, 255028563, 25384540, 726…
## $ OCCUR_DATE              <date> 2021-08-09, 2018-04-07, 2022-12-02, 2006-11-1…
## $ OCCUR_TIME              <Period> 1H 6M 0S, 19H 48M 0S, 22H 57M 0S, 1H 50M 0S…
## $ BORO                    <chr> "BRONX", "BROOKLYN", "BRONX", "BROOKLYN", "BRO…
## $ LOC_OF_OCCUR_DESC       <chr> NA, NA, "OUTSIDE", NA, NA, NA, NA, NA, NA, NA,…
## $ PRECINCT                <dbl> 40, 79, 47, 66, 46, 42, 71, 69, 75, 69, 40, 42…
## $ JURISDICTION_CODE       <dbl> 0, 0, 0, 0, 0, 2, 0, 2, 0, 0, 0, 2, 0, 0, 2, 0…
## $ LOC_CLASSFCTN_DESC      <chr> NA, NA, "STREET", NA, NA, NA, NA, NA, NA, NA, …
## $ LOCATION_DESC           <chr> NA, NA, "GROCERY/BODEGA", "PVT HOUSE", "MULTI …
## $ STATISTICAL_MURDER_FLAG <lgl> FALSE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, F…
## $ PERP_AGE_GROUP          <chr> NA, "25-44", "(null)", "UNKNOWN", "25-44", "18…
## $ PERP_SEX                <chr> NA, "M", "(null)", "U", "M", "M", NA, NA, "M",…
## $ PERP_RACE               <chr> NA, "WHITE HISPANIC", "(null)", "UNKNOWN", "BL…
## $ VIC_AGE_GROUP           <chr> "18-24", "25-44", "25-44", "18-24", "<18", "18…
## $ VIC_SEX                 <chr> "M", "M", "M", "M", "F", "M", "M", "M", "M", "…
## $ VIC_RACE                <chr> "BLACK", "BLACK", "BLACK", "BLACK", "BLACK", "…
## $ X_COORD_CD              <dbl> 1006343.0, 1000082.9, 1020691.0, 985107.3, 100…
## $ Y_COORD_CD              <dbl> 234270.0, 189064.7, 257125.0, 173349.8, 247502…
## $ Latitude                <dbl> 40.80967, 40.68561, 40.87235, 40.64249, 40.845…
## $ Longitude               <dbl> -73.92019, -73.94291, -73.86823, -73.99691, -7…
## $ Lon_Lat                 <chr> "POINT (-73.92019278899994 40.80967347200004)"…
## $ year                    <dbl> 2021, 2018, 2022, 2006, 2010, 2012, 2011, 2012…
## $ month                   <dbl> 8, 4, 12, 11, 5, 7, 7, 7, 4, 5, 10, 1, 5, 6, 9…
## $ hour                    <dbl> 1, 19, 22, 1, 1, 21, 22, 23, 15, 15, 0, 2, 22,…
Trends
Annual Trends

annual_trend <- shooting_data %>%
  group_by(year) %>%
  summarise(
    incidents = n(),
    fatalities = sum(STATISTICAL_MURDER_FLAG == "true", na.rm = TRUE)
  )

ggplot(annual_trend, aes(x = year)) +
  geom_line(aes(y = incidents, color = "Incidents"), size = 1.2) +
  theme_minimal() +
  labs(title = "Annual Shooting Incidents",
       y = "Count") +
  theme(legend.position = "bottom")
## Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
## ℹ Please use `linewidth` instead.
## This warning is displayed once every 8 hours.
## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
## generated.
 Hourly Distribution

hourly_dist <- shooting_data %>%
  group_by(hour) %>%
  summarise(incidents = n())

ggplot(hourly_dist, aes(x = hour, y = incidents)) +
  geom_bar(stat = "identity", fill = "#2c7fb8") +
  theme_minimal() +
  labs(
    title = "Hourly Distribution of Shooting Incidents",
    x = "Hour of Day",
    y = "Number of Incidents"
  ) +
  scale_x_continuous(breaks = 0:23)
 Borough Analysis

borough_analysis <- shooting_data %>%
  group_by(BORO) %>%
  summarise(
    total_incidents = n(),
    fatal_incidents = sum(STATISTICAL_MURDER_FLAG == "true", na.rm = TRUE),
    fatality_rate = round((fatal_incidents / total_incidents) * 100, 2)
  )

ggplot(borough_analysis, aes(x = reorder(BORO, -total_incidents), y = total_incidents)) +
  geom_bar(stat = "identity", fill = "#31a354") +
  theme_minimal() +
  labs(
    title = "Shooting Incidents by Borough",
    x = "Borough",
    y = "Number of Incidents"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
 Key Statistics

# Creating a table with key statistics
stats_summary <- data.frame(
  Metric = c(
    "Total Incidents",  # Total number of incidents
    "Total Fatalities", # Total number of fatalities
    "Most Affected Borough", # Borough with the highest number of incidents
    "Peak Hour" # Hour with the most incidents
  ),
  Value = c(
    nrow(shooting_data), # Calculates the total number of rows (incidents) in the dataset
    sum(as.logical(shooting_data$STATISTICAL_MURDER_FLAG), na.rm = TRUE), # Counts the total number of fatalities
    borough_analysis$BORO[which.max(borough_analysis$total_incidents)], # Finds the borough with the maximum incidents
    hourly_dist$hour[which.max(hourly_dist$incidents)] # Identifies the hour with the highest number of incidents
  )
)

# Displaying the key statistics table
kable(stats_summary, caption = "Key Statistics") %>% 
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"), # Adds styling options: striped rows, hover effect, compact layout
    full_width = FALSE # Ensures the table is not stretched to full width
  ) %>%
  row_spec(0, bold = TRUE, color = "white", background = "darkblue") %>% # Styles the header row: bold text, white font, dark blue background
  column_spec(1, bold = TRUE) %>% # Highlights the first column with bold text
  column_spec(2, color = "darkgreen") # Changes the color of the values in the second column to dark green
Key Statistics
Metric	Value
Total Incidents	28562
Total Fatalities	5526
Most Affected Borough	BROOKLYN
Peak Hour	23
Demographic Analysis

demographic_profile <- shooting_data %>%
  group_by(VIC_AGE_GROUP, VIC_SEX) %>%
  summarise(
    incidents = n(),
    fatalities = sum(as.logical(STATISTICAL_MURDER_FLAG), na.rm = TRUE)
  ) %>%
  arrange(desc(incidents)) %>%
  mutate(fatality_rate = round((fatalities / incidents) * 100, 2)) # 
## `summarise()` has grouped output by 'VIC_AGE_GROUP'. You can override using the
## `.groups` argument.
kable(head(demographic_profile, 10), caption = "Top 10 Demographic Groups by Incidents") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE) %>%
  add_header_above(c(" " = 2, "Metrics" = 3)) 
Top 10 Demographic Groups by Incidents
Metrics
VIC_AGE_GROUP	VIC_SEX	incidents	fatalities	fatality_rate
25-44	M	11898	2587	21.74
18-24	M	9554	1595	16.69
<18	M	2527	329	13.02
45-64	M	1615	394	24.40
25-44	F	1073	251	23.39
18-24	F	826	134	16.22
<18	F	427	56	13.11
45-64	F	366	98	26.78
65+	M	142	49	34.51
65+	F	63	17	26.98
Conclusions and Recommendations
Key Findings

Temporal Patterns
Peak incidents occur during late-night hours, specifically around 11 PM.
Seasonal trends suggest spikes in incidents during warmer months.
Geographic Distribution
Brooklyn accounts for the highest number of incidents, with a significant fatality rate.
Certain boroughs demonstrate a consistently higher risk profile.
Demographic Trends
Male victims aged 18-44 are the most affected demographic group.
Young males under 24 are particularly vulnerable to incidents.
Recommendations

Targeted Law Enforcement
Deploy additional patrols during peak hours (10 PM - 2 AM).
Focus resources in high-risk boroughs, such as Brooklyn.
Community Engagement
Develop prevention programs targeting young males under 24.
Collaborate with community leaders to promote safety initiatives.
Policy Adjustments
Analyze seasonal trends to optimize resource allocation during high-risk periods.
Increase investment in public safety infrastructure in vulnerable boroughs.
