unzip("C:\\Users\\SARBARTHA SARKAR\\Downloads\\nycflights.csv.zip", 
      exdir = "C:\\Users\\SARBARTHA SARKAR\\Downloads")

flights1 <- read.csv("C:\\Users\\SARBARTHA SARKAR\\Downloads\\nycflights.csv")

library(dplyr)
library(lubridate)
library(RColorBrewer)
library(tidyr)

# ------------------------------------------------------------------------------------
# Objective:
# Assume all flights depart and arrive at their scheduled time.
# Using interval partitioning, visualize flights colored by carrier.
# Also, return the number of active flights for every airline in every hour (0 to 24).
# ------------------------------------------------------------------------------------

data <- flights1 %>%
  select(dep_time, arr_time, carrier) %>%
  filter(!is.na(dep_time), !is.na(arr_time))

# Function to convert integer time (e.g., 517) to "HH:MM:SS"
convert_to_hms <- function(time) {
  time_str <- sprintf("%04d", time)
  hour <- as.numeric(substr(time_str, 1, 2))
  minute <- as.numeric(substr(time_str, 3, 4))
  sprintf("%02d:%02d:00", hour, minute)
}

data <- data %>%
  mutate(
    dep_time_str = sapply(dep_time, convert_to_hms),
    arr_time_str = sapply(arr_time, convert_to_hms)
  )

# Take a manageable sample
n <- 50
data <- head(data, n)

# Convert time to decimal hours
time_to_hours <- function(t) {
  hms <- as.POSIXlt(t, format="%H:%M:%S")
  hms$hour + hms$min/60 + hms$sec/3600
}

data$start_hr <- sapply(data$dep_time_str, time_to_hours)
data$end_hr   <- sapply(data$arr_time_str, time_to_hours)

# Handle cross-midnight flights
data$end_hr[data$end_hr < data$start_hr] <- data$end_hr[data$end_hr < data$start_hr] + 24

# Assign rows to avoid overlaps (interval partitioning)
rows <- numeric(nrow(data))
rows[1] <- 1
for(i in 2:nrow(data)) {
  assigned <- FALSE
  for(r in 1:max(rows)) {
    overlap <- any(
      (data$start_hr[i] < data$end_hr[rows == r]) &
        (data$end_hr[i]   > data$start_hr[rows == r])
    )
    if(!overlap) {
      rows[i] <- r
      assigned <- TRUE
      break
    }
  }
  if(!assigned) rows[i] <- max(rows) + 1
}
data$row <- rows

# Assign colors by carrier with dynamic palette for any number of carriers
carriers <- unique(data$carrier)
colors <- colorRampPalette(brewer.pal(8, "Set2"))(length(carriers))
carrier_colors <- setNames(colors, carriers)

# Adjust margins for legend
par(xpd = TRUE, mar = c(5, 5, 4, 10))

# Plot setup: X-axis limited to full 0–24 hour range
plot(NA, NA, xlim=c(0, 24), ylim=c(0, max(data$row) + 1),
     xlab="Hour of Day", ylab="Row Index", xaxt="n", yaxt="n",
     main="Flight Schedule Distribution by Carrier (0–24 Hours)")

# X-axis and grid
axis(1, at=0:24, labels=0:24)
abline(v=0:24, lty=3, col="gray")

# Y-axis
axis(2, at=1:max(data$row), labels=1:max(data$row))

# Draw bars (flights)
for(i in 1:nrow(data)) {
  y <- data$row[i]
  start <- data$start_hr[i]
  end <- data$end_hr[i]
  ybottom <- y - 0.3
  ytop <- y + 0.3
  col <- carrier_colors[data$carrier[i]]
  
  if(end > 24) {
    rect(xleft=start, xright=24, ybottom=ybottom, ytop=ytop, col=col, border="black")
    rect(xleft=0, xright=end-24, ybottom=ybottom, ytop=ytop, col=col, border="black")
  } else {
    rect(xleft=start, xright=end, ybottom=ybottom, ytop=ytop, col=col, border="black")
  }
}

# Legend outside plot
legend("topright", inset=c(-0.35, 0), legend=names(carrier_colors),
       fill=carrier_colors, title="Carrier", border="black",
       cex=0.8, bg="white", box.lwd=0.8)

# -----------------------
# Count active flights per hour per carrier (0 to 24)
# -----------------------

hour_range <- 0:24

# Initialize a data frame to hold counts
active_flights <- expand.grid(hour = hour_range, carrier = carriers)
active_flights$active_count <- 0

for(i in 1:nrow(data)) {
  carrier <- data$carrier[i]
  start <- floor(data$start_hr[i])
  end <- ceiling(data$end_hr[i])
  
  # Account for cross-midnight by modulo 24
  hours_in_flight <- if (end > 24) c(start:23, 0:(end - 25)) else start:(end - 1)
  
  for(h in hours_in_flight) {
    if(h %in% hour_range) {
      active_flights$active_count[active_flights$hour == h & active_flights$carrier == carrier] <-
        active_flights$active_count[active_flights$hour == h & active_flights$carrier == carrier] + 1
    }
  }
}

# Convert to wide format for easier viewing
pivot_table <- pivot_wider(active_flights, names_from = carrier, values_from = active_count, values_fill = 0)

# Print the complete active flights table
print(pivot_table, n = Inf)
