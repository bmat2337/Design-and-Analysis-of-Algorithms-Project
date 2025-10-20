library(nycflights13)
library(dplyr)
library(lubridate)
library(RColorBrewer)
library(tidyr)

# ------------------------------------------------------------------------------------
# Objective :
# Assume all flights depart and arrive at their scheduled time.
# Using interval partitioning, visualize flights colored by carrier.
# Also, return the number of active flights for every airline in every hour (3 to 16).
# ------------------------------------------------------------------------------------

data <- flights %>%
  select(sched_dep_time, sched_arr_time, carrier) %>%
  filter(!is.na(sched_dep_time), !is.na(sched_arr_time))

convert_to_hms <- function(time) {
  time_str <- sprintf("%04d", time)
  hour <- as.numeric(substr(time_str, 1, 2))
  minute <- as.numeric(substr(time_str, 3, 4))
  sprintf("%02d:%02d:00", hour, minute)
}

data <- data %>%
  mutate(
    dep_time_str = sapply(sched_dep_time, convert_to_hms),
    arr_time_str = sapply(sched_arr_time, convert_to_hms)
  )

n <- 50
data <- head(data, n)

time_to_hours <- function(t) {
  hms <- as.POSIXlt(t, format = "%H:%M:%S")
  hms$hour + hms$min / 60 + hms$sec / 3600
}

data$start_hr <- sapply(data$dep_time_str, time_to_hours)
data$end_hr <- sapply(data$arr_time_str, time_to_hours)
data$end_hr[data$end_hr < data$start_hr] <- data$end_hr[data$end_hr < data$start_hr] + 24

rows <- numeric(nrow(data))
rows[1] <- 1

for (i in 2:nrow(data)) {
  assigned <- FALSE
  for (r in 1:max(rows)) {
    overlap <- any(
      (data$start_hr[i] < data$end_hr[rows == r]) &
        (data$end_hr[i] > data$start_hr[rows == r])
    )
    if (!overlap) {
      rows[i] <- r
      assigned <- TRUE
      break
    }
  }
  if (!assigned) rows[i] <- max(rows) + 1
}

data$row <- rows
carriers <- unique(data$carrier)
colors <- colorRampPalette(brewer.pal(8, "Set2"))(length(carriers))
carrier_colors <- setNames(colors, carriers)

par(xpd = TRUE, mar = c(5, 5, 4, 10))
plot(
  NA, NA, xlim = c(3, 16), ylim = c(0, max(data$row) + 1),
  xlab = "Hour of Day", ylab = "Row Index", xaxt = "n", yaxt = "n",
  main = "Flight Schedule Distribution by Carrier (3 AM to 4 PM)"
)
axis(1, at = 3:16, labels = 3:16)
abline(v = 3:16, lty = 3, col = "gray")
axis(2, at = 1:max(data$row), labels = 1:max(data$row))

for (i in 1:nrow(data)) {
  y <- data$row[i]
  start <- data$start_hr[i]
  end <- data$end_hr[i]
  ybottom <- y - 0.3
  ytop <- y + 0.3
  col <- carrier_colors[data$carrier[i]]
  if (end > 24) {
    rect(xleft = start, xright = 24, ybottom = ybottom, ytop = ytop, col = col, border = "black")
    rect(xleft = 0, xright = end - 24, ybottom = ybottom, ytop = ytop, col = col, border = "black")
  } else {
    rect(xleft = start, xright = end, ybottom = ybottom, ytop = ytop, col = col, border = "black")
  }
}

legend(
  "topright", inset = c(-0.35, 0), legend = names(carrier_colors),
  fill = carrier_colors, title = "Carrier", border = "black",
  cex = 0.8, bg = "white", box.lwd = 0.8
)

# -----------------------
# Count active flights per hour per carrier (3 to 16)
# -----------------------

hour_range <- 3:16
active_flights <- expand.grid(hour = hour_range, carrier = carriers)
active_flights$active_count <- 0

for (i in 1:nrow(data)) {
  carrier <- data$carrier[i]
  start <- floor(data$start_hr[i])
  end <- ceiling(data$end_hr[i])
  hours_in_flight <- if (end > 24) c(start:23, 0:(end - 25)) else start:(end - 1)
  for (h in hours_in_flight) {
    if (h %in% hour_range) {
      active_flights$active_count[active_flights$hour == h &
                                    active_flights$carrier == carrier] <-
        active_flights$active_count[active_flights$hour == h &
                                      active_flights$carrier == carrier] + 1
    }
  }
}

pivot_table <- pivot_wider(active_flights, names_from = carrier, values_from = active_count, values_fill = 0)
print(pivot_table)

