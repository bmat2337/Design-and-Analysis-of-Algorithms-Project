library(dplyr)
library(lubridate)
library(hms)
library(RColorBrewer)
library(tidyr)

data <- read.csv("C://Users//simad//OneDrive//Desktop//food delivery data.csv")

data <- data %>%
  select(Ordered.Time, Delivery.Time..mins.) %>%
  mutate(
    Ordered.Time = as_hms(strptime(Ordered.Time, format = "%I:%M %p")),
    Delivered.Time = as_hms(as.numeric(Ordered.Time) + Delivery.Time..mins. * 60)
  )

time_to_hours <- function(t) {
  as.numeric(hour(t)) + as.numeric(minute(t)) / 60 + as.numeric(second(t)) / 3600
}

data <- data %>%
  mutate(
    start_hr = time_to_hours(Ordered.Time),
    end_hr = time_to_hours(Delivered.Time)
  )

data$end_hr[data$end_hr < data$start_hr] <- data$end_hr[data$end_hr < data$start_hr] + 24

n <- 100
data <- head(data, n)
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

par(xpd = TRUE, mar = c(5, 5, 4, 10))
plot(
  NA, NA, xlim = c(0, 24), ylim = c(0, max(data$row) + 1),
  xlab = "Hour of Day", ylab = "Order Index",
  xaxt = "n", yaxt = "n", main = "Food Delivery Schedule Distribution"
)

axis(1, at = 0:24, labels = 0:24)
abline(v = 0:24, lty = 3, col = "gray")
axis(2, at = 1:max(data$row), labels = 1:max(data$row))

colors <- colorRampPalette(brewer.pal(8, "Set2"))(max(data$row))

for (i in 1:nrow(data)) {
  y <- data$row[i]
  start <- data$start_hr[i]
  end <- data$end_hr[i]
  ybottom <- y - 0.3
  ytop <- y + 0.3
  col <- colors[y]
  if (end > 24) {
    rect(xleft = start, xright = 24, ybottom = ybottom, ytop = ytop, col = col, border = "black")
    rect(xleft = 0, xright = end - 24, ybottom = ybottom, ytop = ytop, col = col, border = "black")
  } else {
    rect(xleft = start, xright = end, ybottom = ybottom, ytop = ytop, col = col, border = "black")
  }
}

hour_range <- 0:23
active_orders <- data.frame(hour = hour_range, active_count = 0)

for (i in 1:nrow(data)) {
  start <- floor(data$start_hr[i])
  end <- ceiling(data$end_hr[i])
  hours_active <- if (end > 24) c(start:23, 0:(end - 25)) else start:(end - 1)
  for (h in hours_active) {
    if (h %in% hour_range) {
      active_orders$active_count[active_orders$hour == h] <-
        active_orders$active_count[active_orders$hour == h] + 1
    }
  }
}

print(active_orders)

