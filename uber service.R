library(dplyr)
library(lubridate)
library(RColorBrewer)

data <- read.csv("C://Users//simad//OneDrive//Desktop//uber.csv") %>%
  select(Time, Vehicle.Type, Avg.CTAT) %>%
  filter(Avg.CTAT != "null")

data$end <- as.POSIXct(data$Time, format = "%H:%M:%S") +
  as.numeric(as.character(data$Avg.CTAT)) * 60

data$Time <- format(as.POSIXct(data$Time, format = "%H:%M:%S"), "%H:%M:%S")
data$end <- format(data$end, "%H:%M:%S")
data <- head(data, 30)

time_to_hours <- function(t) {
  hms <- as.POSIXlt(t, format = "%H:%M:%S")
  hms$hour + hms$min / 60 + hms$sec / 3600
}

data$start_hr <- sapply(data$Time, time_to_hours)
data$end_hr <- sapply(data$end, time_to_hours)
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

vehicle_types <- unique(data$Vehicle.Type)
colors <- brewer.pal(min(length(vehicle_types), 8), "Set2")
vehicle_colors <- setNames(colors[seq_along(vehicle_types)], vehicle_types)

par(xpd = TRUE, mar = c(5, 5, 4, 10))

# Plot
plot(
  NA, NA,
  xlim = c(0, 24), ylim = c(0, max(data$row) + 1),
  xlab = "Hour of Day", ylab = "Row Index",
  xaxt = "n", yaxt = "n",
  main = "Vehicle Time Distribution"
)
axis(1, at = 0:24, labels = 0:24)
abline(v = 0:24, lty = 3, col = "gray")
axis(2, at = 1:max(data$row), labels = 1:max(data$row))

for (i in 1:nrow(data)) {
  y <- data$row[i]
  start <- data$start_hr[i]
  end <- data$end_hr[i]
  ybottom <- y - 0.3
  ytop <- y + 0.3
  col <- vehicle_colors[data$Vehicle.Type[i]]
  
  if (end > 24) {
    rect(
      xleft = start, xright = 24,
      ybottom = ybottom, ytop = ytop,
      col = col, border = "black"
    )
    rect(
      xleft = 0, xright = end - 24,
      ybottom = ybottom, ytop = ytop,
      col = col, border = "black"
    )
  } else {
    rect(
      xleft = start, xright = end,
      ybottom = ybottom, ytop = ytop,
      col = col, border = "black"
    )
  }
}

legend(
  "topright", inset = c(-0.35, 0),
  legend = names(vehicle_colors),
  fill = vehicle_colors,
  title = "Vehicle Type",
  border = "black", cex = 0.8,
  bg = "white", box.lwd = 0.8
)