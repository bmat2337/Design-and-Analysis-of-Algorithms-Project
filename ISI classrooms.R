library(dplyr)
library(tidyr)

data <- read.csv("C://Users//simad//OneDrive//Desktop//ISI.csv")

data <- data %>%
  mutate(Duration = gsub("\\.", ":", Duration)) %>%
  separate(Duration, into = c("Start.Time", "End.Time"), sep = "-") %>%
  mutate(across(c(Start.Time, End.Time), trimws)) %>%
  mutate(across(c(Start.Time, End.Time), ~ paste0(.x, ":00")))

data <- data %>%
  rename(Day = Week.day, Batch = Batches)

time_to_num <- function(t) {
  tmp <- as.POSIXct(t, format = "%H:%M:%S")
  as.numeric(format(tmp, "%H")) + as.numeric(format(tmp, "%M")) / 60
}

data <- data %>%
  mutate(
    Start.Num = sapply(Start.Time, time_to_num),
    End.Num = sapply(End.Time, time_to_num)
  )

intersects <- function(int1, int2) {
  !(int1[2] <= int2[1] || int2[2] <= int1[1])
}

assign_heights <- function(intervals) {
  heights <- numeric(length(intervals))
  for (k in seq_along(intervals)) {
    used <- c()
    for (j in seq_len(k - 1)) {
      if (intersects(intervals[[k]], intervals[[j]])) {
        used <- c(used, heights[j])
      }
    }
    h <- 1
    while (h %in% used) h <- h + 1
    heights[k] <- h
  }
  heights
}

par(mfrow = c(3, 2), mar = c(4, 4, 3, 1))

cols <- c("B1" = "skyblue", "B2" = "lightgreen", "B3" = "plum", "M1" = "gold")
weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")

for (day in weekdays) {
  df <- data %>% filter(Day == day)
  if (nrow(df) == 0) {
    plot.new()
    title(main = paste("No classes on", day))
    next
  }
  df <- df %>% arrange(Start.Num)
  intervals_day <- lapply(seq_len(nrow(df)), function(i) c(df$Start.Num[i], df$End.Num[i]))
  heights_day <- assign_heights(intervals_day)
  plot(
    NA, NA,
    xlim = c(8, 18),
    ylim = c(0.5, max(heights_day) + 0.5),
    xlab = "Time (hours)",
    ylab = "Class Row",
    main = paste("Schedule for", day),
    xaxt = "n", yaxt = "n"
  )
  axis(1, at = 8:18)
  axis(2, at = 1:max(heights_day))
  abline(v = 8:18, lty = "dotted", col = "gray")
  bar_thickness <- 0.4
  for (i in seq_along(intervals_day)) {
    b <- df$Batch[i]
    rect(
      intervals_day[[i]][1],
      heights_day[i] - bar_thickness / 2,
      intervals_day[[i]][2],
      heights_day[i] + bar_thickness / 2,
      col = cols[b], border = "black"
    )
    subject_abbr <- substr(df$Subject[i], 1, 10)
    text(mean(intervals_day[[i]]), heights_day[i], labels = subject_abbr, cex = 0.5, font = 2)
  }
}

plot.new()
legend(
  "center",
  legend = names(cols),
  fill = cols,
  border = "black",
  title = "Batch Colors",
  cex = 1.2,
  bty = "n"
)

cat("\nMinimum required classrooms per day:\n")
for (day in weekdays) {
  df <- data %>% filter(Day == day)
  if (nrow(df) > 0) {
    intervals_day <- lapply(seq_len(nrow(df)), function(i) c(df$Start.Num[i], df$End.Num[i]))
    heights_day <- assign_heights(intervals_day)
    cat(day, ":", max(heights_day), "rooms\n")
  } else {
    cat(day, ": 0 rooms (no classes)\n")
  }
}

cat("\nClass distribution:\n")
print(table(data$Day, data$Batch))
