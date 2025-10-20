library(ggplot2)
library(dplyr)

interval_partition_plot <- function(n = 10, seed = 123) {
  start_time <- Sys.time()
  set.seed(seed)
  
  # Generate random start times between 9.0 (9 AM) and 17.0 (5 PM)
  starts <- sort(runif(n, 9.0, 16.0))
  durations <- runif(n, 0.3, 1.5)
  ends <- pmin(starts + durations, 17.0)
  
  df <- data.frame(
    id = 1:n,
    start = starts,
    end = ends,
    height = n:1
  )
  
  p1 <- ggplot(df, aes(x = start, xend = end, y = height, yend = height)) +
    geom_segment(linewidth = 6, color = "steelblue") +
    theme_minimal(base_size = 14) +
    labs(
      title = "Initial Intervals (Height = n)",
      x = "Time (hours)",
      y = "Assigned Height"
    ) +
    scale_x_continuous(breaks = seq(9, 17, by = 1))
  print(p1)
  
  df <- df[order(df$start), ]
  room_end <- c()  # stores the current end time of each "room"
  assigned_height <- numeric(n)
  
  for (i in seq_len(n)) {
    placed <- FALSE
    for (r in seq_along(room_end)) {
      if (df$start[i] >= room_end[r]) {
        room_end[r] <- df$end[i]
        assigned_height[i] <- r
        placed <- TRUE
        break
      }
    }
    if (!placed) {
      room_end <- c(room_end, df$end[i])
      assigned_height[i] <- length(room_end)
    }
  }
  
  df$height <- assigned_height
  
  p2 <- ggplot(df, aes(x = start, xend = end, y = height, yend = height, color = factor(height))) +
    geom_segment(linewidth = 6) +
    theme_minimal(base_size = 14) +
    labs(
      title = "After Interval Partitioning (Greedy Algorithm)",
      x = "Time (hours)",
      y = "Assigned Height",
      color = "Track"
    ) +
    scale_x_continuous(breaks = seq(9, 17, by = 1))
  print(p2)
  
  end_time <- Sys.time()
  exec_time <- end_time - start_time
  cat("Execution Time:", round(exec_time, 4), "seconds\n")
}

# Example run
interval_partition_plot(n = 20)

