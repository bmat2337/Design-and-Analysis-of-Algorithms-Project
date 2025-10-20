library(dplyr)
library(ggplot2)

# ---------- Part 1: quick plot for first 30 rows ----------
data <- read.csv("C:\\Users\\simad\\OneDrive\\Desktop\\course-catalog.csv") %>%
  select(Start.Time, End.Time, Room) %>%
  filter(Start.Time != "ARRANGED", Room != "")
data <- head(data, 30)

time_to_num <- function(t) {
  tmp <- strptime(t, format = "%I:%M %p")
  as.numeric(format(tmp, "%H")) + as.numeric(format(tmp, "%M")) / 60
}

Start.Num <- sapply(data$Start.Time, time_to_num)
End.Num <- sapply(data$End.Time, time_to_num)

X <- data.frame(
  Start.Num = Start.Num,
  End.Num = End.Num,
  Room = data$Room,
  stringsAsFactors = FALSE
)

plot(NA, NA,
     xlim = c(9, 19),
     ylim = c(0.5, nrow(X) + 0.5),
     xlab = "class-timings",
     ylab = "rooms",
     main = "timings v/s rooms",
     xaxt = "n", yaxt = "n")
axis(1, at = 9:19)
axis(2, at = 1:nrow(X))
abline(v = 9:19, lty = "dotted", col = "black")

for (i in 1:nrow(X)) {
  rect(xleft = X$Start.Num[i],
       xright = X$End.Num[i],
       ybottom = i - 1/3,
       ytop = i + 1/3,
       col = "skyblue", border = "black")
  text(x = (X$Start.Num[i] + X$End.Num[i]) / 2,
       y = i,
       labels = X$Room[i],
       cex = 0.5, col = "black")
}

intervals <- vector("list", nrow(X))
for (i in 1:nrow(X)) {
  intervals[[i]] <- c(X$Start.Num[i], X$End.Num[i])
}

intersects <- function(int1, int2) {
  return(!(int1[2] < int2[1] || int2[2] < int1[1]))
}

assign_heights <- function(intervals) {
  heights <- c()
  for (k in seq_along(intervals)) {
    used <- c()
    if (k > 1) {
      for (j in seq_len(k - 1)) {
        if (intersects(intervals[[k]], intervals[[j]])) {
          used <- c(used, heights[j])
        }
      }
    }
    h <- 1
    while (h %in% used) {
      h <- h + 1
    }
    heights[k] <- h
    cat("After comparison", k, ":\n")
    for (i in 1:k) {
      cat("[", intervals[[i]][1], ",", intervals[[i]][2], "] -> height", heights[i], "\n")
    }
    cat("\n")
  }
  return(heights)
}

heights <- assign_heights(intervals)

S <- 9:19
plot(NA, NA,
     xlim = range(S),
     ylim = c(1, nrow(X)),
     xlab = "timings",
     ylab = "classrooms",
     main = "min. req. of rooms w/o intersection",
     xaxt = "n", yaxt = "n")
axis(1, at = S)
axis(2, at = 1:nrow(X))
abline(v = S, lty = "dotted", col = "black")

bar_thickness <- 2 / 3
for (i in seq_along(intervals)) {
  rect(intervals[[i]][1], heights[i] - bar_thickness / 2,
       intervals[[i]][2], heights[i] + bar_thickness / 2,
       col = "green", border = "black")
  text(mean(intervals[[i]]), heights[i],
       labels = X$Room[i], cex = 0.5, col = "black")
}

## The number of required classrooms:
cat("Required classrooms (initial 30 rows):", max(heights), "\n")

# ---------- Part 2: timed run for chosen n ----------
n <- as.integer(readline(prompt = "Enter the number of rows to use (n): "))

total_time <- system.time({
  data <- read.csv("C:\\Users\\simad\\OneDrive\\Desktop\\course-catalog.csv") %>%
    select(Start.Time, End.Time, Room) %>%
    filter(Start.Time != "ARRANGED", Room != "")
  data <- head(data, n)
  
  time_to_num <- function(t) {
    tmp <- strptime(t, format = "%I:%M %p")
    as.numeric(format(tmp, "%H")) + as.numeric(format(tmp, "%M")) / 60
  }
  
  Start.Num <- sapply(data$Start.Time, time_to_num)
  End.Num <- sapply(data$End.Time, time_to_num)
  
  X <- data.frame(
    Start.Num = Start.Num,
    End.Num = End.Num,
    Room = data$Room,
    stringsAsFactors = FALSE
  )
  
  plot(NA, NA,
       xlim = c(9, 17),
       ylim = c(0.5, nrow(X) + 0.5),
       xlab = "Time (9AM - 5PM)",
       ylab = "Rooms",
       main = "min resources",
       xaxt = "n", yaxt = "n")
  axis(1, at = 9:17)
  axis(2, at = 1:nrow(X))
  abline(v = 9:17, lty = "dotted", col = "darkgray")
  
  for (i in 1:nrow(X)) {
    rect(xleft = X$Start.Num[i],
         xright = X$End.Num[i],
         ybottom = i - 1/3,
         ytop = i + 1/3,
         col = "skyblue", border = "blue")
    text(x = (X$Start.Num[i] + X$End.Num[i]) / 2,
         y = i,
         labels = X$Room[i],
         cex = 0.8, col = "black")
  }
  
  intervals <- vector("list", nrow(X))
  for (i in 1:nrow(X)) {
    intervals[[i]] <- c(X$Start.Num[i], X$End.Num[i])
  }
  
  intersects <- function(int1, int2) {
    return(!(int1[2] < int2[1] || int2[2] < int1[1]))
  }
  
  assign_heights_inner_time <- system.time({
    assign_heights_inner <- function(intervals) {
      heights <- c()
      for (k in seq_along(intervals)) {
        used <- c()
        if (k > 1) {
          for (j in seq_len(k - 1)) {
            if (intersects(intervals[[k]], intervals[[j]])) {
              used <- c(used, heights[j])
            }
          }
        }
        h <- 1
        while (h %in% used) {
          h <- h + 1
        }
        heights[k] <- h
        cat("After interval", k, ":\n")
        for (i in 1:k) {
          cat("[", intervals[[i]][1], ",", intervals[[i]][2], "] -> height", heights[i], "\n")
        }
        cat("\n")
      }
      return(heights)
    }
    
    final_heights <- assign_heights_inner(intervals)
    heights <- final_heights
  }) # end assign_heights_inner_time
  
  S <- 9:19
  plot(NA, NA,
       xlim = range(S),
       ylim = c(1, max(n / 2, max(heights))),
       xlab = "Time (9AM - 5PM)",
       ylab = "Height",
       main = "Intervals as horizontal bars",
       xaxt = "n", yaxt = "n")
  axis(1, at = S)
  axis(2, at = 1:max(n / 2, max(heights)))
  abline(v = S, lty = "dotted", col = "darkgray")
  
  bar_thickness <- 2 / 3
  for (i in seq_along(intervals)) {
    rect(intervals[[i]][1], heights[i] - bar_thickness / 2,
         intervals[[i]][2], heights[i] + bar_thickness / 2,
         col = "yellow", border = "red")
    text(mean(intervals[[i]]), heights[i],
         labels = paste0("I", i), cex = 0.8, col = "black")
  }
}) # end total_time

cat("\n=== TIMING RESULTS ===\n")
cat("Total execution time for n =", n, ":\n")
print(total_time)
cat("\nTime for assign_heights function (the core algorithm):\n")
print(assign_heights_inner_time)

cat("\n=== TIME COMPLEXITY ANALYSIS ===\n")
cat("The assign_heights function has O(n^2) time complexity\n")
cat("For n =", n, "intervals, the algorithm performs approximately", n * (n - 1) / 2, "comparisons\n")

interval_storage <- n * 2  # Each interval stores start & end
height_storage <- n        # Heights vector
temp_storage <- n          # Temporary vectors like 'used' during overlap checks
total_space_units <- interval_storage + height_storage + temp_storage

cat("Each interval stores start and end times (2n elements)\n")
cat("Additional memory used for 'heights' and temporary tracking vectors (O(n))\n")
cat("Total space usage (in abstract units):", total_space_units, "\n")

# ---------- Part 3: time complexity sweep (n = 1..140) ----------
n_values <- c()
elapsed_times <- c()

for (m in 1:140) {
  n_values <- c(n_values, m)
  total_time_m <- system.time({
    data_m <- read.csv("C:\\Users\\simad\\OneDrive\\Desktop\\course-catalog.csv") %>%
      select(Start.Time, End.Time, Room) %>%
      filter(Start.Time != "ARRANGED", Room != "")
    data_m <- head(data_m, m)
    
    time_to_num_m <- function(t) {
      tmp <- strptime(t, format = "%I:%M %p")
      as.numeric(format(tmp, "%H")) + as.numeric(format(tmp, "%M")) / 60
    }
    
    Start.Num_m <- sapply(data_m$Start.Time, time_to_num_m)
    End.Num_m <- sapply(data_m$End.Time, time_to_num_m)
    
    X_m <- data.frame(Start.Num = Start.Num_m, End.Num = End.Num_m, Room = data_m$Room, stringsAsFactors = FALSE)
    
    intervals_m <- vector("list", nrow(X_m))
    for (j in 1:nrow(X_m)) {
      intervals_m[[j]] <- c(X_m$Start.Num[j], X_m$End.Num[j])
    }
    
    intersects_m <- function(int1, int2) !(int1[2] < int2[1] || int2[2] < int1[1])
    
    assign_heights_m <- function(intervals) {
      heights <- c()
      for (k in seq_along(intervals)) {
        used <- c()
        if (k > 1) {
          for (p in seq_len(k - 1)) {
            if (intersects_m(intervals[[k]], intervals[[p]])) {
              used <- c(used, heights[p])
            }
          }
        }
        h <- 1
        while (h %in% used) h <- h + 1
        heights[k] <- h
      }
      return(heights)
    }
    
    final_heights_m <- assign_heights_m(intervals_m)
  })
  elapsed_times <- c(elapsed_times, total_time_m["elapsed"])
  if (m %% 10 == 0) {
    cat("Completed n =", m, "- Time:", total_time_m["elapsed"], "seconds\n")
  }
}

results_time <- data.frame(n = n_values, elapsed_time = elapsed_times)
max_elapsed <- max(elapsed_times)
max_n_val <- max(n_values)
scaling_factor <- ifelse(max_n_val^2 == 0, 1, max_elapsed / (max_n_val^2))
results_time$theoretical_n2 <- results_time$n^2 * scaling_factor

ggplot(results_time, aes(x = n)) +
  geom_line(aes(y = elapsed_time, color = "Actual Time"), size = 1) +
  geom_line(aes(y = theoretical_n2, color = "Theoretical O(n^2)"), linetype = "dashed", size = 1) +
  geom_point(aes(y = elapsed_time), size = 1.5, alpha = 0.6) +
  labs(
    title = "Time Complexity Analysis (n = 1 to 140)",
    subtitle = paste("Total execution time:", round(sum(elapsed_times), 1), "seconds"),
    x = "Input Size (n)",
    y = "Time (seconds)",
    color = "Legend"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("Actual Time" = "blue", "Theoretical O(n^2)" = "red")) +
  theme(legend.position = "bottom")

cat("\n=== SUMMARY (TIME) ===\n")
cat("Total execution time for n=1 to 140:", round(sum(elapsed_times), 1), "seconds\n")
cat("Average time per n:", round(mean(elapsed_times), 3), "seconds\n")
cat("Maximum time (at n = 140):", round(max(elapsed_times), 2), "seconds\n")

# ---------- Part 4: space complexity estimate sweep (n = 1..140) ----------
n_values_space <- c()
space_usage <- c()

for (m in 1:140) {
  n_values_space <- c(n_values_space, m)
  # estimate storage as in earlier analysis
  interval_storage_m <- m * 2
  height_storage_m <- m
  temp_storage_m <- m
  total_space_units_m <- interval_storage_m + height_storage_m + temp_storage_m
  space_usage <- c(space_usage, total_space_units_m)
  if (m %% 10 == 0) {
    cat("Completed n =", m, "- Estimated Space Units:", total_space_units_m, "\n")
  }
}

results_space <- data.frame(n = n_values_space, space_usage = space_usage)
max_space <- max(space_usage)
max_n_space <- max(n_values_space)
scaling_factor_space <- ifelse(max_n_space == 0, 1, max_space / max_n_space)
results_space$theoretical_On <- results_space$n * scaling_factor_space

ggplot(results_space, aes(x = n)) +
  geom_line(aes(y = space_usage, color = "Actual Space Usage"), size = 1) +
  geom_line(aes(y = theoretical_On, color = "Theoretical O(n)"), linetype = "dashed", size = 1) +
  geom_point(aes(y = space_usage), size = 1.5, alpha = 0.6) +
  labs(
    title = "Space Complexity Analysis (n = 1 to 140)",
    subtitle = paste("Total estimated space units:", round(sum(space_usage), 1)),
    x = "Input Size (n)",
    y = "Space Usage (abstract units)",
    color = "Legend"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("Actual Space Usage" = "red", "Theoretical O(n)" = "blue")) +
  theme(legend.position = "bottom")

cat("\n=== SUMMARY (SPACE) ===\n")
cat("Total estimated space units (n = 1 to 140):", round(sum(space_usage), 1), "\n")
cat("Average space usage per n:", round(mean(space_usage), 3), "\n")
cat("Maximum space usage (at n = 140):", round(max(space_usage), 2), "\n")

# Return the main results data frames
results_time
results_space

