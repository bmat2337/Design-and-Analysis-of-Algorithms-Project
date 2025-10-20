# Interval Partitioning Visualizations in R

This repository contains multiple **R scripts** that demonstrate real-world applications of the **greedy interval partitioning algorithm** — a classic scheduling method that assigns tasks, flights, or deliveries to the minimum number of resources without overlap. Each file processes a different dataset and visualizes the resulting schedules.

---

## Files Overview

### **I. random.R**

Generates and visualizes **random time intervals** between 9 AM and 5 PM.
Applies the **greedy interval partitioning algorithm** to optimally assign each interval to the earliest available track (room).
Plots intervals **before and after scheduling**, illustrating efficient resource allocation.

---

### **II. course_catalogue.R**

Reads class schedule data and converts start/end times to numeric values.
Uses interval partitioning to assign **classrooms without overlap**.
Analyzes **time complexity (O(n²))** and **space complexity (O(n))** through performance plots and visual comparisons.

---

### **III. nycflights13.R**

Uses data from the **`nycflights13`** package to visualize scheduled flights by carrier.
Applies interval partitioning to assign non-overlapping rows per flight and **colors flights by airline**.
Plots schedules between **3 AM–4 PM** and outputs a **pivot table** of hourly active flights per carrier.

---

### **IV. food_delivery.R**

Processes **`food delivery data.csv`** to analyze delivery schedules.
Converts order and delivery times to numeric hours and assigns deliveries to non-overlapping rows.
Plots their 24-hour distribution and prints **active delivery counts per hour**, showing workload variation.

---

### **V. ISI.R**

Reads **`ISI.csv`** containing class schedules.
Cleans and splits time data, converts it to numeric form, detects overlaps, and assigns classrooms using interval partitioning.
Visualizes batch-wise class intervals for each weekday and displays **minimum classrooms required** per day.

---

### **VI. uber.R**

Analyzes **`uber.csv`** to visualize **vehicle activity** throughout the day.
Cleans trip data, converts times to numeric hours, and assigns overlapping trips to separate rows.
Each **vehicle type** is color-coded, and a 24-hour plot shows their active intervals with a legend.

---

### **VII. flights.R**

Processes **`flights.csv`** to visualize airline schedules.
Converts departure and arrival times to decimal hours and applies interval partitioning to assign flights to rows.
Plots flights by **carrier color** and prints **hourly active flight counts** per airline for the 0–24 hour range.

---

## Core Concept: Interval Partitioning

All scripts use a **greedy interval partitioning algorithm**:

1. Sort intervals by start time.
2. Assign each interval to the **earliest available resource** (track, room, vehicle, etc.) that has no overlap.
3. If none are available, create a new resource.

This ensures **optimal resource usage** and efficient visualization of temporal data.

---

## Visualization & Analysis Tools

* **Packages used:** `dplyr`, `lubridate`, `tidyr`, `RColorBrewer`, `ggplot2` (for some visualizations)
* **Visual outputs:** Timeline plots showing non-overlapping intervals, color-coded by category.
* **Performance metrics:** Execution time and space complexity analysis for selected scripts.

---

## Learning Outcome

This project demonstrates:

* The **greedy interval partitioning algorithm** in multiple domains.
* How to visualize scheduling problems in R.
* Real-world resource optimization across diverse datasets — from classrooms to flights and deliveries.

---

## Author

**[SARBARTHA SARKAR BMAT2337]**
**[ANIS KUMAR SARKAR SARKAR BMAT2309]**
---
