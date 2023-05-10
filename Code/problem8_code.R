# Problem 8: Linear Regression
appliance.data <- data.frame(
  Region = c(1:6), Expenditure = c(1.5, 5.0, 8.0, 2.0, 4.0,4.5),
  Sales = c(2.0,4.0,4.5,2.0,2.5,3.0), stringsAsFactors = FALSE )

appliance.data

relation <- lm(formula = Sales ~ Expenditure,
               data = appliance.data)

# Give the chart file a name.
png(file = "problem8_Sales_Expenditure_regression.png")

# Plot the chart.
plot(appliance.data$Expenditure,appliance.data$Sales,col = "blue",main = "Expenditures and Sales Regression",
     abline(relation, col="blue"),cex = 1.3,pch = 16,xlab = expression(paste("Expenditures ", 10^3, "£")),
     ylab = expression(paste("Sales ", 10^6, "£")) )
# Save the file.
dev.off()

print(summary(relation))

# (b) Estimate the expected sales for a region where 6.3 thousand pounds are being spent on advertising.
a <- data.frame(Expenditure= 6.30)
result <-  predict(relation,a)
result
