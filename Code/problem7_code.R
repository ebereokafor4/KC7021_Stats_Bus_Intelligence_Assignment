# Problem 7: Compute the correlation coefficient

adverts <- c(10,	7,	6,	5,	4,	3,	2,	0)
purchases <- c(12,	3,	8,	10,	5,	4,	1,	4)

correlation <- cor(adverts, purchases, method = "pearson")
print(correlation)