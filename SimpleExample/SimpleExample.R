features <- data.frame(feature = character(), npv = numeric(), degradation = numeric(), size = numeric(), stringsAsFactors = FALSE)
features[1, ]  <- c("Feature 1",  100, 1,  1) # Value, Degradation, Size !! all estimates !!
features[2, ]  <- c('Feature 2',  150, 1,  3)
features[3, ]  <- c('Feature 3',   80, 4,  5)
features[4, ]  <- c('Feature 4',  200, 3,  3)
features[5, ]  <- c('Feature 5',  170, 1,  8)
features[6, ]  <- c('Feature 6',  310, 5, 13)
features[7, ]  <- c('Feature 7',   90, 1,  2)
features[8, ]  <- c('Feature 8',  120, 3,  2)
features[9, ]  <- c('Feature 9',  110, 1,  1)
features[10, ] <- c('Feature 10', 170, 2,  3)
features[11, ] <- c('Feature 11', 230, 3,  5)
features[12, ] <- c('Feature 12', 270, 5,  8)
features[13, ] <- c('Feature 13', 160, 4,  1)
features[14, ] <- c('Feature 14', 350, 4,  8)
features[15, ] <- c('Feature 15', 290, 3, 13)
features[16, ] <- c('Feature 16', 180, 1,  5)
features[17, ] <- c('Feature 17', 270, 1,  3)
features[18, ] <- c('Feature 18', 130, 5,  1)
features[19, ] <- c('Feature 19', 140, 3,  8)
features[20, ] <- c('Feature 20', 210, 1,  5)

runs = 100000 # we have 20! permutations ~2,4*10^18

orders = list()
orders_cod = list()

while (runs > 0) {
    data <- features[sample(1:nrow(features)), ]
    data$order <- seq.int(nrow(data))
    data$npv_size <- as.numeric(data$npv) / as.numeric(data$size)

    data$degradation_factor <- as.numeric(data$degradation) * data$order / 100
    data$real_npv <- as.numeric(data$npv) / ( 1 + data$degradation_factor )
    data$cod <- as.numeric(data$npv) - data$real_npv

    cod <- sum(data$cod)
    if (length(orders_cod) < 10) {
        orders[[length(orders)+1]] <- data
        orders_cod[[length(orders_cod)+1]] <- cod
    } else {
        position = which.max(orders_cod)
        max <- orders_cod[position]
        if (max > cod) {
            orders[position] <- data
            orders_cod[position] <- cod
        }
    }
    runs <- runs - 1
}

# linear stupid way: COD = 761
# best simulation until now: 568, 562
# reduced cod by 25%
# so, if the 761 were 1M EURO, you can pay me 250k EURO 

