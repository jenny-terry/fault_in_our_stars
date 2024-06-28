library(magrittr)
options(mc.cores = parallel::detectCores())
options(scipen = 999)
options(max.print = 100000)

# read in efa data (note that the parsing issues are only to do with data types, which can be fixed as required)
efa_data <- here::here("data/efa_data.csv")|> readr::read_csv()

# select items
efa_data <- efa_data |> 
  dplyr::select(Q7.1_1:Q7.1_23, Q7.2_1:Q7.2_23, Q8.1_1:Q8.1_20, Q8.2_1:Q8.2_24)

efa_data <- efa_data |> 
  dplyr::rename(
    "STARS - Test: Studying for an examination in a statistics course" = Q7.1_1, 
    "STARS - Int: Interpreting the meaning of a table in a journal article" = Q7.1_2, 
    "STARS - Help: Going to ask my statistics teacher for individual help with material I am having difficulty understanding" = Q7.1_3, 
    "STARS - Test: Doing the coursework for a statistics course" = Q7.1_4, 
    "STARS - Int: Making an objective decision based on empirical data" = Q7.1_5,    
    "STARS - Int: Reading a journal article that includes some statistical analyses" = Q7.1_6, 
    "STARS - Int: Trying to decide which analysis is appropriate for my research project" = Q7.1_7, 
    "STARS - Test: Doing an examination in a statistics course" = Q7.1_8, 
    "STARS - Int: Reading an advertisement for a car which includes figures on miles per gallon, depreciation, etc."
    = Q7.1_9, 
    "STARS - Test: Walking into the room to take a statistics test"
    = Q7.1_10, 
    "STARS - Int: Interpreting the meaning of a probability value once I have found it" = Q7.1_11, 
    "STARS - Int: Arranging to have a body of data put into the computer" = Q7.1_12, 
    "STARS - Test: Finding that another student in class got a different answer than I did to a statistical problem" = Q7.1_13, 
    "STARS - Int: Determining whether to reject or retain the null hypothesis" = Q7.1_14, 
    "STARS - Test: Waking up in the morning on the day of a statistics test" = Q7.1_15, 
    "STARS - Help: Asking one of my teachers for help in understanding statistical output" = Q7.1_16,
    "STARS - Int: Trying to understand the odds in a lottery" = Q7.1_17, 
    "STARS - Int: Watching a student search through a load of computer output from his/her research" = Q7.1_18,
    "STARS - Help: Asking someone in the computer lab for help in understanding statistical output" = Q7.1_19,
    "STARS - Int: Trying to understand the statistical analyses described in the abstract of a journal article" = Q7.1_20, 
    "STARS - Test: Enrolling in a statistics course" = Q7.1_21, 
    "STARS - Test: Going over a final examination in statistics after it has been marked" = Q7.1_22,
    "STARS - Help: Asking a fellow student for help in understanding output" = Q7.1_23, 
    
    "R-MARS-S - Test: Studying for a statistics test" = Q7.2_1,
    "R-MARS-S - Test: Taking the statistics section of a university entrance exam" = Q7.2_2, 
    "R-MARS-S - Test: Taking an exam (quiz) in a statistics course" = Q7.2_3, 
    "R-MARS-S - Test: Taking an exam (final) in a statistics course" = Q7.2_4,
    "R-MARS-S - Test: Thinking about an upcoming statistics test 1 week before" = Q7.2_5, 
    "R-MARS-S - Test: Thinking about an upcoming statistics test 1 day before" = Q7.2_6, 
    "R-MARS-S - Test: Thinking about an upcoming statistics test 1 hour before" = Q7.2_7, 
    "R-MARS-S - Test: Realizing you have to take a certain number of statistics classes to fulfill requirements for your degree" = Q7.2_8, 
    "R-MARS-S - Test: Receiving your final statistics grade" = Q7.2_9, 
    "R-MARS-S - Test: Being give a surprise test in a statistics class" = Q7.2_10, 
    "R-MARS-S - Num: Calculating the deviances of a set of scores on paper, with each deviance being the difference between the mean of the scores and each individual score in the set" = Q7.2_11, 
    "R-MARS-S - Num: Calculating the squared deviances by multiplying each deviance by itself" = Q7.2_12, 
    "R-MARS-S - Num: Calculating the sum of squared deviances by adding the squared deviances together" = Q7.2_13, 
    "R-MARS-S - Num: Calculating the variance of scores by dividing the sum of squared deviances by the number of scores" = Q7.2_14, 
    "R-MARS-S - Course: Buying a statistics textbook" = Q7.2_15, 
    "R-MARS-S - Course: Watching a teacher work on a statistical equation on the board" = Q7.2_16, 
    "R-MARS-S - Course: Signing up for a statistics course" = Q7.2_17, 
    "R-MARS-S - Course: Listening to another student explain a statistics formula" = Q7.2_18, 
    "R-MARS-S - Course: Walking into a statistics class" = Q7.2_19,
    
    "R-MARS-S-NUM - Num: Being given a set of statistical problems involving subtraction to solve" = Q7.2_20,
    "R-MARS-S-NUM - Num: Being given a set of statistical problems involving multiplication to solve" = Q7.2_21,
    "R-MARS-S-NUM - Num: Being given a set of statistical problems involving addition to solve" = Q7.2_22,
    "R-MARS-S-NUM - Num: Being given a set of statistical problems involving division to solve" = Q7.2_23,
   
    "STARS-M - Test: Studying for an examination in a maths course" = Q8.1_1, 
    "STARS-M - Int: Interpreting numbers in a table in a journal article" = Q8.1_2, 
    "STARS-M - Help: Going to ask my maths teacher for individual help with material I am having difficulty understanding" = Q8.1_3, 
    "STARS-M - Test: Doing the coursework for a maths course" = Q8.1_4, 
    "STARS-M - Int: Making an objective decision based on numerical information" = Q8.1_5, 
    "STARS-M - Int: Reading a journal article that includes some mathematical analyses" = Q8.1_6, 
    "STARS-M - Int: Trying to decide how to approach a mathematical problem to solve it" = Q8.1_7, 
    "STARS-M - Test: Doing an examination in a maths course" = Q8.1_8, 
    "STARS-M - Test: Walking into the room to take a maths test" = Q8.1_9, 
    "STARS-M - Int: Interpreting the probability of it raining on a weather app" = Q8.1_10, 
    "STARS-M - Test: Finding that another student in class got a different answer than I did to a mathematical problem" = Q8.1_11, 
    "STARS-M - Int: Determining whether a mathematical statement is true or false" = Q8.1_12, 
    "STARS-M - Test: Waking up in the morning on the day of a maths test" = Q8.1_13, 
    "STARS-M - Help: Asking one of my teachers for help in understanding a mathematical solution" = Q8.1_14, 
    "STARS-M - Int: Watching a student search through a load of computer output from his/her maths project" = Q8.1_15, 
    "STARS-M - Help: Asking someone in the computer lab for help in understanding a mathematical solution" = Q8.1_16,
    "STARS-M - Int: Trying to understand numerical information described in an article" = Q8.1_17, 
    "STARS-M - Test: Enrolling in a maths course" = Q8.1_18,
    "STARS-M - Test: Going over a final examination in maths after it has been marked" = Q8.1_19,
    "STARS-M - Help: Asking a fellow student for help in understanding a mathematical solution" = Q8.1_20,
    
    "R-MARS - Test: Studying for a maths test" = Q8.2_1, 
    "R-MARS - Test: Taking the maths section of a university entrance exam" = Q8.2_2, 
    "R-MARS - Test: Taking an exam (quiz) in a maths course" = Q8.2_3, 
    "R-MARS - Test: Taking an exam (final) in a maths course" = Q8.2_4, 
    "R-MARS - Test: Thinking about an upcoming maths test 1 week before" = Q8.2_5, 
    "R-MARS - Test: Thinking about an upcoming maths test 1 day before" = Q8.2_6, 
    "R-MARS - Test: Thinking about an upcoming maths test 1 hour before" = Q8.2_7, 
    "R-MARS - Test: Realizing you have to take a certain number of maths classes to fulfill requirements for your degree" = Q8.2_8, 
    "R-MARS - Test: Receiving your final maths grade" = Q8.2_9, 
    "R-MARS - Test: Being given a suprise test in a maths class" = Q8.2_10, 
    "R-MARS - Num: Reading a cash register receipt after your purchase" = Q8.2_11, 
    "R-MARS - Num: Being given a set of numerical problems involving addition to solve on paper" = Q8.2_12, 
    "R-MARS - Num: Being given a set of subtraction problems to solve" = Q8.2_13, 
    "R-MARS - Num: Being given a set of multiplication problems to solve" = Q8.2_14, 
    "R-MARS - Num: Being given a set of division problems to solve" = Q8.2_15, 
    "R-MARS - Course: Buying a maths textbook" = Q8.2_16, 
    "R-MARS - Course: Watching a teacher work on an algebraic equation on the board" = Q8.2_17, 
    "R-MARS - Course: Signing up for a maths course" = Q8.2_18, 
    "R-MARS - Course: Listening to another student explain a maths formula" = Q8.2_19,
    "R-MARS - Course: Walking into a maths class" = Q8.2_20, 
    
    "R-MARS-NUM - Num: Finding the codomain of the function h(x, y) = x + y when x = {3,4,5,6} and y = {5,7,9,13}" = Q8.2_21,
    "R-MARS-NUM - Num: Finding the codomain of the function h(x, y) = x − y when x = {3,4,5,6} and y = {5,7,9,13}" = Q8.2_22,
    "R-MARS-NUM - Num: Finding the codomain of the function h(x, y) = x ×y when x = {3,4,5,6} and y = {5,7,9,13}" = Q8.2_23,
    "R-MARS-NUM - Num: Finding the codomain of the function h(x, y) = x / y when x = {3,4,5,6} and y = {5,7,9,13}" = Q8.2_24
)

# descriptive statistics (this section hates piping, so whatever)
efa_descriptives <- psych::describe(efa_data) 
efa_descriptives$item <- row.names(efa_descriptives)  
efa_descriptives <- tibble::as_tibble(efa_descriptives) 
efa_descriptives <- efa_descriptives |>
  dplyr::select(item, mean, sd, min, max, skew, kurtosis, se)
efa_descriptives

# correlation matrix
efa_data <- efa_data |> 
  dplyr::select(order(colnames(efa_data)))

efa_corr <- lavaan::lavCor(object = efa_data, 
                                    ordered = names(efa_data),
                                    output = "cor")

efa_corr <- as.data.frame(efa_corr)

# high & low corrs
efa_corr %<>% tibble::as_tibble() 
high_corrs <- sum(efa_corr >= 0.9)  
high_corrs70 <- sum(efa_corr >= 0.7)  
low_corrs <- sum(efa_corr < 0.3)
no_corrs <- nrow(efa_corr)*ncol(efa_corr)
high_corrs_percent <- high_corrs/no_corrs*100
high_corrs70_percent <- high_corrs70/no_corrs*100
low_corrs_percent <- low_corrs/no_corrs*100

# KMO test
efa_kmo <- psych::KMO(efa_data)

# determinant
efa_det <- det(as.matrix(efa_corr))

#bartlett
efa_bart <- psych::cortest.bartlett(efa_corr, n = 1000)

set.seed(260723)

# test for the number of factors
efa_out1 <- psych::fa.parallel(efa_data, fa = "fa", fm = "ml", cor = "poly", SMC = T, n.iter = 100)

efa_out2 <- psych::vss(efa_data, fm = "ml", cor = "poly")

efa_out1
efa_out2

# fancy scree plot
# https://sakaluk.wordpress.com/2016/05/26/11-make-it-pretty-scree-plots-and-parallel-analysis-using-psych-and-ggplot2/#psych

# create data frame 'obs' from observed eigenvalue data
obs <- data.frame(efa_out1$fa.values)
obs$type <- c('Observed Data')
obs$num <- c(row.names(obs))
obs$num <- as.numeric(obs$num)
colnames(obs) <- c('eigenvalue', 'type', 'num')

# calculate quantiles for eigenvalues, but only store those from simulated CF model in percentile1
percentile <- apply(efa_out1$values, 2, function(x) quantile(x,.95))
min <- as.numeric(nrow(obs))
min <- (4*min) - (min-1)
max <- as.numeric(nrow(obs))
max <- 4*max
percentile1 <- percentile[min:max]
 
# create data frame called 'sim' with simulated eigenvalue data
sim <- data.frame(percentile1)
sim$type <- c('Simulated Data (95th %ile)')
sim$num <- c(row.names(obs))
sim$num <- as.numeric(sim$num)
colnames(sim) <- c('eigenvalue', 'type', 'num')
 
# merge the two data frames (obs and sim) together into data frame called eigendat
eigendat <- rbind(obs, sim)

# use data from eigendat, map number of factors to x-axis, eigenvalue to y-axis, and give different data point shapes depending on whether eigenvalue is observed or simulated
library(ggplot2)
scree_plot <- ggplot(eigendat, aes(x = num, y = eigenvalue, shape = type)) +
  geom_line() +
  geom_point(size = 4) +
  scale_y_continuous(name = 'Eigenvalue') +
  scale_x_continuous(name = 'Factor Number', breaks = min(eigendat$num):max(eigendat$num), limits = c(1, 25)) +
  scale_shape_manual(values = c(16, 1)) +
  geom_vline(xintercept = efa_out1$nfact, linetype = 'dashed') +
  jtools::theme_apa()
scree_plot

# 22 factor rotaion

efa_mod22 <- psych::fa(efa_data, nfactors = 22, rotate = "oblimin", fm = "ml", cor = "poly", SMC = T, p = .0)
efa_mod22

save.image(file = "workspace.RData")
beepr::beep(sound = 5)

# printLoadings_function
# code from this Stack Exchange post: https://stackoverflow.com/questions/17371266/extracting-output-from-principal-function-in-psych-package-as-a-data-frame

getS3method("print","loadings")
printLoadings <- function (x, digits = 3, cutoff = 0.1, sort = FALSE, ...) 
{
   Lambda <- unclass(x)
   p <- nrow(Lambda)
   factors <- ncol(Lambda)
   if (sort) {
      mx <- max.col(abs(Lambda))
      ind <- cbind(1L:p, mx)
      mx[abs(Lambda[ind]) < 0.5] <- factors + 1
      Lambda <- Lambda[order(mx, 1L:p), ]
   }
   cat("\nLoadings:\n")
   fx <- format(round(Lambda, digits))
   names(fx) <- NULL
   nc <- nchar(fx[1L], type = "c")
   fx[abs(Lambda) < cutoff] <- paste(rep(" ", nc), collapse = "")
   newx <- print(fx, quote = FALSE, ...)
   vx <- colSums(x^2)
   varex <- rbind(`SS loadings` = vx)
   if (is.null(attr(x, "covariance"))) {
      varex <- rbind(varex, `Proportion Var` = vx/p)
      if (factors > 1) 
         varex <- rbind(varex, `Cumulative Var` = cumsum(vx/p))
   }
   cat("\n")
   print(round(varex, digits))
   invisible(newx)
}

# write csv 22 for table shenanigans
efa_22_loadings2 <- printLoadings(efa_mod22$loadings, digits = 2, cutoff = 0, sort = T)
efa_22_loadings2 <- tibble::rownames_to_column(as.data.frame(efa_22_loadings2), "items")
#readr::write_csv(efa_22_loadings2, here::here("data/efa_22_loadings2.csv"))

# rotate and write_8
efa_mod8 <- psych::fa(efa_data, nfactors = 8, rotate = "oblimin", fm = "ml", cor = "poly", SMC = T, p = .0)
efa_mod8

efa_8_loadings <- printLoadings(efa_mod8$loadings, digits = 2, cutoff = 0.3, sort = T)
efa_8_loadings <- tibble::rownames_to_column(as.data.frame(efa_8_loadings), "items")
#readr::write_csv(efa_8_loadings, here::here("data/efa_8_loadings.csv"))