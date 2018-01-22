#Functions to produce visuals aiding in Portfolio Project Selection and Justification
install.packages("ggplot2")
install.packages("gridExtra")
library(ggplot2)
library(gridExtra)

#Epic Burn-Up Chart Visual
generateEpicBurnUp <- function(iterationName, initialEstimate, actualStoryPoints, cumulativeStoryPoints) {

    df <- data.frame(iterationName, initialEstimate, actualStoryPoints, cumulativeStoryPoints)

    ggplot(df, aes(x = iterationName, y = initialEstimate, group = 1)) +
      geom_line(aes(y = initialEstimate), lwd = 1.25) +
      ggtitle("Epic Burn-Up") +
      xlab("Increment and Iteration") +
      ylab("Effort (in Story Points)") +
      geom_line(aes(y = actualStoryPoints), col = "red", lwd = 1.25) +
      geom_line(aes(y = cumulativeStoryPoints), col = "darkgreen", lwd = 1.25) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

#Epic Burn-Up Chart Visual Example
iterationName <- c("PI 1 - Iteration 1", "PI 1 - Iteration 2", "PI 1 - Iteration 3", "PI 1 - Iteration 4", "PI 1 - Iteration 5", "PI 1 - Iteration 6")
initialEstimate <- c(500, 500, 550, 550, 570, 570)
actualStoryPoints <- c(75, 65, 78, 108, 115, 130)
cumulativeStoryPoints <- cumsum(actualStoryPoints)
plot1 <- generateEpicBurnUp(iterationName, initialEstimate, actualStoryPoints, cumulativeStoryPoints)

#Weight Shortest Job First (WSJF) Visual
generateWsjf <- function(featureName, duration, cod) {

    weight <- cod / duration
    df <- data.frame(featureName, duration, cod, weight)
    df$loc <- df$duration / 2

    ggplot(df, aes(x = loc, y = cod, width = duration)) +
      geom_bar(aes(fill = featureName), stat = "identity", position = "identity", alpha = .4) +
      ggtitle("Cost of Delay") +
      guides(fill = guide_legend(title = "Feature Name")) +
      xlab("Estiamted Duration (in Iterations)") +
      ylab("Cost of Delay")
}

#WSJF Visual Example
featureName <- c("Feature #1", "Feature #2", "Feature #3", "Feature #4", "Feature #5", "Feature #6", "Feature #7", "Feature #8", "Feature #9", "Feature #10")
duration <- c(2, 4, 7, 8, 3, 4, 4, 5, 10, 12)
cod <- c(12.0, 4.25, 7.5, 3.333, 5.0, 6.0, 13.75, 10.0, 2.25, 1.25)
plot2 <- generateWsjf(featureName, duration, cod)

#Investment Opportunity Schedule (IOS)
generateIos <- function(projectName, projectReturnRate, projectCost) {

    projectReturnRate <- projectReturnRate * 100
    df <- data.frame(projectName, projectReturnRate, projectCost)
    df <- df[with(df, order(-projectReturnRate)),]
    df$loc <- cumsum(df$projectCost) - df$projectCost / 2

    ggplot(df, aes(x = loc, y = projectReturnRate, width = projectCost)) +
      geom_bar(aes(fill = projectName), stat = "identity") +
      scale_x_continuous(breaks = df$loc) +
      ggtitle("Investment Opportunity Schedule") +
      xlab("Costs ($)") +
      ylab("Rate of Return (%)") +
      guides(fill = guide_legend(title = "Project/Epic")) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

#IOS Visual Example
projectName <- c("Epic #1", "Epic #2", "Epic #3", "Epic #4", "Epic #5", "Epic #6")
projectReturnRate <- c(.10, .22, .05, .12, .30, .09)
projectCost <- c(124000, 212000, 153000, 420000, 75000, 98000)
plot3 <- generateIos(projectName, projectReturnRate, projectCost)

#Epic Progress Bars
generateEpicProgress <- function(epicNames, startDates, targetDates, totalFeaturePoints, completedFeaturePoints, initialEstimates) {

    df <- data.frame(epicNames, startDates, targetDates, totalFeaturePoints, completedFeaturePoints, initialEstimates)
    df$percentComplete <- completedFeaturePoints / totalFeaturePoints
    df$remainingPoints <- totalFeaturePoints - completedFeaturePoints

    ggplot(df, aes(x = epicNames, y = totalFeaturePoints, color = epicNames)) +
      geom_bar(aes(y = totalFeaturePoints, fill = epicNames), stat = "identity") +
      geom_text(aes(label = totalFeaturePoints), col = "black", hjust = 1.25) +
      geom_bar(aes(y = completedFeaturePoints), fill = "green", stat = "identity") +
      geom_text(aes(label = completedFeaturePoints), col = "green", hjust = 1.25, vjust = -1.15) +
      geom_errorbar(aes(ymin = initialEstimates, ymax = initialEstimates), col = "red", lwd = 1.15) +
      coord_flip() +
      ggtitle("Epic Progress Report") +
      xlab("Epic (Project) Name") +
      ylab("Effort (in Story Points)") +
      theme(legend.position = "none")
}

#Epic Progres Bars Visual Example
epicNames <- c("Epic #1", "Epic #2", "Epic #3", "Epic #4", "Epic #5", "Epic #6")
totalFeaturePoints <- c(100, 230, 150, 95, 78, 176)
completedFeaturePoints <- c(50, 170, 94, 34, 40, 125)
initialEstimates <- c(120, 200, 140, 100, 50, 200)
startDates <- c("2018-02-22", "2018-02-22", "2018-04-22", "2018-06-22", "2018-08-22", "2018-09-22")
targetDates <- c("2018-03-22", "2018-05-22", "2018-07-22", "2018-09-22", "2018-11-22", "2018-12-22")
plot4 <- generateEpicProgress(epicNames, startDates, targetDates, totalFeaturePoints, completedFeaturePoints, initialEstimates)

grid.arrange(plot1, plot2, plot3, plot4, ncol = 2)