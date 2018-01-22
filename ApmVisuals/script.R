#Functions to produce visuals aiding in Portfolio Project Selection and Justification
install.packages("ggplot2")
install.packages("gridExtra")
library(ggplot2)
library(gridExtra)

#Epic Burn-Up Chart Visual
generateEpicBurnUp <- function(iterationName, initialEstimate, actualStoryPoints, cumulativeStoryPoints = cumsum(actualStoryPoints)) {

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