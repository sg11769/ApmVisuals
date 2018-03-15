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

#Feature Progress Visual
generateFeatureProgress <- function(featureName, completedPoints, totalPoints) {
    df <- data.frame(featureName, completedPoints, totalPoints)
    df$percentComplete <- completedPoints / totalPoints
    df$remainingPoints <- totalPoints - completedPoints

    ggplot(df, aes(x = featureName, y = totalPoints, color = featureName)) +
      geom_bar(aes(y = totalPoints, fill = featureName), stat = "identity") +
      geom_text(aes(label = totalPoints), col = "black", hjust = 1.25) +
      geom_bar(aes(y = completedPoints), fill = "purple", stat = "identity") +
      geom_text(aes(label = completedPoints), col = "green", hjust = 1.25, vjust = -1.15) +
      coord_flip() +
      ggtitle("Feature Progress Report") +
      xlab("Feature Name") +
      ylab("Effort (in Story Points)") +
      theme(legend.position = "none")
}

#Program Predictability Measure
generateProgramPredictability <- function(teamName, incrementName, actualObjectivesAchieved, targetObjectivesPlanned) {

    df <- data.frame(teamName, incrementName, actualObjectivesAchieved, targetObjectivesPlanned)
    df$percentObjectivesAchieved <- actualObjectivesAchieved / targetObjectivesPlanned

    ggplot(df, aes(x = incrementName, y = percentObjectivesAchieved, group = teamName)) +
      geom_point(aes(y = percentObjectivesAchieved, col = teamName)) +
      geom_line(aes(col = teamName), linetype = 5) +
      geom_line(aes(y = mean(percentObjectivesAchieved))) +
      ggtitle("Program Predictability Measure") +
      xlab("Program Increment") +
      ylab("Objective Completion Rate (%)")
}

#PI Burn-Down Chart
generatePiBurnDown <- function(incrementName, remainingStories, idealRemainingStories) {

    df <- data.frame(incrementName, remainingStories, idealRemainingStories)

    ggplot(df, aes(x = incrementName, y = remainingStories)) +
      geom_point(col = "red") +
      geom_point(aes(y = idealRemainingStories)) +
      ggtitle("Program Increment Burn Down") +
      xlab("Program Increment") +
      ylab("User Story Count")
}