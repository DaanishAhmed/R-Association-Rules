# DATA 630 Assignment 1
# Written by Daanish Ahmed
# Semester Summer 2017
# June 6, 2017
# Professor Edward Herranz

# This R script conducts an association rules analysis on a dataset containing 
# statistical information on burn victims.  The purpose of this assignment is to 
# generate a list of association rules and analyze the results to gain useful 
# insights.  This script consists of several components, including opening and 
# initializing the data, exploration and preprocessing, implementing the Apriori 
# algorithm, pruning redundant rules, and the commands for analyzing the results.



# This section of code covers opening the dataset and initializing the packages 
# that are used in this script.

# Sets the working directory for this assignment.  Please change this directory 
# to whichever directory you are using, and make sure that all files are placed 
# in that location.
setwd("~/Class Documents/2016-17 Summer/DATA 630/R/Assignment 1")

# In order to run the discretization commands, we need to use the arules and 
# arulesViz packages.

# If you have not installed these packages yet, remove the two # symbols below.
# install.packages("arules")
# install.packages("arulesViz")

# Loads the arules and arulesViz packages into the system.
library("arules")
library("arulesViz")

# Opens the CSV file "burn_edit.csv".
burn <- read.csv(file="burn_edit.csv", head=TRUE, sep=",")

# End of opening the dataset.



# This section of code covers data preprocessing.  It includes exploration of 
# the original dataset, removing unique identifiers, dealing with missing 
# values, and discretization of numeric variables.

# Previews the burn dataset.
View(burn)

# Shows the descriptive statistics for all variables in the dataset.
summary(burn)

# Displays the structure of the burn data.  This is necessary to see if there 
# are any unique identifiers (IDs) that can be removed.  Such variables are not 
# useful for the analysis and should be removed.
str(burn)

# The first variable is an ID, and we remove it.
burn <- burn[, -1]

# Verifies that the ID variable has been removed.
str(burn)

# This function checks to see how many missing values are in each variable.
apply(burn, 2, function(credit) sum(is.na(credit)))

# Since there are no missing values in any of the variables, we do not need to 
# replace any values.

# We need to discretize all of the variables that are not already factors.  This 
# is required before running the Apriori rules method.
burn$FACILITY <- discretize(burn$FACILITY, "frequency", categories=6)
burn$AGE <- discretize(burn$AGE, "frequency", categories=6)
burn$TBSA <- discretize(burn$TBSA, "frequency", categories=6)

# Verifies that the facility, age, and TBSA variables have been successfully 
# converted to factors.
summary(burn$FACILITY)
summary(burn$AGE)
summary(burn$TBSA)

# End of data preprocessing.



# This section of code covers the implementation of the Apriori algorithm. 
# This method will create a list of decision rules using the data provided in 
# the burn dataset.  Afterwards, redundant rules will be removed from the list.

# This generates a list of rules using the following parameters: 0.15 support, 
# 0.85 confidence, and minimum length of 2.
rules <- apriori(burn, parameter= list(supp=0.15, conf=0.85, minlen=2))

# Creates a sorted list that sorts the rules by lift in descending order, and 
# displays the top 10 rules in the list.
rules.sorted <- sort(rules, by="lift")
inspect(rules.sorted[1:10])

# This code looks for redundant rules in the current sorted list of rules and 
# stores each redundant rule in an array.
subset.matrix <- is.subset(rules.sorted, rules.sorted)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- F
redundant <- colSums(subset.matrix, na.rm=T) >= 1

# Returns each redundant rule and its corresponding index.
which(redundant)

# This code uses the array of redundant rules and creates a new rules list 
# that excludes the redundant rules.
rules.pruned <- rules.sorted[!redundant]

# Shows the descriptive statistics for the new list of rules, including the 
# number of rules, length of rules, and statistical information for support, 
# confidence, and lift.
summary(rules.pruned)

# Examines the top 10 rules in our pruned rules list.
inspect(rules.pruned[1:10])

# End of Apriori algorithm implementation.



# The next few sections of code include all commands that were used in the 
# results analysis portion of the assignment.  These include visualizations 
# and generating additional lists of rules for specific itemsets.

# Creates a scatterplot of the top 10 pruned rules with the support on the 
# x-axis, confidence on the y-axis, and lift as color-coded dots.
plot(rules.pruned[1:10])

# End of analyzing initial pruned rules.  The next sections discuss additional
# rules for specific itemsets.



# This code finds the association rules that result in victim's deaths.  The 
# original list did not have any rules with the condition {DEATH=dead} on the 
# RHS, thus I created a new rules list using the same procedure as before.

# This command creates a new rules list containing {DEATH=dead} on the RHS.
# Since dead victims rarely occur in the dataset, it is necessary to set a 
# lower minimum support threshold.  Likewise, I set a lower confidence value.
rules_dead <- apriori(burn, parameter= list(supp=0.05, conf=0.5, minlen=2), 
                      appearance=list(rhs=c("DEATH=dead"), default="lhs"))

# Sorts the list by lift in descending order.
rules_dead <- sort(rules_dead, by="lift")

# Creates an array to store all redundant rules.
subset.matrix <- is.subset(rules_dead, rules_dead)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- F
redundant <- colSums(subset.matrix, na.rm=T) >= 1

# Creates a new rules list that excludes the redundant rules.
rules_dead.p <- rules_dead[!redundant]

# Displays all rules in the new rules list.
inspect(rules_dead.p)

# Creates a grouped plot for this rules list.  The size of each circle 
# represents the support value, while the color indicates the lift value.
plot(rules_dead.p, method = "grouped")

# End of analyzing victim death cases.



# This code finds the association rules which involve inhalation injuries.
# This process once again involves creating a new list, but this time 
# {INH_INJ=yes} is on the LHS to show what inhalation injuries cause.

# This command creates a new rules list containing {INH_INJ=yes} on the LHS.
# Due to the low frequency of inhalation injuries, it is necessary to set 
# lower thresholds for support and confidence.
rules_inh <- apriori(burn, parameter= list(supp=0.05, conf=0.5, minlen=2), 
                      appearance=list(lhs=c("INH_INJ=yes"), default="rhs"))

# Sorts the list by lift in descending order.
rules_inh <- sort(rules_inh, by="lift")

# Creates an array to store all redundant rules.
subset.matrix <- is.subset(rules_inh, rules_inh)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- F
redundant <- colSums(subset.matrix, na.rm=T) >= 1

# Creates a new rules list that excludes the redundant rules.
rules_inh.p <- rules_inh[!redundant]

# Displays all rules in the new rules list.
inspect(rules_inh.p)

# Creates a circle graph for this rules list.  The size of each circle 
# represents the support value, while the color indicates the lift value.
plot(rules_inh.p, method="graph", control=list(type="items"))

# End of analyzing inhalation injuries.



# End of results analysis.

# End of script.

