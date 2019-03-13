#### Library Load, File Load, and WD ####
library(readr)
library(dplyr)
library(ggplot)
setwd("REDACTED")

REDACTED <- read_csv("REDACTED", 
                col_types = cols(Date = col_date(format = "%m/%d/%Y")))
colnames(REDACTED) <- c("DATE", "REDACTED","REDACTED")
REDACTED <- as.data.frame(REDACTED)

#### Base function for rebalancing portfolio timing ####
# Create function that takes in the dataframe, specifies a number of days
# for rebalancing and returns the compounded annual growth rate

daysInvest <- function(df, days, invest) {
  df[,paste("REDACTED",as.character(days))] <- 0
  df[,4] <- append(rep(0, days),df[1:(nrow(df)-days),"REDACTED"])
  df$UReturn <- (df[,4] - df$REDACTED) / df[,4]
  df[,paste("REDACTED",as.character(days))] <- 0
  df[,6] <- append(rep(0, days),df[1:(nrow(df)-days),"REDACTED"])
  df$DReturn <- (df[,6] - df$REDACTED) / df[,6]
  out <- na.omit(df[days+1:nrow(df),])
  out$TotReturn <- sapply(out$DReturn, function(x) max(x,-1)) + sapply(out$UReturn, function(x) max(x,-1))
  row.names(out) <- NULL
  i <- 1
  investNew <- invest
  while(i <= nrow(out)) {
    investNew <- (1 + out[i,"TotReturn"]) * investNew
    i <- i + days
  }
  cagr <- (investNew / invest) ^ (1/(as.numeric(max(df$DATE)-min(df$DATE))/365)) - 1
  return(cagr)
}

#### Simulation ####
# Simulate returns with different rebalancing windows

# Create empty df
return_df <- data.frame(start_date = as.Date(as.character()), days = numeric(), cagr = numeric())
# Simulate data at 15 day itnervals
for (j in seq(1,nrow(REDACTED),15)) {
  # if there is less than two years of training data, end
  if (nrow(REDACTED)-j < 720) {
    break
  }
  df1 <- REDACTED[j:(j+720),]
  # Simulate rebalancing timings between 10 and 100 days at 10 day intervals
  for (i in seq(10,100,10)) {
    cagr_round <- daysInvest(df = df1, days = i, invest = 1)
    return_df <- rbind(return_one_df, data.frame(start_date = REDACTED[j,"DATE"],days = i, cagr = cagr_round))
  }
}


ggplot(return_df, aes(x = days, y = cagr)) + 
  xlab("Rebalancing Timing") +
  ylab("CAGR") +
  ggtitle("Simulated CAGR Investing Two Years") +
  scale_y_continuous(labels = scales::percent, limits = c(-1,1)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_boxplot(aes(group = days))
ggsave("Simulated CAGR Boxplot.jpeg", device = "jpeg")


ggplot(return_df, aes(x = days, y = cagr)) + 
  xlab("Rebalancing Timing") +
  ylab("CAGR") +
  ggtitle("Simulated CAGR Investing Three Years") +
  scale_y_continuous(labels = scales::percent, limits = c(-1,1)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_box(aes(color=as.factor(start_date)))
ggsave("Simulated CAGR.jpeg", device = "jpeg")

return_df %>%
  group_by(days) %>%
  summarize(avg = mean(cagr), 
            firstq = quantile(cagr, probs = .25),
            secondq = quantile(cagr, probs = .5),
            thirdq = quantile(cagr, probs = .75))
