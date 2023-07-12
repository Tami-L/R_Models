 #### Markov Chain Analysis
   library(markovchain)
 library(diagram)
# define a transition matrix
  P1 <- matrix(c(0.645, 0.099, 0.152, 0.033, 0.071,0.611, 0.138, 0.128, 0.033,
                   + 0.090,0.514, 0.067, 0.271, 0.030, 0.118,0.609, 0.107, 0.178, 0.064,
                   + 0.042,0.523, 0.093, 0.183, 0.022, 0.179),nrow = 5, byrow = TRUE)
  # create the DTMC
    P <- new("markovchain",transitionMatrix=P1, states=c("Nonindex", "Injury", "Theft", "Damage", "Combination"), name="MarkovChain A")
     P
    #### Visualization by plotmat function from diagram package
       plot(P)
     ###### Markov Chain
      stateNames <- c("Nonindex", "Injury", "Theft", "Damage", "Combination")
      row.names(P1) <- stateNames; colnames(P1) <- stateNames
       plotmat(P1,pos = c(1,4),
                + lwd = 1, box.lwd = 2,
                + cex.txt = 0.8,
                + box.size = 0.1,
                + box.type = "circle",
                + box.prop = 0.5,
                + box.col = "light blue",
                + arr.length=.1,
                + arr.width=.1,
                + self.cex = .6,
                + self.shifty = -.01,
                + self.shiftx = .14,
                + main = "Markov Chain")
      
       #### Create the most essential transition matrix
        myFit<-markovchainFit(data=Rain,confidencelevel=.9,method = "mle")
         Prain<-myFit$estimate
        Prainfall=Prain@transitionMatrix
         P=round(Prainfall,4)
        P