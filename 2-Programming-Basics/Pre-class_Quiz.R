#What does this code do? 
ggplot(data = hydro, aes(x = datetime, y = discharge))+
  geom_line()+
  theme_classic()
#Creates line graph of discharge over time in a classic format 

#Find 3 errors in this code
ggplot(data = mydata, aes(x = discharge, y = nitrate))+
      geom_point() +
      ylab("Nitrate")+
      xlab("Q (L/s)")+
      theme_classic()
      
#canvas quiz: https://canvas.vt.edu/courses/126451/assignments/1131133