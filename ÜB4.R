#1)
#a)
#95% Intervalle von 100 Stichproben SNV 
t975 = qt(p=0.975,df=4)
konfi = array(dim=c(100,2))
for (i in 1:100){
  rdm = rnorm(5)
  my = mean(rdm)
  sig = sd(rdm)
  konfi[i,1] = my - (sig*t975)/sqrt(5)
  konfi[i,2] = my + (sig*t975)/sqrt(5)
}
print(konfi[1:10,])
#---------------------------------------------------------------------------------------------------------------------
#b)
#Relative Häufigkeit wahrer MW in Konfidenzintervall
count = 0
for (i in 1:100){
  if (konfi[i,1]<= 0 & konfi[i,2]>=0){
    count = count +1
  }
}
rel_haeufig = count/100
print(rel_haeufig)
#---------------------------------------------------------------------------------------------------------------------
#c)
#Plotte Konfidenzintervalle, farbliche Markierung wahrer MW enthalten
interval.plot = function(konfi){
  n = length(konfi)/2
  plot(c(1,n), c(-3,3), type="n",
       xlab = "", ylab = "Konfidenzintervall", main ="Konfidenzintervalle")
  curve(x*0,col = "blue",add=TRUE)
  count = 0
  for(i in 1:n){
    if (konfi[i,1]<= 0 & konfi[i,2]>=0){
      colour = "green"
      count= count + 1
    }
    segments(x0=i, x1=i, y0=konfi[i,1], y1=konfi[i,2]
             ,col = colour,lwd = 1)
    colour = "red"
  }
  return(count/n)
}

interval.plot(konfi)
