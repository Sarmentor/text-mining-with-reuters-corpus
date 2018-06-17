 plot(colnames(resultados),resultados[1,],type="l", col="red", main= "Classifiers Error Rate", xlab="sparse value", ylab="")
 par(new=T, xaxs="s" , yaxs="s")
 plot(colnames(resultados),resultados[2,],type="l", col="green" , xlab="", ylab="")
 par(new=T)
 plot(colnames(resultados),resultados[3,],type="l", col="blue", xlab="", ylab="")
 par(new=T)
 plot(colnames(resultados),resultados[4,],type="l", col="brown", xlab="", ylab="")
 par(new=T)
 plot(colnames(resultados),resultados[5,],type="l", col="black", xlab="", ylab="")
 
 windows()
 
split.screen(c(2,1))        # split display into two screens
split.screen(c(1,3), screen = 1)
split.screen(c(1,2), screen = 2) # now split the bottom half into 2

# - Set the number of figures in terms of rows and columns.  
#   - 2 rows, 2 columns, for 4 total figures.  
par(mfcol=c(2,3));  
  
# - Set the margins for a single figure. Note that this parameter can  
#   be set on a per-figure basis since each figure can have unique margins.  
# - If you do not redefine the mar parameter after this, all figures will  
#   use this setting.  
par(mar=c(5,4,4,2));  
  
# - Set the outer margin area. Note that this parameter applies for the entire  
#   figure and altering it while drawing figures may cause problems.  
par(oma=c(3,3,3,3));  
  

#screen(1) # prepare screen 1 for output
 plot(colnames(resultados_f1),resultados_f1[1,],type="b", col="red", main= "Classifier DT f1 result", xlab="sparse value", ylab="")
 #par(new=T, yaxt="n")
 #screen(2) # prepare screen 1 for output
 plot(colnames(resultados_f1),resultados_f1[2,],type="b", col="green" ,main= "Classifier KNN f1 result", xlab="sparse value", ylab="")
 #par(new=T, yaxt="n")
 #screen(3) # prepare screen 1 for output
 plot(colnames(resultados_f1),resultados_f1[3,],type="l", col="blue", main= "Classifier NN f1 result", xlab="sparse value", ylab="")
 #par(new=T, yaxt="n")
 #screen(4) # prepare screen 1 for output
 plot(colnames(resultados_f1),resultados_f1[4,],type="o", col="brown", main= "Classifier NB f1 result", xlab="sparse value", ylab="")
 #par(new=T, yaxt="n")
 #screen(5) # prepare screen 1 for output
 plot(colnames(resultados_f1),resultados_f1[5,],type="s", col="black", main= "Classifier SVM f1 result", xlab="sparse value", ylab="")
 mtext("Classifiers f1 score", side=3, line=1, cex=2, col="blue", outer=TRUE)  
 box("outer", col="blue")
 
 plot(colnames(resultados),resultados[1,],type="b", col="red", main= "Classifiers error result", xlab="sparse value", ylab="")
 par(new=T, yaxs="i")
 #screen(2) # prepare screen 1 for output
 plot(colnames(resultados),resultados[2,],type="b", col="green" , xlab="", ylab="")
 par(new=T, yaxt="n")
 #screen(3) # prepare screen 1 for output
 plot(colnames(resultados),resultados[3,],type="l", col="blue", xlab="", ylab="")
 par(new=T, yaxt="n")
 #screen(4) # prepare screen 1 for output
 plot(colnames(resultados),resultados[4,],type="o", col="brown", xlab="", ylab="")
 par(new=T, yaxt="n")
 #screen(5) # prepare screen 1 for output
 plot(colnames(resultados),resultados[5,],type="s", col="black", xlab="", ylab="")
 
 plot(colnames(resultados_precision),resultados_precision[1,],type="b", col="red", main= "Classifiers precision result", xlab="sparse value", ylab="")
 par(new=T, yaxs="i")
 #screen(2) # prepare screen 1 for output
 plot(colnames(resultados_precision),resultados_precision[2,],type="b", col="green" , xlab="", ylab="")
 par(new=T, yaxt="n")
 #screen(3) # prepare screen 1 for output
 plot(colnames(resultados_precision),resultados_precision[3,],type="l", col="blue", xlab="", ylab="")
 par(new=T, yaxt="n")
 #screen(4) # prepare screen 1 for output
 plot(colnames(resultados_precision),resultados_precision[4,],type="o", col="brown", xlab="", ylab="")
 par(new=T, yaxt="n")
 #screen(5) # prepare screen 1 for output
 plot(colnames(resultados_precision),resultados_precision[5,],type="s", col="black", xlab="", ylab="")
 
 plot(colnames(resultados_recall),resultados_recall[1,],type="b", col="red", main= "Classifiers recall result", xlab="sparse value", ylab="")
 par(new=T, yaxs="i")
 #screen(2) # prepare screen 1 for output
 plot(colnames(resultados_recall),resultados_recall[2,],type="b", col="green" , xlab="", ylab="")
 par(new=T, yaxt="n")
 #screen(3) # prepare screen 1 for output
 plot(colnames(resultados_recall),resultados_recall[3,],type="l", col="blue", xlab="", ylab="")
 par(new=T, yaxt="n")
 #screen(4) # prepare screen 1 for output
 plot(colnames(resultados_recall),resultados_recall[4,],type="o", col="brown", xlab="", ylab="")
 par(new=T, yaxt="n")
 #screen(5) # prepare screen 1 for output
 plot(colnames(resultados_recall),resultados_recall[5,],type="s", col="black", xlab="", ylab="")