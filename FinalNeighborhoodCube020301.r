 # Function for search a regular expression in a string, inputs  x is an array of strings, b is the number of strings
 neighborcubefunction <- function(x,b, ...){
 library(stringr)
 seq2=x
 countseq=b
 total=c(0,0,0,0,0,0)
 total
 neigborCube1 <- array(0, dim=c(4,4,6), dimnames = list( c("A","C","G","U"),c("A","C","G","U"),1:6))
 
  for(i in 1:b)
{
   seqtemp= seq2[i]
# level1
neigborCube1["A" ,"A" ,1]=neigborCube1["A" ,"A" ,1]+  str_count(seqtemp,regex("AA"))
neigborCube1["A" ,"C" ,1]=neigborCube1["A" ,"C" ,1]+  str_count(seqtemp,regex("AC"))
neigborCube1["A" ,"G" ,1]=neigborCube1["A" ,"G" ,1]+  str_count(seqtemp,regex("AG"))
neigborCube1["A" ,"U" ,1]=neigborCube1["A" ,"U" ,1]+  str_count(seqtemp,regex("AU"))
#
neigborCube1["C" ,"A" ,1]=neigborCube1["C" ,"A" ,1]+  str_count(seqtemp,regex("CA"))
neigborCube1["C" ,"C" ,1]=neigborCube1["C" ,"C" ,1]+  str_count(seqtemp,regex("CC"))
neigborCube1["C" ,"G" ,1]=neigborCube1["C" ,"G" ,1]+  str_count(seqtemp,regex("CG"))
neigborCube1["C" ,"U" ,1]=neigborCube1["C" ,"U" ,1]+  str_count(seqtemp,regex("CU"))
#
neigborCube1["G" ,"A" ,1]=neigborCube1["G" ,"A" ,1]+  str_count(seqtemp,regex("GA"))
neigborCube1["G" ,"C" ,1]=neigborCube1["G" ,"C" ,1]+  str_count(seqtemp,regex("GC"))
neigborCube1["G" ,"G" ,1]=neigborCube1["G" ,"G" ,1]+  str_count(seqtemp,regex("GG"))
neigborCube1["G" ,"U" ,1]=neigborCube1["G" ,"U" ,1]+  str_count(seqtemp,regex("GU"))
#
neigborCube1["U" ,"A" ,1]=neigborCube1["U" ,"A" ,1]+  str_count(seqtemp,regex("UA"))
neigborCube1["U" ,"C" ,1]=neigborCube1["U" ,"C" ,1]+  str_count(seqtemp,regex("UC"))
neigborCube1["U" ,"G" ,1]=neigborCube1["U" ,"G" ,1]+  str_count(seqtemp,regex("UG"))
neigborCube1["U" ,"U" ,1]=neigborCube1["U" ,"U" ,1]+  str_count(seqtemp,regex("UU"))
# level2
neigborCube1["A" ,"A" ,2]=neigborCube1["A" ,"A" ,2]+  str_count(seqtemp,regex("A[ACGU]A"))
neigborCube1["A" ,"C" ,2]=neigborCube1["A" ,"C" ,2]+  str_count(seqtemp,regex("A[ACGU]C"))
neigborCube1["A" ,"G" ,2]=neigborCube1["A" ,"G" ,2]+  str_count(seqtemp,regex("A[ACGU]G"))
neigborCube1["A" ,"U" ,2]=neigborCube1["A" ,"U" ,2]+  str_count(seqtemp,regex("A[ACGU]U"))
#
neigborCube1["C" ,"A" ,2]=neigborCube1["C" ,"A" ,2]+  str_count(seqtemp,regex("C[ACGU]A"))
neigborCube1["C" ,"C" ,2]=neigborCube1["C" ,"C" ,2]+  str_count(seqtemp,regex("C[ACGU]C"))
neigborCube1["C" ,"G" ,2]=neigborCube1["C" ,"G" ,2]+  str_count(seqtemp,regex("C[ACGU]G"))
neigborCube1["C" ,"U" ,2]=neigborCube1["C" ,"U" ,2]+  str_count(seqtemp,regex("C[ACGU]U"))
#
neigborCube1["G" ,"A" ,2]=neigborCube1["G" ,"A" ,2]+  str_count(seqtemp,regex("G*A"))
neigborCube1["G" ,"C" ,2]=neigborCube1["G" ,"C" ,2]+  str_count(seqtemp,regex("G[ACGU]C"))
neigborCube1["G" ,"G" ,2]=neigborCube1["G" ,"G" ,2]+  str_count(seqtemp,regex("G[ACGU]G"))
neigborCube1["G" ,"U" ,2]=neigborCube1["G" ,"U" ,2]+  str_count(seqtemp,regex("G[ACGU]U"))
#
neigborCube1["U" ,"A" ,2]=neigborCube1["U" ,"A" ,2]+  str_count(seqtemp,regex("U[ACGU]A"))
neigborCube1["U" ,"C" ,2]=neigborCube1["U" ,"C" ,2]+  str_count(seqtemp,regex("U[ACGU]C"))
neigborCube1["U" ,"G" ,2]=neigborCube1["U" ,"G" ,2]+  str_count(seqtemp,regex("U[ACGU]G"))
neigborCube1["U" ,"U" ,2]=neigborCube1["U" ,"U" ,2]+  str_count(seqtemp,regex("U[ACGU]U"))
# level3
neigborCube1["A" ,"A" ,3]=neigborCube1["A" ,"A" ,3]+  str_count(seqtemp,regex("A[ACGU][ACGU]A"))
neigborCube1["A" ,"C" ,3]=neigborCube1["A" ,"C" ,3]+  str_count(seqtemp,regex("A[ACGU][ACGU]C"))
neigborCube1["A" ,"G" ,3]=neigborCube1["A" ,"G" ,3]+  str_count(seqtemp,regex("A[ACGU][ACGU]G"))
neigborCube1["A" ,"U" ,3]=neigborCube1["A" ,"U" ,3]+  str_count(seqtemp,regex("A[ACGU][ACGU]U"))
#
neigborCube1["C" ,"A" ,3]=neigborCube1["C" ,"A" ,3]+  str_count(seqtemp,regex("C[ACGU][ACGU]A"))
neigborCube1["C" ,"C" ,3]=neigborCube1["C" ,"C" ,3]+  str_count(seqtemp,regex("C[ACGU][ACGU]C"))
neigborCube1["C" ,"G" ,3]=neigborCube1["C" ,"G" ,3]+  str_count(seqtemp,regex("C[ACGU][ACGU]G"))
neigborCube1["C" ,"U" ,3]=neigborCube1["C" ,"U" ,3]+  str_count(seqtemp,regex("C[ACGU][ACGU]U"))
#
neigborCube1["G" ,"A" ,3]=neigborCube1["G" ,"A" ,3]+  str_count(seqtemp,regex("G[ACGU][ACGU]A"))
neigborCube1["G" ,"C" ,3]=neigborCube1["G" ,"C" ,3]+  str_count(seqtemp,regex("G[ACGU][ACGU]C"))
neigborCube1["G" ,"G" ,3]=neigborCube1["G" ,"G" ,3]+  str_count(seqtemp,regex("G[ACGU][ACGU]G"))
neigborCube1["G" ,"U" ,3]=neigborCube1["G" ,"U" ,3]+  str_count(seqtemp,regex("G[ACGU][ACGU]U"))
#
neigborCube1["U" ,"A" ,3]=neigborCube1["U" ,"A" ,3]+  str_count(seqtemp,regex("U[ACGU][ACGU]A"))
neigborCube1["U" ,"C" ,3]=neigborCube1["U" ,"C" ,3]+  str_count(seqtemp,regex("U[ACGU][ACGU]C"))
neigborCube1["U" ,"G" ,3]=neigborCube1["U" ,"G" ,3]+  str_count(seqtemp,regex("U[ACGU][ACGU]G"))
neigborCube1["U" ,"U" ,3]=neigborCube1["U" ,"U" ,3]+  str_count(seqtemp,regex("U[ACGU][ACGU]U"))
# level4
neigborCube1["A" ,"A" ,4]=neigborCube1["A" ,"A" ,4]+  str_count(seqtemp,regex("A[ACGU][ACGU][ACGU]A"))
neigborCube1["A" ,"C" ,4]=neigborCube1["A" ,"C" ,4]+  str_count(seqtemp,regex("A[ACGU][ACGU][ACGU]C"))
neigborCube1["A" ,"G" ,4]=neigborCube1["A" ,"G" ,4]+  str_count(seqtemp,regex("A[ACGU][ACGU][ACGU]G"))
neigborCube1["A" ,"U" ,4]=neigborCube1["A" ,"U" ,4]+  str_count(seqtemp,regex("A[ACGU][ACGU][ACGU]U"))
#
neigborCube1["C" ,"A" ,4]=neigborCube1["C" ,"A" ,4]+  str_count(seqtemp,regex("C[ACGU][ACGU][ACGU]A"))
neigborCube1["C" ,"C" ,4]=neigborCube1["C" ,"C" ,4]+  str_count(seqtemp,regex("C[ACGU][ACGU][ACGU]C"))
neigborCube1["C" ,"G" ,4]=neigborCube1["C" ,"G" ,4]+  str_count(seqtemp,regex("C[ACGU][ACGU][ACGU]G"))
neigborCube1["C" ,"U" ,4]=neigborCube1["C" ,"U" ,4]+  str_count(seqtemp,regex("C[ACGU][ACGU][ACGU]U"))
#
neigborCube1["G" ,"A" ,4]=neigborCube1["G" ,"A" ,4]+  str_count(seqtemp,regex("G[ACGU][ACGU][ACGU]A"))
neigborCube1["G" ,"C" ,4]=neigborCube1["G" ,"C" ,4]+  str_count(seqtemp,regex("G[ACGU][ACGU][ACGU]C"))
neigborCube1["G" ,"G" ,4]=neigborCube1["G" ,"G" ,4]+  str_count(seqtemp,regex("G[ACGU][ACGU][ACGU]G"))
neigborCube1["G" ,"U" ,4]=neigborCube1["G" ,"U" ,4]+  str_count(seqtemp,regex("G[ACGU][ACGU][ACGU]U"))
#
neigborCube1["U" ,"A" ,4]=neigborCube1["U" ,"A" ,4]+  str_count(seqtemp,regex("U[ACGU][ACGU][ACGU]A"))
neigborCube1["U" ,"C" ,4]=neigborCube1["U" ,"C" ,4]+  str_count(seqtemp,regex("U[ACGU][ACGU][ACGU]C"))
neigborCube1["U" ,"G" ,4]=neigborCube1["U" ,"G" ,4]+  str_count(seqtemp,regex("U[ACGU][ACGU][ACGU]G"))
neigborCube1["U" ,"U" ,4]=neigborCube1["U" ,"U" ,4]+  str_count(seqtemp,regex("U[ACGU][ACGU][ACGU]U"))
# level5
neigborCube1["A" ,"A" ,5]=neigborCube1["A" ,"A" ,5]+  str_count(seqtemp,regex("A[ACGU][ACGU][ACGU][ACGU]A"))
neigborCube1["A" ,"C" ,5]=neigborCube1["A" ,"C" ,5]+  str_count(seqtemp,regex("A[ACGU][ACGU][ACGU][ACGU]C"))
neigborCube1["A" ,"G" ,5]=neigborCube1["A" ,"G" ,5]+  str_count(seqtemp,regex("A[ACGU][ACGU][ACGU][ACGU]G"))
neigborCube1["A" ,"U" ,5]=neigborCube1["A" ,"U" ,5]+  str_count(seqtemp,regex("A[ACGU][ACGU][ACGU][ACGU]U"))
#
neigborCube1["C" ,"A" ,5]=neigborCube1["C" ,"A" ,5]+  str_count(seqtemp,regex("C[ACGU][ACGU][ACGU][ACGU]A"))
neigborCube1["C" ,"C" ,5]=neigborCube1["C" ,"C" ,5]+  str_count(seqtemp,regex("C[ACGU][ACGU][ACGU][ACGU]C"))
neigborCube1["C" ,"G" ,5]=neigborCube1["C" ,"G" ,5]+  str_count(seqtemp,regex("C[ACGU][ACGU][ACGU][ACGU]G"))
neigborCube1["C" ,"U" ,5]=neigborCube1["C" ,"U" ,5]+  str_count(seqtemp,regex("C[ACGU][ACGU][ACGU][ACGU]U"))
#
neigborCube1["G" ,"A" ,5]=neigborCube1["G" ,"A" ,5]+  str_count(seqtemp,regex("G[ACGU][ACGU][ACGU][ACGU]A"))
neigborCube1["G" ,"C" ,5]=neigborCube1["G" ,"C" ,5]+  str_count(seqtemp,regex("G[ACGU][ACGU][ACGU][ACGU]C"))
neigborCube1["G" ,"G" ,5]=neigborCube1["G" ,"G" ,5]+  str_count(seqtemp,regex("G[ACGU][ACGU][ACGU][ACGU]G"))
neigborCube1["G" ,"U" ,5]=neigborCube1["G" ,"U" ,5]+  str_count(seqtemp,regex("G[ACGU][ACGU][ACGU][ACGU]U"))
#
neigborCube1["U" ,"A" ,5]=neigborCube1["U" ,"A" ,5]+  str_count(seqtemp,regex("U[ACGU][ACGU][ACGU][ACGU]A"))
neigborCube1["U" ,"C" ,5]=neigborCube1["U" ,"C" ,5]+  str_count(seqtemp,regex("U[ACGU][ACGU][ACGU][ACGU]C"))
neigborCube1["U" ,"G" ,5]=neigborCube1["U" ,"G" ,5]+  str_count(seqtemp,regex("U[ACGU][ACGU][ACGU][ACGU]G"))
neigborCube1["U" ,"U" ,5]=neigborCube1["U" ,"U" ,5]+  str_count(seqtemp,regex("U[ACGU][ACGU][ACGU][ACGU]U"))
# level6
neigborCube1["A" ,"A" ,6]=neigborCube1["A" ,"A" ,6]+  str_count(seqtemp,regex("A[ACGU][ACGU][ACGU][ACGU][ACGU]A"))
neigborCube1["A" ,"C" ,6]=neigborCube1["A" ,"C" ,6]+  str_count(seqtemp,regex("A[ACGU][ACGU][ACGU][ACGU][ACGU]C"))
neigborCube1["A" ,"G" ,6]=neigborCube1["A" ,"G" ,6]+  str_count(seqtemp,regex("A[ACGU][ACGU][ACGU][ACGU][ACGU]G"))
neigborCube1["A" ,"U" ,6]=neigborCube1["A" ,"U" ,6]+  str_count(seqtemp,regex("A[ACGU][ACGU][ACGU][ACGU][ACGU]U"))
#
neigborCube1["C" ,"A" ,6]=neigborCube1["C" ,"A" ,6]+  str_count(seqtemp,regex("C[ACGU][ACGU][ACGU][ACGU][ACGU]A"))
neigborCube1["C" ,"C" ,6]=neigborCube1["C" ,"C" ,6]+  str_count(seqtemp,regex("C[ACGU][ACGU][ACGU][ACGU][ACGU]C"))
neigborCube1["C" ,"G" ,6]=neigborCube1["C" ,"G" ,6]+  str_count(seqtemp,regex("C[ACGU][ACGU][ACGU][ACGU][ACGU]G"))
neigborCube1["C" ,"U" ,6]=neigborCube1["C" ,"U" ,6]+  str_count(seqtemp,regex("C[ACGU][ACGU][ACGU][ACGU][ACGU]U"))
#
neigborCube1["G" ,"A" ,6]=neigborCube1["G" ,"A" ,6]+  str_count(seqtemp,regex("G[ACGU][ACGU][ACGU][ACGU][ACGU]A"))
neigborCube1["G" ,"C" ,6]=neigborCube1["G" ,"C" ,6]+  str_count(seqtemp,regex("G[ACGU][ACGU][ACGU][ACGU][ACGU]C"))
neigborCube1["G" ,"G" ,6]=neigborCube1["G" ,"G" ,6]+  str_count(seqtemp,regex("G[ACGU][ACGU][ACGU][ACGU][ACGU]G"))
neigborCube1["G" ,"U" ,6]=neigborCube1["G" ,"U" ,6]+  str_count(seqtemp,regex("G[ACGU][ACGU][ACGU][ACGU][ACGU]U"))
#
neigborCube1["U" ,"A" ,6]=neigborCube1["U" ,"A" ,6]+  str_count(seqtemp,regex("U[ACGU][ACGU][ACGU][ACGU][ACGU]A"))
neigborCube1["U" ,"C" ,6]=neigborCube1["U" ,"C" ,6]+  str_count(seqtemp,regex("U[ACGU][ACGU][ACGU][ACGU][ACGU]C"))
neigborCube1["U" ,"G" ,6]=neigborCube1["U" ,"G" ,6]+  str_count(seqtemp,regex("U[ACGU][ACGU][ACGU][ACGU][ACGU]G"))
neigborCube1["U" ,"U" ,6]=neigborCube1["U" ,"U" ,6]+  str_count(seqtemp,regex("U[ACGU][ACGU][ACGU][ACGU][ACGU]U"))
seqtemp=""

#total=str_count(seq2[1],regex("AU"))
}
total=c(0,0,0,0,0,0)
  for(i in 1:b)
{     

    total[1]= total[1]+str_count(sequ[i],regex("A[ACGU]"))+str_count(sequ[i],regex("C[ACGU]"))+str_count(sequ[i],regex("G[ACGU]"))+str_count(sequ[i],regex("U[ACGU]"))
    total[2]= total[2]+str_count(sequ[i],regex("A[ACGU][ACGU]"))+str_count(sequ[i],regex("C[ACGU][ACGU]"))+str_count(sequ[i],regex("G[ACGU][ACGU]"))+str_count(sequ[i],regex("U[ACGU][ACGU]"))
    total[3]= total[3]+str_count(sequ[i],regex("A[ACGU][ACGU][ACGU]"))+str_count(sequ[i],regex("C[ACGU][ACGU][ACGU]"))+str_count(sequ[i],regex("G[ACGU][ACGU][ACGU]"))+str_count(sequ[i],regex("U[ACGU][ACGU][ACGU]"))
    total[4]= total[4]+str_count(sequ[i],regex("A[ACGU][ACGU][ACGU][ACGU]"))+str_count(sequ[i],regex("C[ACGU][ACGU][ACGU][ACGU]"))+str_count(sequ[i],regex("G[ACGU][ACGU][ACGU][ACGU]"))+str_count(sequ[i],regex("U[ACGU][ACGU][ACGU][ACGU]"))
    total[5]= total[5]+str_count(sequ[i],regex("A[ACGU][ACGU][ACGU][ACGU][ACGU]"))+str_count(sequ[i],regex("C[ACGU][ACGU][ACGU][ACGU][ACGU]"))+str_count(sequ[i],regex("G[ACGU][ACGU][ACGU][ACGU][ACGU]"))+str_count(sequ[i],regex("U[ACGU][ACGU][ACGU][ACGU][ACGU]"))
    total[6]= total[6]+str_count(sequ[i],regex("A[ACGU][ACGU][ACGU][ACGU][ACGU][ACGU]"))+str_count(sequ[i],regex("C[ACGU][ACGU][ACGU][ACGU][ACGU][ACGU]"))+str_count(sequ[i],regex("G[ACGU][ACGU][ACGU]v[ACGU][ACGU]"))+str_count(sequ[i],regex("U[ACGU][ACGU][ACGU][ACGU][ACGU][ACGU]"))
}
  for(i in 1:6)
{  
neigborCube1[ , ,i]=neigborCube1[ , ,i]/(total[i])   
}

 return(neigborCube1)
  }
 # Call function, sequ is the array of strings, b is the number of strings
  neigborCubeResult=neighborcubefunction(sequ,b)
   neigborCubeResult
 
