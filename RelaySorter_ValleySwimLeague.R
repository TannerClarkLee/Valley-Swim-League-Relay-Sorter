#The following will be listed as (availability, relaycount,sex,Back, Breast, Fly, Free, medley count, free count)
#1 means they will be there

A1 = read.csv(file="swimmernames.csv", header=TRUE, sep=",")


nums=nrow(A)
medleysumA_post=10000;
freesumA_post=10000;
medleysumB_post=10000;
freesumB_post=10000;
medleysumC_post=10000;
freesumC_post=10000;
gradrelayMA=100;
gradrelayFA=100;
gradrelayMB=100;
gradrelayFB=100;
gradrelayMC=100;
gradrelayFC=100;
gradfinalMA=nums;
gradfinalFA=nums;
gradfinalMB=nums;
gradfinalFB=nums;
gradfinalMC=nums;
gradfinalFC=nums;
medleyfinalC=c(nums,nums,nums,nums)
freefinalC=c(nums,nums,nums,nums)

  

swimmernames=as.matrix(A1[1])


A=as.matrix(A1[2:10])
rownames(A)=swimmernames

#________________________________________________________________
#                           Medley Relay A
#________________________________________________________________

for (i in 1:nums){
  for (k in 1:nums){
    for (j in 1:nums){
      for (l in 1:nums){
        medleysumA_pre=A[i,4]+A[k,5]+A[j,6]+A[l,7]
        if (medleysumA_pre < medleysumA_post){                                                      #Speed
          if (sum(A[i,1],A[k,1],A[j,1],A[l,1])==4){                                               #Availbility
            if (A[i,2]!=2&A[k,2]!=2&A[j,2]!=2&A[l,2]!=2){                                          #Relay Count
              if (sum(A[i,8],A[k,8],A[j,8],A[l,8])==0){                                            #Medley Count
              if (sum(A[i,3],A[k,3],A[j,3],A[l,3])!=4&&sum(A[i,3],A[k,3],A[j,3],A[l,3])!=0){      #Sex Check
               if (i!=k&i!=j&i!=l&k!=j&k!=l&j!=l){                                                #Double Count
          
                      medleysumA_post=medleysumA_pre;
                      medleyfinalA=c(i,k,j,l)
               }
              }
            }
            }
          }
        }
      }
    }
  }
}
A[medleyfinalA[1],2]=A[medleyfinalA[1],2]+1
A[medleyfinalA[2],2]=A[medleyfinalA[2],2]+1
A[medleyfinalA[3],2]=A[medleyfinalA[3],2]+1
A[medleyfinalA[4],2]=A[medleyfinalA[4],2]+1

A[medleyfinalA[1],8]=A[medleyfinalA[1],8]+1
A[medleyfinalA[2],8]=A[medleyfinalA[2],8]+1
A[medleyfinalA[3],8]=A[medleyfinalA[3],8]+1
A[medleyfinalA[4],8]=A[medleyfinalA[4],8]+1

medleyfinalA=swimmernames[medleyfinalA]


#________________________________________________________________
#________________________________________________________________
#________________________________________________________________
#                           Free Relay A
#________________________________________________________________

for (i in 1:nums){
  for (k in 1:nums){
    for (j in 1:nums){
      for (l in 1:nums){
        freesumA_pre=A[i,7]+A[k,7]+A[j,7]+A[l,7]
        if (freesumA_pre < freesumA_post){                                                          #Speed
          if (sum(A[i,1],A[k,1],A[j,1],A[l,1])==4){                                               #Availbility
            if (A[i,2]!=2&A[k,2]!=2&A[j,2]!=2&A[l,2]!=2){                                          #Relay Count
              if (sum(A[i,9],A[k,9],A[j,9],A[l,9])==0){                                            #Free Count
              if (sum(A[i,3],A[k,3],A[j,3],A[l,3])!=4&&sum(A[i,3],A[k,3],A[j,3],A[l,3])!=0){      #Sex Check
                if (i!=k&i!=j&i!=l&k!=j&k!=l&j!=l){                                               #Double Count
                  
                  freesumA_post=freesumA_pre;
                  freefinalA=c(i,k,j,l)
                }
                }
              }
            }
          }
        }
      }
    }
  }
}
A[freefinalA[1],2]=A[freefinalA[1],2]+1
A[freefinalA[2],2]=A[freefinalA[2],2]+1
A[freefinalA[3],2]=A[freefinalA[3],2]+1
A[freefinalA[4],2]=A[freefinalA[4],2]+1

A[freefinalA[1],9]=A[freefinalA[1],9]+1
A[freefinalA[2],9]=A[freefinalA[2],9]+1
A[freefinalA[3],9]=A[freefinalA[3],9]+1
A[freefinalA[4],9]=A[freefinalA[4],9]+1
freefinalA=swimmernames[freefinalA]
#________________________________________________________________
#________________________________________________________________
#________________________________________________________________
#                           Grad Relay A Male & Female
#________________________________________________________________

for (i in 1:nums){
  if (gradrelayMA>A[i,7]){
    if (A[i,3]==0){
      if(A[i,2]!=2){
        if(A[i,1]==1){
                                                       
  
    gradrelayMA=A[i,7];
    gradfinalMA=i
          }
        
      }
    }
  }
  
    if (gradrelayFA>A[i,7]){
    if (A[i,3]==1){
      if(A[i,2]!=2){
        if(A[i,1]==1){
      
      gradrelayFA=A[i,7];
      gradfinalFA=i
        }
      }
    }
    }
  
}
A[gradfinalMA,2]=A[gradfinalMA,2]+1
A[gradfinalFA,2]=A[gradfinalFA,2]+1
gradfinalMA=c(swimmernames[gradfinalMA],'na','na','na')
gradfinalFA=c(swimmernames[gradfinalFA],'na','na','na')
#________________________________________________________________
#________________________________________________________________
#________________________________________________________________
#                           Medley Relay B
#________________________________________________________________

for (i in 1:nums){
  for (k in 1:nums){
    for (j in 1:nums){
      for (l in 1:nums){
        medleysumB_pre=A[i,4]+A[k,5]+A[j,6]+A[l,7]
        if (medleysumB_pre < medleysumB_post){                                                      #Speed
          if (sum(A[i,1],A[k,1],A[j,1],A[l,1])==4){                                               #Availbility
            if (A[i,2]!=2&A[k,2]!=2&A[j,2]!=2&A[l,2]!=2){                                          #Relay Count
              if (sum(A[i,8],A[k,8],A[j,8],A[l,8])==0){                                            #Medley Count
              if (sum(A[i,3],A[k,3],A[j,3],A[l,3])!=4&&sum(A[i,3],A[k,3],A[j,3],A[l,3])!=0){      #Sex Check
                if (i!=k&i!=j&i!=l&k!=j&k!=l&j!=l){                                                #Double Count
                  
                  medleysumB_post=medleysumB_pre;
                  medleyfinalB=c(i,k,j,l)
                }
                }
              }
            }
          }
        }
      }
    }
  }
}
A[medleyfinalB[1],2]=A[medleyfinalB[1],2]+1
A[medleyfinalB[2],2]=A[medleyfinalB[2],2]+1
A[medleyfinalB[3],2]=A[medleyfinalB[3],2]+1
A[medleyfinalB[4],2]=A[medleyfinalB[4],2]+1

A[medleyfinalB[1],8]=A[medleyfinalB[1],8]+1
A[medleyfinalB[2],8]=A[medleyfinalB[2],8]+1
A[medleyfinalB[3],8]=A[medleyfinalB[3],8]+1
A[medleyfinalB[4],8]=A[medleyfinalB[4],8]+1

medleyfinalB=swimmernames[medleyfinalB]


#________________________________________________________________
#________________________________________________________________
#________________________________________________________________
#                           Free Relay B
#________________________________________________________________

for (i in 1:nums){
  for (k in 1:nums){
    for (j in 1:nums){
      for (l in 1:nums){
        freesumB_pre=A[i,7]+A[k,7]+A[j,7]+A[l,7]
        if (freesumB_pre < freesumB_post){                                                          #Speed
          if (sum(A[i,1],A[k,1],A[j,1],A[l,1])==4){                                               #Availbility
            if (A[i,2]!=2&A[k,2]!=2&A[j,2]!=2&A[l,2]!=2){                                          #Relay Count
              if (sum(A[i,9],A[k,9],A[j,9],A[l,9])==0){                                            #Free Count
              if (sum(A[i,3],A[k,3],A[j,3],A[l,3])!=4&&sum(A[i,3],A[k,3],A[j,3],A[l,3])!=0){      #Sex Check
                if (i!=k&i!=j&i!=l&k!=j&k!=l&j!=l){                                               #Double Count
                  
                  freesumB_post=freesumB_pre;
                  freefinalB=c(i,k,j,l)
                }
              }
              }
            }
          }
        }
      }
    }
  }
}
A[freefinalB[1],2]=A[freefinalB[1],2]+1
A[freefinalB[2],2]=A[freefinalB[2],2]+1
A[freefinalB[3],2]=A[freefinalB[3],2]+1
A[freefinalB[4],2]=A[freefinalB[4],2]+1

A[freefinalB[1],9]=A[freefinalB[1],9]+1
A[freefinalB[2],9]=A[freefinalB[2],9]+1
A[freefinalB[3],9]=A[freefinalB[3],9]+1
A[freefinalB[4],9]=A[freefinalB[4],9]+1
freefinalB=swimmernames[freefinalB]
#________________________________________________________________
#                           Grad Relay B Male & Female
#________________________________________________________________

for (i in 1:nums){
  if (gradrelayMB>A[i,7]){
    if (A[i,3]==0){
      if(A[i,2]!=2){
        if(A[i,1]==1){
          
          gradrelayMB=A[i,7];
          gradfinalMB=i
        }
      }
    }
  }
  
  if (gradrelayFB>A[i,7]){
    if (A[i,3]==1){
      if(A[i,2]!=2){
        if(A[i,1]==1){
          
          gradrelayFB=A[i,7];
          gradfinalFB=i
        }
      }
    }
  }
  
}
A[gradfinalMB,2]=A[gradfinalMB,2]+1
A[gradfinalFB,2]=A[gradfinalFB,2]+1
gradfinalMB=c(swimmernames[gradfinalMB],'na','na','na')
gradfinalFB=c(swimmernames[gradfinalFB],'na','na','na')
#________________________________________________________________
#________________________________________________________________
#________________________________________________________________
#________________________________________________________________
#                           Medley Relay C
#________________________________________________________________

for (i in 1:nums){
  for (k in 1:nums){
    for (j in 1:nums){
      for (l in 1:nums){
        medleysumC_pre=A[i,4]+A[k,5]+A[j,6]+A[l,7]
        if (medleysumC_pre < medleysumC_post){                                                      #Speed
          if (sum(A[i,1],A[k,1],A[j,1],A[l,1])==4){                                               #Availbility
            if (A[i,2]!=2&A[k,2]!=2&A[j,2]!=2&A[l,2]!=2){                                          #Relay Count
              if (sum(A[i,8],A[k,8],A[j,8],A[l,8])==0){                                            #Medley Count
              if (sum(A[i,3],A[k,3],A[j,3],A[l,3])!=4&&sum(A[i,3],A[k,3],A[j,3],A[l,3])!=0){      #Sex Check
                if (i!=k&i!=j&i!=l&k!=j&k!=l&j!=l){                                                #Double Count
                  
                  medleysumC_post=medleysumC_pre;
                  medleyfinalC=c(i,k,j,l)
                }
                }
              }
            }
          }
        }
      }
    }
  }
}
A[medleyfinalC[1],2]=A[medleyfinalC[1],2]+1
A[medleyfinalC[2],2]=A[medleyfinalC[2],2]+1
A[medleyfinalC[3],2]=A[medleyfinalC[3],2]+1
A[medleyfinalC[4],2]=A[medleyfinalC[4],2]+1

A[medleyfinalC[1],8]=A[medleyfinalC[1],8]+1
A[medleyfinalC[2],8]=A[medleyfinalC[2],8]+1
A[medleyfinalC[3],8]=A[medleyfinalC[3],8]+1
A[medleyfinalC[4],8]=A[medleyfinalC[4],8]+1

medleyfinalC=swimmernames[medleyfinalC]


#________________________________________________________________
#________________________________________________________________
#________________________________________________________________
#                           Free Relay C
#________________________________________________________________

for (i in 1:nums){
  for (k in 1:nums){
    for (j in 1:nums){
      for (l in 1:nums){
        freesumC_pre=A[i,7]+A[k,7]+A[j,7]+A[l,7]
        if (freesumC_pre < freesumC_post){                                                          #Speed
          if (sum(A[i,1],A[k,1],A[j,1],A[l,1])==4){                                               #Availbility
            if (A[i,2]!=2&A[k,2]!=2&A[j,2]!=2&A[l,2]!=2){                                          #Relay Count
              if (sum(A[i,9],A[k,9],A[j,9],A[l,9])==0){                                            #Free Count
              if (sum(A[i,3],A[k,3],A[j,3],A[l,3])!=4&&sum(A[i,3],A[k,3],A[j,3],A[l,3])!=0){      #Sex Check
                if (i!=k&i!=j&i!=l&k!=j&k!=l&j!=l){                                               #Double Count
                  
                  freesumC_post=freesumC_pre;
                  freefinalC=c(i,k,j,l)
                }
              }
              }
            }
          }
        }
      }
    }
  }
}
A[freefinalC[1],2]=A[freefinalC[1],2]+1
A[freefinalC[2],2]=A[freefinalC[2],2]+1
A[freefinalC[3],2]=A[freefinalC[3],2]+1
A[freefinalC[4],2]=A[freefinalC[4],2]+1

A[freefinalC[1],9]=A[freefinalC[1],9]+1
A[freefinalC[2],9]=A[freefinalC[2],9]+1
A[freefinalC[3],9]=A[freefinalC[3],9]+1
A[freefinalC[4],9]=A[freefinalC[4],9]+1
freefinalC=swimmernames[freefinalC]

#________________________________________________________________
#________________________________________________________________
#________________________________________________________________
#                           Grad Relay A Male & Female
#________________________________________________________________

for (i in 1:nums){
  if (gradrelayMC>A[i,7]){
    if (A[i,3]==0){
      if(A[i,2]!=2){
        if(A[i,1]==1){
          
          gradrelayMC=A[i,7];
          gradfinalMC=i
        }
      }
    }
  }
  
  if (gradrelayFC>A[i,7]){
    if (A[i,3]==1){
      if(A[i,2]!=2){
        if(A[i,1]==1){
          
          gradrelayFC=A[i,7];
          gradfinalFC=i
        }
      }
    }
  }
  
}
A[gradfinalMC,2]=A[gradfinalMC,2]+1
A[gradfinalFC,2]=A[gradfinalFC,2]+1
gradfinalMC=c(swimmernames[gradfinalMC],'na','na','na')
gradfinalFC=c(swimmernames[gradfinalFC],'na','na','na')
#________________________________________________________________
#________________________________________________________________
B=matrix(c(medleyfinalA,medleyfinalB,medleyfinalC,freefinalA,freefinalB,freefinalC,gradfinalMA,gradfinalFA,gradfinalMB,gradfinalFB,gradfinalMC,gradfinalFC),nrow = 12,ncol=4,byrow=TRUE)
rownames(B)=c('medleyA','medleyB','medleyC','freeA','freeB','freeC','MenGradA','WomenGradA','MenGradB','WomenGradB','MenGradC','WomenGradC')


print(B)