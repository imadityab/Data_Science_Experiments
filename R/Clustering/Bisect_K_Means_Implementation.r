
rm(list=ls(all=T))

#library(mclust)
library(lattice)
library(MASS)
#library(car)
library(utils)          # for read/write csv files
#library(flexclust)      # for kcca

# read the data
d = read.table(file = "C:\\Users\\Aditya\\Desktop\\Studies\\AL\\Assignments\\d-c4hw2.csv", header=T, sep=",")
#C:\Users\Aditya\Desktop\Studies\Learn\R_Work\d-c4hw2.csv

# get field names
names (d)



myKMeans <- function(idata, k) {


# start with selecting k random centroids of data dimensions
# arrays to find minimum and maximum of all the dimensions in data
minArray = data.frame(matrix(ncol = ncol(idata), nrow = 1))
maxArray = data.frame(matrix(ncol = ncol(idata), nrow = 1))
randomCentroids = data.frame(matrix(ncol = ncol(idata), nrow = k))

#Data will be copied to this array with additional cluster label
dataWithCluster <- data.frame(matrix(ncol = ncol(idata)+1, nrow = nrow(idata)))

for(datadim in 1:ncol(idata))
{
#min max per dimension
minArray[1,datadim] = min(idata[,datadim])
maxArray[1,datadim] = max(idata[,datadim])
#generate random
randomArr = runif(k,minArray[1,datadim],maxArray[1,datadim])
for(centroidnum in 1:k)
	{
	#generating K random values and assigning them to each cluster per dimension
	randomCentroids[centroidnum,datadim] = randomArr[centroidnum]
	}
}

#print(randomCentroids)
centroids <- randomCentroids
#minX1 = min(idata[,1])
#maxX1 = max(idata[,1])
#minX2 = min(idata[,2])
#maxX2 = max(idata[,2])
#randomX1 = runif(k,minX1,maxX1)
#randomX2 = runif(k,minX2,maxX2) 
#centroids <- data.frame(matrix(ncol = 2, nrow = k))
#for(counter in 1:k)
#	{
#	centroids[counter,1]= randomX1[counter]
#	centroids[counter,2]= randomX2[counter]
#	}
#loop variable
iterateAgain = TRUE
	while(iterateAgain== TRUE)
	{
	#assume tbis is the last iteration
		iterateAgain = FALSE	
		for(i in 1: nrow(idata))
			{
			#for all rows do this
			minCl = 0
			minDistance = -1
			for(j in 1: k)
				{
				#for all clusters do this
				#sqDistance = (idata[i,1]- centroids[j,1])^2 + (idata[i,2]- centroids[j,2])^2
				sqDistance = 0;
				#print("SqDistance")
				#print(sqDistance)
				#print("Centroid")
				#print(centroids[j,])
				#print("Data Row")
				#print(idata[i,])
				for(datadimension in 1:ncol(idata))
					{
					#calculate square of distance
					sqDistance = sqDistance + (idata[i,datadimension]- centroids[j,datadimension])^2
					}
				#print("SqDistance")
				#print(sqDistance)
				#actual distance
				distance = sqrt(sqDistance)
				#assign point to the closest centroid
				if(minDistance < 0 || minDistance > distance)
					{
					minDistance = distance
					minCl = j
					}
				}
				
				#copy data
				for(datadimen in 1:ncol(idata))
					{
					dataWithCluster[i,datadimen] = idata[i,datadimen]	
					}
				#cluster label	
				dataWithCluster[i,ncol(dataWithCluster)] = minCl	
	
			#dataWithCluster[i,1] = idata[i,1]
			#dataWithCluster[i,2] = idata[i,2]
			#dataWithCluster[i,3] = minCl
			}
			
		#old centroid backup	
		oldCentroids = centroids
		

		for(clustNum in 1:k)
			{
			#for all nonempty clusters
				myCluster = subset(dataWithCluster,dataWithCluster[,ncol(dataWithCluster)]  == clustNum)
				
				if(nrow(myCluster)>0)
				{
				for(datadimen in 1:ncol(idata))
					{
					#calculate new centroids and compare to old
					centroids[clustNum,datadimen]= mean(myCluster[,datadimen], trim=0,na.rm=FALSE)	
					if(centroids[clustNum,datadimen]!=oldCentroids[clustNum,datadimen])
						{
						#centroid change, one more iteration is coming
						iterateAgain=TRUE
						}
					}
			
				#centroids[clustNum,1]= mean(myCluster[,1], trim=0,na.rm=FALSE)
				#centroids[clustNum,2]= mean(myCluster[,2], trim=0,na.rm=FALSE)
				}
			
				
				#if(centroids[clustNum,1]!=oldCentroids[clustNum,1] || centroids[clustNum,2]!=oldCentroids[clustNum,2])
				#	{
				#	iterateAgain = TRUE;
				#	}
			}	
	}	

	#result as original data + additional column for cluster label
  result <- dataWithCluster
  return(result)
}

myBisectKMeans <- function(input, bkNum) 
{
#Stores centroid for each cluster and its SSE
clusterCentroidAndSSE = data.frame(matrix(ncol = 2, nrow = 2))

#Stores SSE per iteration
SSEIteration = data.frame(matrix(ncol = 2, nrow = (bkNum-2)))

#pass to Kmeans first
result = myKMeans(input, k=2)
currentNum=2;

while(currentNum < bkNum)
	{
	#run this loop until we have desired number of clusters
		clusterNumbers = unique(result[,ncol(result)])
		
		#next culprit cluster number according to SSE
		nextClusterNum = -1
		
		#according to size
		nextClusterNumSize = 0; 
		maxSSE= -1
		SumSSE=0;
		MaxSize=0;
		for(x in 1: length(clusterNumbers))
			{
			#for all clusters
				evalNum = clusterNumbers[x]
				evalCluster= subset(result,result[,ncol(result)]  == evalNum)
				clusterSize = nrow(evalCluster)
				
				clusterCentroidAndSSE = data.frame(matrix(ncol = ncol(result), nrow = length(clusterNumbers)))
				for(dimension in 1:(ncol(result)-1))
					{
					#get centroid of each dimension of each cluster
					clusterCentroidAndSSE[evalNum,dimension]= mean(evalCluster[,dimension], trim=0,na.rm=FALSE)
					}
		
		
				clusterCentroidAndSSE[evalNum,ncol(result)]=0
				for(memberNum in 1:nrow(evalCluster))
					{
					#calculate SSE of cluster
						for(dimension in 1:(ncol(result)-1))
						{
						#print(dimension)
						#print((evalCluster[memberNum,dimension]-clusterCentroidAndSSE[evalNum,dimension])^2)
						clusterCentroidAndSSE[evalNum,ncol(result)]= clusterCentroidAndSSE[evalNum,ncol(result)] + (evalCluster[memberNum,dimension]-clusterCentroidAndSSE[evalNum,dimension])^2
						}
					
					}

				print("Cluster")
				print(evalNum)
				print("SSE")
				print(clusterCentroidAndSSE[evalNum,ncol(clusterCentroidAndSSE)])
				print("Size")
				print(clusterSize)
				#Add SSE of this cluster to that of the iteration
				SumSSE = SumSSE  + clusterCentroidAndSSE[evalNum,ncol(result)];
				
				#root logic for SSE can be implemented here
					if(maxSSE < 0 || maxSSE < clusterCentroidAndSSE[evalNum,ncol(result)])
					{
					#get cluster with max SSE
						maxSSE = clusterCentroidAndSSE[evalNum,ncol(result)]
						nextClusterNum = evalNum
					}
					
					if(MaxSize < clusterSize)
					{
					#get cluster with max Size
						MaxSize = clusterSize
						nextClusterNumSize = evalNum
					}
				
			}
			
			if(nextClusterNumSize > 0)
			{
				#Split cluster with max size using Kmeans
				subsetcluster=subset(result,result[,ncol(result)]  == nextClusterNumSize, select=1:(ncol(result)-1))
			
				splitResult = myKMeans(subsetcluster, k=2)
				#splitResult = myKMeans(subset(result,result[,ncol(result)]  == nextClusterNum, select=1:(ncol(result)-1)), k=2)
				nrow(splitResult)
				otherCluster = subset(splitResult,splitResult[,ncol(result)]  == 2)
				otherClusterNum = max(clusterNumbers)+1
				print("Splitting As per size Cluster Number")
				print(nextClusterNumSize)
				print("instead of as per SSE choice of Cluster Number")
				print(nextClusterNum)
				print("Total SSE")
				print(SumSSE)
				print("For currentNum")
				print(currentNum)
				
				SSEIteration[(currentNum-1),1] = currentNum;
				SSEIteration[(currentNum-1),2] = SumSSE;
				
				#merge the 2 clusters with result, update cluster label for one one of the two to next value
				for(resRow in 1: nrow(otherCluster))
					{
						for(datarow in 1: nrow(result))
							{
							isMatch = TRUE
							for(dims in 1: (ncol(result)-1))
								{
								if(result[datarow,dims]!=otherCluster[resRow,dims])
									{
									isMatch=FALSE
									}
								}
								
								if(isMatch)
									{
									
									result[datarow,ncol(result)] = otherClusterNum
																		
									}
							}
							
					}			
			}
		#now we have one more cluster so increment loop variable 
		currentNum = currentNum + 1
		#print("for CurrentNum")
		#print(currentNum)
		#print(clusterCentroidAndSSE)
	}
	#return(SSEIteration)
	# result contains data plus additional column for cluster label.
	return(result)
}

myresult = myBisectKMeans(d,bkNum=4)
plot(myresult[,1],myresult[,2],col=myresult[,3],pch=myresult[,3], xlab="length", ylab="width",main= "length vs. width")

