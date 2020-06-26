##Introduction to Hierarchical Clustering
#Analyze movieLens data
movies = read.table("/Users/USER/Desktop/Movie_Recommend_kmeans/movieLens.txt", header=FALSE, sep="|", quote="\"")

str(movies)

# Add column names
colnames(movies) = c("ID", "Title", "ReleaseDate", "VideoReleaseDate", 
                     "IMDB", "Unknown", "Action", "Adventure", "Animation",
                     "Childrens", "Comedy", "Crime", "Documentary", "Drama", 
                     "Fantasy", "FilmNoir", "Horror", "Musical", "Mystery", 
                     "Romance", "SciFi", "Thriller", "War", "Western")

str(movies)

# Remove unnecessary variables
movies$ID = NULL
movies$ReleaseDate = NULL
movies$VideoReleaseDate = NULL
movies$IMDB = NULL

# Remove duplicates
movies = unique(movies)

# Take a look at our data again:
str(movies)


# Compute distances
distances = dist(movies[,2:20], method = "euclidean")

# Hierarchical clustering
clusterMovies = hclust(distances, method = "centroid") 
# Plot the dendrogram
plot(clusterMovies)

# Assign points to clusters
clusterGroups = cutree(clusterMovies, k = 10)
table(clusterGroups)

clusterMovies = hclust(distances, method = "ward.D") 

#Now let's figure out what the clusters are like.
# Let's use the tapply function to compute the percentage of movies in each genre and cluster
tapply(movies$Action, clusterGroups, mean)
tapply(movies$Romance, clusterGroups, mean)


# Find which cluster Men in Black is in.

subset(movies, Title=="Men in Black (1997)")
clusterGroups[257]

# Create a new data set with just the movies from cluster 2
cluster2 = subset(movies, clusterGroups==2)

# Look at the first 10 titles in this cluster:
cluster2$Title[1:10]



##Introduction to k-means Clustering
#Analyze CharlesBookClub
CharlesBookClub = read.csv("/Users/USER/Desktop/Movie_Recommend_kmeans/CharlesBookClub.csv")
Book.df=CharlesBookClub[,c(3:6,8:18)]
set.seed(123)
Member.seg = kmeans(scale(Book.df), centers=3)

Member.seg
Member.seg$cluster

k=10
WGSS=c()
for(i in 1:k){
   WGSS[i]=sum(kmeans(scale(Book.df), centers=i)$withinss)
}

plot(1:k, WGSS, type="b")


