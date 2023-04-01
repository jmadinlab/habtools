# #Morphological variables from ply files in R
# #Author: Kyle Zawada
#
#
# #Note. All functions are based on ASCII ply files.
# #Binary .ply files can't be read by vcgPlyRead.
# #Pretty sure you can save ply.'s as ASCII in meshmixer.
#
#
#
# #vcrossp ====
# #calculate the cross product of two vectors
# vcrossp <- function(a, b) {
#   #a <- t(as.matrix(a)) #check
#   #b <- t(as.matrix(b)) #check
#   result <- matrix(NA,nrow(a),3)  #pre-generate matrix
#   result[,1] <- a[2]*b[3]-a[3]*b[2] #cross produxt for x
#   result[,2] <- a[3]*b[1]-a[1]*b[3] #cross product for y
#   result[,3] <- a[1]*b[2]-a[2]*b[1] #cross product for z
#   return(result)
# }
#
#
# #centroid ====
# #calculate the centroid for a given set of XYZ coordinates
# centroid <- function(XYZCoords) {
#   xmean <- mean(XYZCoords[,1])
#   ymean <- mean(XYZCoords[,2])
#   zmean <- mean(XYZCoords[,3])
#   return(c(xmean,ymean,zmean))
# }
#
# #setOrigin ====
# #shift the values of a set of XYZ coordinates so that the
# #origin lies at the reference vertex (defaults to nothing)
# setOrigin <- function(XYZCoords, Reference = c(0,0,0)) {
#   XYZCoords[,1] <- XYZCoords[,1] - (Reference[1])
#   XYZCoords[,2] <- XYZCoords[,2] - (Reference[2])
#   XYZCoords[,3] <- XYZCoords[,3] - (Reference[3])
#   return(XYZCoords)
# }
#
# #distCalc ====
# #calculate the geometric distance for two given XYZ coordinates
# distCalc <- function(XYZOne,XYZTwo) {
#   return((((XYZTwo[1]-XYZOne[1])^2)+((XYZTwo[2]-XYZOne[2])^2)+((XYZTwo[3]-XYZOne[3])^2))^(1/2))
# }
#
# #surfaceAreaOfTriangle ===
# #Calculates the surface area of a triangle based on a set of XYZCoords
# surfaceAreaOfTriangle <- function(XYZcoords) { #XYZCoords in X1,X2,X3,Y1,Y2,Y3,Z1,Z2,Z3 order
#   ax <- XYZcoords[2] - XYZcoords[1]
#   ay <- XYZcoords[5] - XYZcoords[4]
#   az <- XYZcoords[8] - XYZcoords[7]
#   bx <- XYZcoords[3] - XYZcoords[1]
#   by <- XYZcoords[6] - XYZcoords[4]
#   bz <- XYZcoords[9] - XYZcoords[7]
#   cx <- ay * bz - az * by
#   cy <- az * bx - ax * bz
#   cz <- ax * by - ay * bx
#
#   0.5 * sqrt(cx*cx + cy*cy + cz*cz)
# }
#
# #meshSurfaceArea ====
# #Calculates the surface area of a mesh by applying surfaceAreaOfTriangle over a plyMesh
# meshSurfaceArea <- function(plyMesh){
#   sum(sapply(1:ncol(plyMesh$it), function(x) {
#     surfaceAreaOfTriangle(as.vector(t(plyMesh$vb[,plyMesh$it[,x]][-4,])))
#   })
#   )
# }
#
# #signedVolumeOfTriangle ====
# #Calculates the signed volume of a triangle based on a set of XYZCoords
# #Signed volume means that volumes can take on a negative value depending on whether the surface normal
# #of the triangle is facing towards or away from the origin. When all positive and negative volumes
# #are integrated across the entire mesh, they values cancel out so that the final volume is an approximation
# #of the total volume of the mesh.
# signedVolumeOfTriangle <- function(XYZCoords) { #XYZCoords in X1,X2,X3,Y1,Y2,Y3,Z1,Z2,Z3 order
#   v321 <- XYZCoords[3] * XYZCoords[5] * XYZCoords[7]
#   v231 <- XYZCoords[2] * XYZCoords[6] * XYZCoords[7]
#   v312 <- XYZCoords[3] * XYZCoords[4] * XYZCoords[8]
#   v132 <- XYZCoords[1] * XYZCoords[6] * XYZCoords[8]
#   v213 <- XYZCoords[2] * XYZCoords[4] * XYZCoords[9]
#   v123 <- XYZCoords[1] * XYZCoords[5] * XYZCoords[9]
#   (1.0/6.0)*(-v321 + v231 + v312 - v132 - v213 + v123);
# }
#
# #meshVolume ====
# #Calculates the volume of a mesh by applying signedVolumeOfTriangle over a plyMesh
# meshVolume <- function(plyMesh) {
#   sum(sapply(1:ncol(plyMesh$it), function(x) {
#     signedVolumeOfTriangle(as.vector(t(plyMesh$vb[,plyMesh$it[,x]][-4,])))
#   })
#   )
# }
#
# #vectorAngle ====
# #calculates the euler angle between two vectors using three verticies
# #should be the correct formula!
# vectorAngle <- function(RefVert,Vert1,Vert2) {
#   mag1 <- sqrt(((Vert1[1] - RefVert[1])^2 +(Vert1[2] - RefVert[2])^2 + (Vert1[3] - RefVert[3])^2)) #calculate the magnitude of RefVert-to-Vert1 vector
#   mag2 <- sqrt(((Vert2[1] - RefVert[1])^2 + (Vert2[2] - RefVert[2])^2 + (Vert2[3] - RefVert[3])^2)) #calculate the magnitude of RefVert-to-Vert2 vector
#   cosine <- (((Vert1 - RefVert)/mag1) %*% ((Vert2 - RefVert)/mag2)) #Calculate the dot product divided by the multiplied vector magnitudes to return the cosine
#   return(acos(cosine)) #calculate the angle in radians by calculating the arc cosine of the cosine to give the angle in radians
# }
#
# #surfaceNormalGet ====
# #Calculates the surface normals for a set of vertices giving the direction the surface is facing.
# #If a target normal is supplied, it will return vertices that match the target normal.
# #If suppying a target normal, it might be worth settign a rounding decimal place to increase
# #matching tolerance.
# #matching target normals is useful if you want to extract all surfaces facing a particular direction.
#
# surfaceNormalGet <- function(Verticies, TargetNormal = NA, DecPlace = NA) {
#   vert1 <- Verticies[1:3] #first vertex XYZ
#   vert2 <- Verticies[4:6] #2nd vertex XYZ
#   vert3 <- Verticies[7:9] #3rd vertex XYZ
#   vec1 <- (vert2 - vert1) #1st edge vector
#   vec2 <- (vert3 - vert1) #2nd edge vector
#   normal <- vcrossp(vec1,vec2) #calculate the vector cross product
#   if(is.na(DecPlace)==T){ #check if DecPlace is supplied
#
#   } else {
#     normal <- round(normal, DecPlace) #round the normal
#     TargetNormal <- round(TargetNormal, DecPlace) #and TargetGet
#   }
#   if(is.na(TargetNormal) == T) { #check if TargetNormal is supplied
#
#   } else {
#     if(sum(TargetNormal %in% normal)==3) { #if TargetNormal and normal match
#       normal <- c(normal,T,vert1,vert2,vert3) #return normal with a TRUE value, plus the corresponding verticies
#     } else {
#       normal <- c(normal,F,0,0,0,0,0,0,0,0,0) #else just return the normal
#     }
#   }
#   return(normal)
# }
#
# #RotateMtx ====
# #A function that returns a 3x3 matrix to rotate a set of xyz coordinates.
# #to rotate a set of XYZ coordinates do; XYZCoords %*% RotateMtx (%*% == matrix multiplication)
# #based on the value of "Angle", if the value of "Angle" is in degrees not radians.
# #Make sure to set "degrees" as TRUE. "axis" denotes the axis of rotation.
#
# RotateMtx <- function(Angle, axis = c("x","y","z"), degrees = T) {
#   if(degrees == T) {
#     Angle <- Angle*(pi/180) #if degrees are supplied, convert to radians
#   } else {
#
#   }
#   if(axis == "z") {
#     RotateMtx <- matrix(data= c(cos(Angle), -sin(Angle),0,
#                                 sin(Angle),cos(Angle),0,
#                                 0,0,1), nrow = 3, ncol =3)
#     return(RotateMtx)
#   } else {
#
#   }
#   if(axis == "y") {
#     RotateMtx <- matrix(data= c(cos(Angle), 0, sin(Angle),
#                                 0,1,0,
#                                 -sin(Angle),0, cos(Angle))
#                         , nrow = 3, ncol =3)
#     return(RotateMtx)
#
#   } else {
#
#   }
#   if(axis == "x") {
#     RotateMtx <- matrix(data= c(1,0,0,
#                                 0, cos(Angle), -sin(Angle),
#                                 0, sin(Angle), cos(Angle))
#                         , nrow = 3, ncol =3)
#     return(RotateMtx)
#
#   } else {
#
#   }
# }
#
# #rotateApply ====
# #apply a given function to data rotated in increments. DataList should be a
# #list of each dataset needed to be rotated for the function to work properly.
# #Will re-write to make it more generally applicable in the future (turns out I haven't done that yet)
# #The nuts and bolts behind this are a bit opaque, but one application of this is to calculate
# #colony shape factor from multiple angles to see how it changes depending on direction.
# #DataList: list of data to apply to
# #AngleIncrement: How much to rotate by each iteration
# #FullRotation: T = rotate through 360 degress, F = rotate through 180 degrees.
# #Function: function to be applied
# #Axis: Axis to rotate through
# #ExtraFunctionInputs: if the supplied function requires additional arguments, include them here.
# rotateApply <- function(DataList, AngleIncrement, FullRotation = F, Function, Axis = "z", ExtraFunctionInputs= NULL){
#   #plot3d(DataList[[1]])
#   Values <- list()
#   if(FullRotation == T){
#     A <-360
#   } else {
#     A <- 180
#   }
#   for(a in 1:(A/AngleIncrement)){
#     RMtx <- RotateMtx(AngleIncrement*a,paste(Axis))
#     if(length(DataList)>1){
#       RtDataList <- (sapply(1:length(DataList), function(x) DataList[[x]] %*% RMtx))
#     } else {
#       RtDataList <- list(DataList[[1]] %*% RMtx)
#     }
#     ListCatch <- ifelse(length(RtDataList) == 1,0,1)
#     x <- NULL
#     if(ListCatch == 1){
#       for(d in 1:(length(RtDataList)-1)){
#         x <- paste(x,paste0("RtDataList[[",d,"]]"),",")
#       }
#       x <- paste(x,paste0("RtDataList[[",length(RtDataList),"]]"))
#     } else {
#       x <- paste(x,paste0("RtDataList[[",length(RtDataList),"]]"))
#     }
#     if(is.null(ExtraFunctionInputs)){
#       output <- eval(parse(text = paste0("Function(",x,")")))
#     } else {
#       ListCatch <- ifelse(length(ExtraFunctionInputs) == 1,0,1)
#       if(ListCatch == 1) {
#         for(d in 1:(length(ExtraFunctionInputs)-1)){
#           x <- paste(x,paste0(",ExtraFunctionInputs[[",d,"]]"),",")
#         }
#         x <- paste(x,paste0(",ExtraFunctionInputs[",length(ExtraFunctionInputs),"]"))
#         output <- eval(parse(text = paste0("Function(",x,")")))
#       } else {
#         x <- paste(x,paste0(",ExtraFunctionInputs[",length(ExtraFunctionInputs),"]"))
#         output <- eval(parse(text = paste0("Function(",x,")")))
#       }
#
#     }
#     Values[[a]] <- output
#     #points3d(RtDataList[[1]], col = a)
#   }
#   return(Values)
# }
#
# #Colony shape factor ====
# #calculates colony shape factor, a dimensionless value that describes
# #mechanical dislodgment vulnerability of a coral.
# #Defaults to zero for no adjustments.
# #Colony: xyz coordinates of a colony mesh.
# #Attach: attachment surface area, if none supplied, will attempt to estimate.
# #by finding the centroid of all lowest points in dataset.
#
# colonyShapeFactor <- function(Colony, Attach = NULL) {
#   if(is.null(Attach)) {
#     Attach <- Colony[which(round(Colony[,3],0)==min(round(Colony[,3],0))),]
#     Attach[,3]<- 0
#   } else {
#
#   }
#   AttachPoint <- centroid(Attach)
#   dat <- setOrigin(Colony,Reference = AttachPoint)  #set origin of colony data to the attachment point of the coral
#   dat <- round(dat) #round all of the data to zero d.p.
#   dat <- unique(dat[,c(1,3)]) #extract the unique X and Z coordinate pairs (right hand rule)
#   Heights <- sort(unique(dat[,2])) #extract the Heights (Z Values) of dat
#   Widths <- sapply(c(1:length(Heights)), function(x) projectedWidth(dat[which(dat[,2] == Heights[x]),1],
#                                                                     dat[which(dat[,2] == Heights[x]+1),1],1)
#   ) #Calculates the projected width of the points at each height x (see projectedWidth)
#
#   Areas <- Widths*Heights
#   parDiam <- Attach[which(Attach[,2]==max(Attach[,2])),2]-Attach[which(Attach[,2]==min(Attach[,2])),2] #diameter of the attachment area parallel to wave action (Y-axis) via difference between the max and min Y value in the attachment area
#   perDiam <- Attach[which(Attach[,1]==max(Attach[,1])),1]-Attach[which(Attach[,1]==min(Attach[,1])),1] #diameter of the attachment area perpendicular to wave action (X-axis) via difference between the max and min X value in the attachment area
#   CSF<- (16/((parDiam^2)*perDiam*pi))*sum(Areas) #colony shape factor calculation
#   return(CSF)
# }
#
# #rotateToSurfaceNormalMtx ====
# #Generates a rotation matrix to rotate the z axis of a set of coordinates to a supplied normal.
# #e.g. if the normal is at 45 degrees to the z-axis, this will create a matrix where;
# #XYZCoords %*% rotateToSurfaceNormalMtx results in the Z-axcis for the coordinates to match the
# #supplied normal.
#
# rotateToSurfaceNormalMtx <- function(Normal){#XYZCoords, Normal) {   ifelse(is.na(sum(Normal/Normal)), Normal <- Normal + 0.00000001, Normal <- Normal) #if any of the normal coordinates are 0, add a tiny constant to avoid throwing errors.
#   XVector <- vcrossp(Normal, c(0,0,Normal[3])) #calculate the cross product of the normal vector and the Z-axis vector
#   YVector <- vcrossp(Normal, XVector[1,1:3]) #calculate the cross product of the Xvector
#   ZDist <- distCalc(c(0,0,0),Normal) #calculate lengths of vectors wrt the origin
#   XDist <- distCalc(c(0,0,0),XVector)
#   YDist <- distCalc(c(0,0,0),YVector)
#   ifelse(sum(Normal) != 0,ZVectorNorm <-  Normal/ZDist, ZVectorNorm <- c(1,1,1)) #normalise the vectors via the vector distances
#   ifelse(sum(XVector) != 0,XVectorNorm <-  XVector/XDist, XVectorNorm <-  c(1,1,1)) #normalise the vectors via the vector distances
#   ifelse(sum(YVector) != 0,YVectorNorm <-  YVector/YDist, YVectorNorm <- c(1,1,1)) #normalise the vectors via the vector distances
#   RotMtx <- t(rbind(-XVectorNorm,-YVectorNorm,-ZVectorNorm)) #generate the rotation matrix using the inversed normals (This is used to rotate the coords so that the z-axis down is equal to the normal, supply the inverted normal to stop this)
#   #XYZRotated <-  XYZCoords %*% RotMtx #matrix multiplication for rotation
#   return(RotMtx)
# }
#
# #maximumLinearDimension ====
# #calculates all pairwise geometric distances
# #between a set of points and returns the largest value
# #to restrict this to certain dimensions, replace all coordinates wih 0 for dimensions you're
# #not interested in i.e. set all Z coordinates to 0 to get the MLD in X,Y space.
# maximumLinearDimension <- function(XYZCoords){
#   Distances <- dist(XYZCoords) #geometric distance matrix of the coordinates
#   return(max(Distances))
# }
#
#
# #ellipseEstimates ====
# #calculates a range of ellipse-based metrics from a 2D coordinate set.
# #returns the X and Y diameter, the area and the perimeter.
# ellipseEstimates <- function(XYCoords) {
#   YAxis <- XYCoords[which(XYCoords[,2]==max(XYCoords[,2])),2]- #YAxis calculated by calculating the difference between the minimum and maximum y values
#     XYCoords[which(XYCoords[,2]==min(XYCoords[,2])),2]
#   XAxis <- XYCoords[which(XYCoords[,1]==max(XYCoords[,1])),1]- #XAxis calculated by calculating the difference between the minimum and maximum x values
#     XYCoords[which(XYCoords[,1]==min(XYCoords[,1])),1]
#   Area <- ((YAxis/2)*(XAxis/2))*pi #Equation for calculating area of ellipse
#   Circumference <- 4*((XAxis/2) + (YAxis/2)) * ((pi/4)^((4*(XAxis/2)*(YAxis/2))/(((XAxis/2)+(YAxis/2))^2))) #Crazy mathsy equation to calculate the circumference of an ellipse
#   return(c(XAxis,YAxis,Area,Circumference))
# }
#
#
# #boundingBox ====
# #Returns the surface area and volume of the bounding box around a set of XYZ cordinates.
# boundingBox <- function(XYZCoords) {
#   XLength <- abs(diff(range(XYZCoords[,1])))
#   YLength <- abs(diff(range(XYZCoords[,2])))
#   ZLength <- abs(diff(range(XYZCoords[,3])))
#   XYZ <- c(XLength,YLength,ZLength)
#   Volume <- XYZ[1]*XYZ[2]*XYZ[3]
#   SurfaceArea <- 2*((XYZ[1]*XYZ[2])+(XYZ[1]*XYZ[3])+(XYZ[2]*XYZ[3]))
#   return(c(SurfaceArea,Volume))
# }
#
#
# #ProjectedWidth ====
# #calculates the width of a vector by calculating the differences between the ordered data,
# #then totalling by a given tolerance value. CheckVector allows comparison to another vector
# #prior to calculation, typically used to check a vector above or below Vector for area
# #calculations
# projectedWidth <- function(Vector,CheckVector = Vector, Tol = 1) {
#   VDiff <- c(2,diff(sort(Vector[which(Vector %in% CheckVector)])))  #generate vector of differences on ordered vector values that are also present in CheckVector (CheckVector defaults to Vector unless user values are supplied)
#   Ans <- sum(VDiff[which(VDiff <= Tol)]) #Total all differences that are equal or below the tolerance value, used in a spatial context to ignore distances between points further apart than Tol
#   return(Ans)
# }
#
#
# #PlanarAreaByPointsGrid ====
# #calculates planar area by putting points on a 1by1 unit grid and calculates the distances of
# #side-by-side points for each y level
#
# planarAreaFromPointsGrid <- function(XYZCoords, Tol = 1, dims = c(1,2)) {
#   dat <- round(XYZCoords) #round all of the data to zero d.p.
#   dat <- unique(dat[,c(dims[1],dims[2])]) #extract the unique X and Y coordinate pairs (right hand rule)
#   YVals <- unique(dat[,2]) #extract the Y Values of dat
#   Areas <- sapply(c(1:length(YVals)),
#                   function(x) projectedWidth(dat[which(dat[,2] == YVals[x]),1],
#                                              dat[which(dat[,2] == YVals[x]+1),1],
#                                              1)) #Calculates the projected width of the points at each YValue x (see projectedWidth)
#   return(sum(abs(Areas)))
# }
#
# #Volume scale ====
# #Scale a set of coordinates to unit (1) volume.
# #XYZCoords: set of coordinates
# #Volume: measured volume on coordinate set
#
# volumeScale <- function(XYZCoords, Volume){
#   XYZCoordsTwo <- XYZCoords/(Volume^(1/3))
#   return(XYZCoordsTwo)
# }
#
#
# #projectedAreaScale ====
# #Scale a set of coordinates to a given projected area.
# #XYZCoords: set of coordinates
# #ProjectedArea: measured area on coordinate set
# #TargetArea: Target area after scaling (defaults to 1)
#
# projAreaScale <- function(XYZCoords, ProjectedArea, TargetArea = 1) {
#   XYZCoordsTwo <- (XYZCoords/((ProjectedArea^(1/2)/(TargetArea^(1/2)))))
#   return(XYZCoordsTwo)
# }
#
#
# #coordScaler ====
# #Generalised coordinate scaling function, by default scales to 1 using
# #the number of columns in the dataset as the dimension.
# #Coords: Set of coordiantes with each column refering to a dimension
# #ActualValue: Measured value on coordinate set, e.g. surface area or volume
# #TargetValue: Allows scaling to a set value other than 1,
# #e.g. scale to a volume of 3000mm3
# #Dimension: Dimension to scale to. Should match the ActualValue dimension
# #e.g. 2 for surface area, 3 for volume, etc.
#
# coordScaler <- function(Coords, ActualValue, TargetValue = 1, Dimension = ncol(Coords)) {
#   CoordsTwo <- (Coords/ #divide the coordinate set
#                   ((ActualValue^(1/Dimension)/ #by the measured value raised to the power of 1/dimension of measurement
#                       (TargetValue^(1/Dimension))))) #divided by the target value raised to the power of 1/dimension of measurement
#   return(CoordsTwo) #retun scaled coordinates.
# }
#
# #convhulln ====
# #Convex hull function from the "geometry" package.
# #set options argument to "FA", to include convex surface area and
# #volume in output.
# library(geometry)
#
# #convhulln() #this is the function
#
# #sphericity:====
# #calculates sphericity (a measure of compactness) based on the surface area
# #and volume of an object by comparing the ratio of the surface area of a sphere with the
# #same volume as the object and the objects surface area
#
# sphericity<- function(SurfaceArea,Volume) {
#   S <- ((pi^(1/3))*((6*Volume)^(2/3)))/SurfaceArea
#   return(S)
# }
#
# #packing:====
# #calculates packing (a measure of how surface area is distributed in the local
# #environment) by dividing the surface area by the surface area of the convex hull.
#
# packing<- function(SurfaceArea,ConvexArea){
#   Cvex <- SurfaceArea/ConvexArea
#   return(Cvex)
# }
#
# #convexity:====
# #calculates convexity (a measure of how volume is distributed in the local
# #environment) by dividing the object volume by the convex hull volume.
#
# convexity<- function(Volume,ConvexVolume){
#   Cvex <- Volume/ConvexVolume
#   return(Cvex)
# }
#
# #secondMomentArea ====
# #Calculates the 2nd moment of surface area by multiplying the surface area of each
# #triangle in the mesh by it's distance from the origin (should be set to the attachment point
# #of the mesh). The sum of these values is the 2nd moment of area.
# #To compare moments in terms of shape only, set the volume of the object to a set value
# #(typically 1mm3) for all object you wish to compare.
# #Axis is z by default, meaning it will calculate the vertical second moment, but this can be changed if needed.
#
# secondMomentArea <- function(plyMesh, Axis = "z"){
#   Dim <- ifelse(Axis == "z",3,ifelse(Axis == "y",2,1)) #set the dimension based on Axis value
#   SAMoments <- sapply(1:ncol(plyMesh$it), function(x) {
#     surfaceAreaOfTriangle(as.vector(t(plyMesh$vb[,plyMesh$it[,x]][-4,]))) * #calculate the surface area
#       centroid(matrix(as.vector(t(plyMesh$vb[,plyMesh$it[,x]][-4,])),3,3))[Dim] #multiply it by the distance to the origin along Axis
#   })
#   return(sum(SAMoments))
# }
#
# #secondMomentVolume ====
# #Calculates the 2nd moment of volume by multiplying the volume of each
# #triangle in the mesh by it's distance from the origin (should be set to the attachment point
# #of the mesh). The sum of these values is the 2nd moment of volume.
# #To compare moments in terms of shape only, set the volume of the object to a set value
# #(typically 1mm3) for all object you wish to compare.
# #Axis is z by default, meaning it will calculate the vertical second moment, but this can be changed if needed.
#
# secondMomentVolume <- function(plyMesh, Axis = "z"){
#   Dim <- ifelse(Axis == "z",3,ifelse(Axis == "y",2,1)) #set the dimension based on Axis value
#   VolMoments <- sapply(1:ncol(plyMesh$it), function(x) {
#     signedVolumeOfTriangle(as.vector(t(plyMesh$vb[,plyMesh$it[,x]][-4,]))) * #calculate the signed volume of a triangle
#       centroid(rbind(t(matrix(as.vector(plyMesh$vb[,plyMesh$it[,x]][-4,]))),c(0,0,0)))[Dim] #multiply it by the distance to the origin along Axis from the centroid of the prism
#   })
#   return(sum(VolMoments))
# }
#
# #Fractal dimension code ====
#
# #Rescale
#
# #A function to rescale a set of points for use in the countFilledCells function
#
# #x = target to rescale
# #x0 = minimum of x
# #xm = maximum of x
# #n = rescale factor. if n = 1, the function will return x scaled so that the mimimum value is 0 and the max is 1,
# #if n = 2, the range will be from 0 to 2, etc
#
# rescale <- function(x, x0, xm, n) {
#   (x - x0)/(xm - x0)*n
# }
#
# #CountFilledCells
#
# #This rescales a set of coordinates to a particular grid size, if a grid cell contains a coordinate, it returns a 1, else 0.
# #It them sums up the total number of grid cells that contain a coordinate.
#
# #dat = 3d matrix data
# #xyzmin/max = the minimum and maximum span of the xy & z values,
# #ngrid = 3d grid size value.
#
# countFilledCells <- function(dat, xmin, xmax, ymin, ymax, zmin, zmax, ngrid) {
#   ret <- table(ceiling(rescale(dat[,1], xmin, xmax, ngrid)), ceiling(rescale(dat[,2], ymin, ymax, ngrid)), ceiling(rescale(dat[,3], zmin, zmax, ngrid)))
#   sum(ret > 0) #count only cells that contain a point
# }
#
# #FractalDimension
#
# #This is a 3D implementation of the box counting algorithm for fractal dimension.
# #By counting how many grid cells contain a coordinate across a range of grid cell sizes.
# #By plotting grid cell size by number of cells that contain a point, we can calculate the slope,
# #which is our estimate for fractal dimension. Be wary to set the correct min and max cell sizes.
# #typically the min size is a bit bigger than the resolution of the coordiante system. i.e. if the average
# #euclidean distance between two points is 1, then set the min size to above 1.
#
#
# #Set plot to TRUE to return a plot with the fitted line. if too many points are far away from the fitted line
# #consider changing your minimum/maximum limits. The function also returns the fitted mdoel so you can assess
# #R-sqaured and other model parameters for checking.
#
# #min_size is 0 by default. If your minimum size is unknown, run with plot = T and inspect the plot.
# #identify which size should be the minimum by looking at the size that the trend becomes linear.
# #remembering that the plot is ona log10 scale.
#
# #XYZCoords should be a n by 3 matrix, with each column representing x,y and z coordiantes respectively.
#
# #Fractal dimensions for 2D surfaces should range between 2 and 3, anything below or above that is problematic.
#
# fractalDimension <- function(XYZCoords, max_cubes=63, min_cubes = 4, plot=FALSE) {
#
#   max_size <- max(diff(apply(XYZCoords, 2, range)))
#
#   cubes <- unique(round(10^cumsum(rep(0.1, 20))),0) #generates a range of cube counts, i.e. 1000 = 1000 cubes will be overlayed on the XYZCoords.
#   cubes <- cubes[cubes >= min_cubes] #culls largest cubes because at larger sizes the values a)  don't add much and b) reduce the model fit.
#   sizes <- (diff(range(XYZCoords)))/cubes #sizes of cubes in mm calculated using the range of the XZY data. This only effects the intercept and not the slope and so doesn't have an effect on the fractal dimension itself, but my be useful for troubleshooting odd results
#   sizes <- sizes[cubes <= max_cubes] #remove sizes below minimum threshold
#   cubes <- cubes[cubes <= max_cubes] #remove cube sizes that are below the minimum threshold
#   counts <- lapply(cubes, function(n) countFilledCells(XYZCoords,
#                                                        min(XYZCoords[,1]),
#                                                        min(XYZCoords[,1]) + max_size,
#                                                        min(XYZCoords[,2]),
#                                                        min(XYZCoords[,2]) + max_size,
#                                                        min(XYZCoords[,3]),
#                                                        min(XYZCoords[,3]) + max_size,
#                                                        n)) #see count filled cells function
#   counts <- unlist(counts) #convert list of counts to vector
#   mod <- lm(log10(counts) ~ log10(cubes)) #linear regression of cube counts for each cube size
#   D <- abs(as.numeric(coef(mod)[2])) #D = fractal dimension, calculated as the slope of log10(counts) and log10(sizes)
#
#   if (plot) { #if plot = T
#     plot(log10(counts) ~ log10(cubes)) #plot the regression model for visual inspection
#     abline(mod) #add regression line
#     abline(mod$coefficients[[1]],2, col = "red")
#     abline(mod$coefficients[[1]],3, col = "blue")
#   }
#
#   return(list(D,mod)) #return a list, with the 1st element being fractal dimension, followed by the regression model for further assessment (r-squared, etc)
# }
#
#
# fractalDimensionFinal <- function(XYZCoords, min_size=0, plot=FALSE, reps = 1, jitter = min_size/2) {
#
#   max_size <- ceiling(max(diff(apply(XYZCoords, 2, range))))
#
#   cube_vec <- unique(round((10^cumsum(rep(0.1, 30))),0))
#   cubes <- cube_vec[cube_vec <= max_size]
#   size_vec <- (diff(range(XYZCoords)))/cubes
#   cubes <- cube_vec[size_vec >= min_size]
#   cubes <- cubes[size_vec <= max_size]
#   sizes <- size_vec[size_vec >= min_size]
#   sizes <- sizes[size_vec <= max_size]
#   x0 <- min(XYZCoords[,1])
#   y0 <- min(XYZCoords[,2])
#   z0 <- min(XYZCoords[,3])
#
#   answer <- lapply(cubes, function(n) lapply(1:reps, function(j) countFilledCells(XYZCoords, x0, x0 + max_size, y0, y0 + max_size, z0, z0 + max_size, n)))
#   answer <- unlist(answer)
#   mod <- lm(log10(answer) ~ log10(sizes))
#   D <- -as.numeric(coef(mod)[2])
#
#   if (plot) {
#     plot(log10(answer) ~log10(sizes))
#     abline(mod)
#   }
#
#   return(list(D,mod))
# }
#
# #Josh frac dim
#
# rescale <- function(x, x0, xm, n) {
#   (x - x0)/(xm - x0)*n
# }
#
# count_filled_cells <- function(dat, xmin, xmax, ymin, ymax, zmin, zmax, ngrid) {
#   ret <- table(ceiling(rescale(dat[,1], xmin, xmax, ngrid)), ceiling(rescale(dat[,2], ymin, ymax, ngrid)), ceiling(rescale(dat[,3], zmin, zmax, ngrid)))
#   sum(ret > 0)
# }
#
# # For colonies (Kyle)
#
#
# get_D3 <- function(data, min_size=10, plot=FALSE) {
#
#   max_size <- ceiling(max(diff(apply(data, 2, range))))
#
#   cube_vec <- unique(10^cumsum(rep(0.1, 25)))
#   size_vec <- max_size/cube_vec
#   cubes <- cube_vec[size_vec >= min_size & size_vec < max_size]
#   sizes <- size_vec[size_vec >= min_size & size_vec < max_size]
#
#   x0 <- min(data[,1])
#   y0 <- min(data[,2])
#   z0 <- min(data[,3])
#
#   answer <- lapply(cubes, function(n) count_filled_cells(data, x0, x0 + max_size, y0, y0 + max_size, z0, z0 + max_size, n))
#   answer <- unlist(answer)
#
#   mod <- lm(log10(answer) ~ log10(sizes))
#   D <- -as.numeric(coef(mod)[2])
#
#   if (plot) {
#     plot(log10(answer) ~ log10(sizes))
#     abline(mod)
#   }
#
#   return(list(D,mod))
# }
