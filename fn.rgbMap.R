
rm(list=ls(all=TRUE))

## this is the map function. code to run the function is below

fn.rgbMap=function(.rStack,.std="quantile",minX=75){

  ## .rStack=rStack; .std="quantile"
  require(png)
  require(raster)

  .std=.std[1]; MAX=5 ## this is the maximum number of quantiles
  if(!.std %in% c("quantile","range"))
	stop("Unknown standardization")

  if(class(.rStack) != "RasterStack") 
	stop(".rStack must be a rasterStack")
  if( nlayers(.rStack) != 3)
	stop("Number of rStack layers must be exactly 3")

  rNames=names(.rStack)

  if(.std == "quantile"){
    for(R in 1:nlayers(.rStack)){
      .rStack[[R]]= cut(.rStack[[R]],
		breaks=quantile(.rStack[[R]],seq(0,1,len=5+1)))
    }
  } 

    for(R in 1:nlayers(.rStack)){
      .rStack[[R]]= (.rStack[[R]]-minValue(.rStack[[R]]))/
	   (maxValue(.rStack[[R]])-minValue(.rStack[[R]]))*255
    }

  names(.rStack)=rNames

  ## plotRGB(.rStack)

  ## legend
  
  VALS=1:MAX
  MAT=matrix(1:MAX^2,nrow=MAX,byrow=T)
  MAT=MAT[rev(VALS),]

  png("temp.png", 2,2,"in",10,"transparent",600)
  layout(MAT); rm(MAT)
  par(mar=rep(0,4),oma=c(0,0,0,0))

  for(G in VALS ){
    for(R in VALS){
      plot(c(0,MAX*0.5+0.5),c(0,MAX*0.5+0.5),
		col=NA,ann=F,axes=F);box("plot")

      for(B in rev(VALS) ){
	  ## R=1;G=1;B=5
        
	  X=  c( (B-1)*0.5,B*0.5+0.5,B*0.5+0.5,(B-1)*0.5,(B-1)*0.5)
	  Y=  c( (B-1)*0.5,(B-1)*0.5,B*0.5+0.5,B*0.5+0.5,(B-1)*0.5)
        COL= rgb( (R-min(VALS))/(max(VALS)-min(VALS)),
			(G-min(VALS))/(max(VALS)-min(VALS)),
			(B-min(VALS))/(max(VALS)-min(VALS)) )  
        polygon(X,Y,col=COL,border="gray",lwd=0.5)

      }
    }
  }
  dev.off()

  plot.new()
  plot.window(extent(.rStack)[1:2],extent(.rStack)[3:4])
  plotRGB(.rStack, add=T)
##   axis(1,line=-2); axis(2,line=-2)
  img <- readPNG("temp.png")

  xLim=par()$usr[1:2]
  yLim=par()$usr[3:4]
  ASP= abs(diff(xLim))/abs(diff(yLim))
  xVals=c(minX,xLim[2])
  Units= diff(xVals)
  Units=Units/ASP
  yVals=c(yLim[2]-Units,yLim[2])

  xVals; yVals

  rasterImage(img, xVals[1], yVals[1], xVals[2],yVals[2])
  unlink("temp.png")

  text(mean(xVals),yVals[1],names(.rStack)[1],adj=c(0.5,2),cex=1,font=2)
  text(xVals[1],mean(yVals),names(.rStack)[2],adj=c(0.5,-1),cex=1,font=2,
	srt=90)


} ## end function
  
  

################################################################
################################################################  
## STEP 1: Get the raster data
## replace with code for your raster data
## at the end you need a rasterStack with three rasters

  library(raster); library(png)

  std="range" ## you can also use quantile
  wgPoly=rgdal::readOGR("./__Data/WGShapeFile","WG")
  r1=raster("./__Data/minus_tx/w001001.adf")
  r2=raster("./__Data/minus_tn/w001001.adf")
  r3=raster("./__Data/minus_pr2/w001001.adf")

  rStack=stack(list("MaxTemp"=r1,"MinTemp"=r2,"Prec"=r3)) 
  rStack=stack(projectRaster(rStack,crs= wgPoly@proj4string ))
  rStack=stack(crop(rStack,extent(wgPoly)))
  ## plot(rStack)

  ASP= abs(diff(extent(rStack)[1:2]))/abs(diff(extent(rStack)[3:4]))

  FN=paste0("__graphRGB_",paste(names(rStack),collapse="_"),".png")
  png(FN,4,4/ASP,"in",10,"transparent",600)
  fn.rgbMap(rStack,"range",minX=75.5)
  dev.off()


