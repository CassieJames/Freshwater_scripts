## a collection of cool functions for spatial work in ascii grids
## lauren hodgson creates functions from bits of jeremy vanderwal's code, mostly
## ----------------------------------------------------------------------------

## make a base ascii with value of zero
make.base.asc=function(tasc) {
	base.asc=tasc
	base.asc[which(is.finite(base.asc))]=0;
	return(base.asc)
}
## example
## base.asc=make.base.asc(test.asc)

## ----------------------------------------------------------------------------

## make your positions file with columns 'row', 'col', 'lat', 'lon'
make.pos=function(tasc) {
	pos = as.data.frame(which(is.finite(tasc),arr.ind=TRUE))
	pos$lat = getXYcoords(tasc)$y[pos$col] #extract the longitudes
	pos$lon = getXYcoords(tasc)$x[pos$row]; #extract the latitudes
	return(pos)
}
## example
## pos=make.pos(base.asc)

## ----------------------------------------------------------------------------

## make an ascii grid file from a column of pos
## note: requires pos
make.asc=function(tcol) {
	tasc=base.asc
	tasc[cbind(pos$row,pos$col)]=tcol; return(tasc)}
##example
##test.asc=make.asc(pos$test)

## ----------------------------------------------------------------------------

## assign multiple values to multiple variables
## found on:
## http://strugglingthroughproblems.wordpress.com/2010/08/27/matlab-style-multiple-assignment-in%C2%A0r/
'%=%' = function(assign.list, list.variables, ...) UseMethod('%=%')
assign.list = function(...) {
	List = as.list(substitute(list(...)))[-1L]
	class(List) = 'lbunch'
	
	List #i don't know what this is
	
}
'%=%.lbunch' = function(assign.list, list.variables, ...) {
	Envir = as.environment(-1)
	
	for (II in 1:length(assign.list)) {
		do.call('<-', list(assign.list[[II]], list.variables[[II]]), envir=Envir)
	}
}

## examples
## assign.list(a,b) %=% c(1,2)
## assign.list(min.lat,max.lat) %=% range(pos$lat,na.rm=T)
## assign.list(min.lon,max.lon) %=% range(pos$lon,na.rm=T)

## ----------------------------------------------------------------------------

## 'zooming' in on an area of interest within an image
## currently only works for southern hemisphere

## dynamic zoom takes a column of pos and allows you to set the proportion of degrees for padding
## only works for column of pos with NA and finite values
dynamic.zoom = function(long.col,lat.col,limit.col=NULL, padding.percent) {
	x=padding.percent/100
	if (is.null(limit.col)) {
		range.lon=range(long.col,na.rm=T)
		range.lat=range(lat.col,na.rm=T)
	} else {
		range.lon=range(long.col[which(limit.col>0)],na.rm=T)
		range.lat=range(lat.col[which(limit.col>0)],na.rm=T)
	}
	assign.list(min.lon,max.lon,min.lat,max.lat) %=% c(range.lon,range.lat)
	dist.lon=max.lon-min.lon
	dist.lat=max.lat-min.lat
	max.dist=max(dist.lon,dist.lat)
	prop=x*max.dist
	assign.list(min.lon,max.lon,min.lat,max.lat) %=% c(min.lon-prop,max.lon+prop,min.lat-prop,max.lat+prop)
	return(c(min.lon,max.lon,min.lat,max.lat))
	}

## fixed zoom takes a column of pos and allows you to set fixed degrees for padding	
## only works for column of pos with NA and finite values
fixed.zoom = function(tcol,degrees.of.padding, min.degrees) {
	x=degrees.of.padding
	min.lat=min(pos$lat[which(is.finite(tcol))],na.rm=TRUE)
	max.lat=max(pos$lat[which(is.finite(tcol))],na.rm=TRUE)
	min.lon=min(pos$lon[which(is.finite(tcol))],na.rm=TRUE)
	max.lon=max(pos$lon[which(is.finite(tcol))],na.rm=TRUE)
	min.lon=min.lon-x;max.lon=max.lon+x;min.lat=min.lat-x;max.lat=max.lat+x
	dist.lon=max.lon-min.lon
	dist.lat=max.lat-min.lat
	min.dist=min(dist.lon,dist.lat)
	if (min.dist<min.degrees) { 
		x=x+(min.degrees-min.dist)/2
		min.lon=min.lon-x;max.lon=max.lon+x;min.lat=min.lat-x;max.lat=max.lat+x
	} else {  }
	return(c(min.lon,max.lon,min.lat,max.lat))
}

## ----------------------------------------------------------------------------

## note: currently only places legend in bottom margin,and relies on plotrix

## positions legend in margin as a percentage of width and height of image
margin.legend=function(x,y,w,h,cols,text.limits,cex) {
	
	par(xpd=T)
	assign.list(l,r,b,t) %=% par("usr")
	xdiff=r-l
	ydiff=t-b
	x=x/100*xdiff #5% in 
	y=y/100*ydiff #5% down
	w=x+(w/100*xdiff) #50% across (finishes at 55%)
	h=y-(h/100*ydiff) #5% high (finishes at 
	
	return( color.legend(l+x,b+h,l+w,b+y,text.limits,cols,cex) )
}
## xl = percent from left limit of image of left side of legend
## xr = percent from left limit of image of right side of legend
## yt = percent from bottom limit of image of top side of legend
## yb = percent from bottom limit of image of bottom side of legend


## ----------------------------------------------------------------------------

clip.image=function(l,r,b,t) {
	clippos=pos; clippos$base=1
	clippos[which(clippos$lat<b),'base']=NA
	clippos[which(clippos$lat>t),'base']=NA
	clippos[which(clippos$lon<l),'base']=NA
	clippos[which(clippos$lon>r),'base']=NA
	clip=base.asc; clip[cbind(pos$row,pos$col)]=clippos[,'base']
	return(clip)}
		
		
## ----------------------------------------------------------------------------

##uses asc of sigDiff to make an ascii of imageDiff
ascDiff=function (tasc, sig.levels = c(0.025, 0.975))
{	
    tasc[which(is.finite(tasc) & tasc <= sig.levels[1])] = 9
    tasc[which(is.finite(tasc) & tasc > sig.levels[1] & tasc <
        sig.levels[2])] = 10
    tasc[which(is.finite(tasc) & tasc <= 1)] = 11
	return(tasc)
}

## ----------------------------------------------------------------------------


padded.limits=function(limits, floor.lim=-Inf, ceiling.lim=Inf) {

	ldiff=limits[2]-limits[1]
	
	lmin=limits[1]
	lmax=limits[2]
	
	if (ldiff<=2){
		lims=c(round(lmin-0.05,1),round(lmax+0.05,1)) } #round to 0.1
	if (ldiff>2 & ldiff<=10){
		lims=c(round(lmin-0.5),round(lmax+0.5)) }#round to 1
	if (ldiff>10 & ldiff<=50){
		lims=c(round((lmin-2.5)*2,-1)/2,round((lmax+2.5)*2,-1)/2) }#round to 5
	if (ldiff>50 & ldiff<=100){
		lims=c(round(lmin-5,-1),round(lmax+5,-1))} #round to 10
	if (ldiff>100 & ldiff<=200){
		lims=c(round((lmin-10)*5,-2)/5,round((lmax+10)*5,-2)/5) }#round to 20
	if (ldiff>200 & ldiff<=500) {
		lims=c(round((lmin-25)*2,-2)/2,round((lmax+25)*2,-2)/2) }#round to 50
	if (ldiff>500) {
		lims=c(round(lmin-50,-2)/2,round(lmax+50,-2)/2) }#round to 100
			
	if (lims[1]<floor.lim) { lims[1]=floor.lim }
	if (lims[2]>ceiling.lim) { lims[2]=ceiling.lim }
	
return(lims)}

## eg. ylim=padded.limits(limits,floor.lim=0)

## ----------------------------------------------------------------------------
#get stats on a vector
get.stats=function(v) {
	outquant=quantile(v,c(0.1,0.25,0.5,0.75,0.9),na.rm=TRUE,type=8)
	Min=min(v); Mean=mean(v); Max=max(v); SD=sd(v)
	stats=c(Mean,SD,Min,Max,outquant)
	return(stats)}


