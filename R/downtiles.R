#' download tiles from a tile server using URL
#'
#' This function downloads tiles from a tile server using URL
#'
#' @param extent spatial extent to download tiles
#' @param level numerical vectors to indicate which zoom levels to be downloaded
#' @param down.wd character to indicate the path to store tiles
#' @param par integer to indicate how much processes to be executed
#' @import parallel
#' @export
#' @examples
#' library(geoChina)
#' xy.1<-wgs2gcj(wgsLon = 119.9135,wgsLat = 30.76719)
#' xy.2<-wgs2gcj(wgsLon =121.326,wgsLat = 32.03939 )
#' getTiles(extent=c(xy.1$lng,xy.2$lng,xy.1$lat,xy.2$lat),level=1:17)
#' serving tiles under the down.wd of getTiles()
#' library(leaflet)
#' leaflet()%>%addTiles(
#'     urlTemplate = 'http://127.0.0.1:8000/{z}/{x}/{y}.png')%>%
#'     setView(lng=121,lat =31,zoom=7)
getTiles <- function(extent=c(72,135,18,54),level=1:20,down.wd='/tmp',par=15) {
    extent.cn<-extent
    cat('indexing map tiles ....\n')
    levels<-vector('list',length(level))
    for (level_i in 1:length(level) ){
        left.top<-deg2num(lon_deg = extent.cn[1],
                          lat_deg = extent.cn[3],
                          zoom = level[level_i] )
        right.bottom<-deg2num(lon_deg = extent.cn[2],
                              lat_deg = extent.cn[4],
                              zoom = level[level_i])
        url<-vector(
            mode='character',
            length(left.top[1]:(right.bottom[1]+1))*
                length(left.top[2]:(right.bottom[2]+1)))

        paths<-vector('character',length(url))
        i<-0
        for(xx in left.top[1]:right.bottom[1] ) {
            for (yy in left.top[2]:right.bottom[2]){
                url[i<-i+1] <- paste0(
                    'http://webrd0',sample(1:4,1),
                    '.is.autonavi.com/appmaptile?lang=zh_cn&size=1&scale=1&style=',
                    ifelse(level[level_i]<=2,7,8),'&x=',
                    xx,'&y=',yy,'&z=',level[level_i])
                paths[i]<-paste0(paste(level[level_i],xx,yy,sep = '/'),'.png')
            }
        }
        levels[[level_i]]$url<-url
        levels[[level_i]]$paths<-paths
    }
    testmap<-do.call(rbind, lapply(levels, data.frame, stringsAsFactors=F))

    cat('building folder structure....\n')
    sapply(dirname(file.path(down.wd,testmap$paths)),dir.create,recursive = T)
    cat('downloading map tile....\n')
    sockconn<-showConnections(all=F)
    sockconn<-sapply(as.numeric(rownames(sockconn)),getConnection)
    sapply(sockconn,close)
    cl <- parallel::makeCluster(parallel::detectCores()*par)
    parallel::clusterMap(cl, download.file, url = testmap$url,
               destfile =file.path(down.wd,testmap$paths),.scheduling = 'dynamic',
               mode='wb',method='curl',
               extra='--silent --retry-delay 20 --retry-max-time 10 --retry 10')
    cat('finished....\n')
    parallel::stopCluster(cl)
    # if(require(servr)&&require(leaflet)){
        # show_map(center.x = mean( extent.cn[1],extent.cn[2],na.rm = T),
                 # center.y = mean(extent.cn[3],extent.cn[4],na.rm = T),
                 # zoom = mean(level))
        # library(servr)
        # server_config(dir = down.wd,port ='8000')
        # servr::httd(dir = down.wd,8000)
    # }

}

# show_map<-function(center.x=NULL,center.y=NULL,zoom=NULL,
#                    baseURL='http://127.0.0.1:8000/{z}/{x}/{y}.png'){
#     if(is.null(center.x)||is.null(center.y)||is.null(zoom)){
#       return()
#     }
#     library(leaflet)
#     leaflet()%>%addTiles(
#         urlTemplate =baseURL)%>%
#         setView(lng=center.x,lat =center.y,zoom=zoom)
# }

deg2num<-function(lat_deg, lon_deg, zoom){
    lat_rad <- lat_deg * pi /180
    n <- 2.0 ^ zoom
    xtile <- floor((lon_deg + 180.0) / 360.0 * n)
    ytile = floor((1.0 - log(tan(lat_rad) + (1 / cos(lat_rad))) / pi) / 2.0 * n)
    return( c(xtile, ytile))
}
