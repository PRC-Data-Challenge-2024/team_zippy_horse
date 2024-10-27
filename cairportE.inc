      INTEGER iAIRPORT,NAIPRF,NAIPRNF,NAIRPqF,NAPNF
       INTEGER,PARAMETER:: NAIRPORT=10500+3
      INTEGER,PARAMETER:: NAPNFx=3000
      character*4 airp(NAIRPORT),airportold,APNF(NAPNFx)
      real xairport(NAIRPORT),yairport(NAIRPORT)
      INTEGER FLhftairport(NAIRPORT),noairportfl(NAPNFx)
      INTEGER NAPUSED(NAIRPORT)
      real xdepold,xdesold,ydepold,ydesold
      character*4 depold,desold
      character*4,Parameter :: depold0= '#+p#'
      common/cairport0/xairport,yairport,xdepold,xdesold &
     ,ydepold,ydesold &
     ,airportold,airp,APNF,depold,desold &
     ,iAIRPORT &
     ,FLhftairport,noairportfl,NAPUSED,NAIPRF,NAIPRNF,NAIRPqF,NAPNF