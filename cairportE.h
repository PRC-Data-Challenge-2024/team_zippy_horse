      INTEGER iAIRPORT,NAIRPORT,NAIPRF,NAIPRNF,NAIRPqF
     $,NAPNF
      INTEGER,PARAMETER:: NAPNFx=3000
      parameter(NAIRPORT=21209)!8060
      character*4 airp(NAIRPORT),airportold,APNF(NAPNFx)
      real xairport(NAIRPORT),yairport(NAIRPORT),zairport(NAIRPORT)
      INTEGER FLhftairport(NAIRPORT),noairportfl(NAPNFx)
      INTEGER NAPUSED(NAIRPORT)
      real xdepold,xdesold,ydepold,ydesold,zdepold,zdesold
      character*4 depold,desold,depold0
      parameter(depold0='#+p#')
      common/cairport0/xairport,yairport,zairport,xdepold,xdesold
     $,ydepold,ydesold,zdepold,zdesold
     $,iAIRPORT
     $,FLhftairport,noairportfl,NAPUSED,NAIPRF,NAIPRNF,NAIRPqF,NAPNF
     $,airportold,airp,APNF,depold,desold
   
     