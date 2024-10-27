      SUBROUTINE AIRPORTINIT
      implicit none
      INTEGER I,N,J
      INCLUDE "CairportE.h"
!      open(5, file=
!     $'H:\Eigene Dateien\CoCip\COCIP2017\INPUT\Airports\'
!     $ //'airport.txt', action='read')     
!cc     https://www.world-airport-codes.com/
!      open(5, file=
!     $ 'D:\H-Lokal\Eigene Dateien\Corona\Trafficdata\EUROCONTROL/'
!     $ //'Airportsextended.txt', action='read')   
!      print*,' open done'     
!     $,'D:\H-Lokal\Eigene Dateien\Corona\Trafficdata\EUROCONTROL/'
!     $ //'Airportsextended.txt'
       open(5, file='INPUT/airportlist.txt', action='read')   
      print*,' open done'     
     $,'H:\Eigene Dateien\Corona_EUR\EUROCONTROL/'
     $ //'airportlist.txt'    
     
!     H:\Eigene Dateien\Corona_EUR\EUROCONTROL
! 21 Nov 2020: manual combination of 
! https://ext.eurocontrol.int/ddr/datasets
! ENV PreOPS AIRAC 2008 16JUL2020 With Airspace Closure.zip
! ENV PreOPS AIRAC 2008 16JUL2020 With Airspace Closure

! plus Teoh's airport_coordinates_2020.csv
! see  Airport_Theo.f 

! plus data for EUROCONTROL contest from internet 24 Aug 2024
          N=0
!      N777=0
      DO I=1,NAIRPORT
!  777 continue
      read(5,'(A4,1x,2F15.6)',end=999)airp(I),yairport(I),xairport(I)!,zairport(I) ,end=999
!      print*,' I,airp(I)',I,airp(I),yairport(I),xairport(I)
!      xairport(I)=xairport(I)/60.
!      yairport(I)=yairport(I)/60.
!      if(I.ge.8060) THEN
!      do J=1,I-1
!c!      print*,' I,J,airp(I),airp(J)',I,J,airp(I),airp(J)
!      if(airp(I).eq.airp(J)) THEN
!      xairport(j)=xairport(I)
!      yairport(j)=yairport(I)
!      print*,' goto 777'
!      N777=N777+1
!      goto 777
!      end if
!      end do
!      end if
!      N=I 
       N=I
      end do
  999 continue
! plus data for EUROCONTROL contest from internet 24 Aug 2024
       print*,' airports , N=', N
       N=n+1
       airp(N)='LTFO'
       xairport(N)=40.+49./60.+49./3600.
       yairport(N)=41.+10./60.+25./3600.
       N=n+1
       airp(N)='ZMCK'
       xairport(N)=106.+49.20/60.
       yairport(N)=47.+38.82/60.
       N=n+1
       airp(N)='HSSK'
       xairport(N)=32.+33.18/60.
       yairport(N)=15.+35.42/60.
       
      print*,' N,NAIRPORT',N,NAIRPORT
      if(N.ne.nairport) stop
!      print*,' N, NAIRPORT,N777',N, NAIRPORT,N777
!      open(7,file='airportlist.txt')
!      do I=1,N
!      write(7,'(A4,1x,2F15.6)') airp(I),yairport(I),xairport(I)
!      end do
!      print*,' airportlist done'
!      close(7)
!      stop
      airportold='Leer'
      NAIPRF=0
      NAIPRNF=0
      NAIRPqF=0
      NAPNF=0
      NAPUSED=0
      depold=depold0
      desold=depold0
      ydepold=-999.
      ydesold=-999.
      xdepold=-999.
      xdesold=-999.  
!      open(49,file='airport_notfound.txt')
      end
      SUBROUTINE AIRPORTINITcopy
      implicit none
      INTEGER I,N,J,N777
      INCLUDE "CairportE.h"
!      open(5, file=
!     $'H:\Eigene Dateien\CoCip\COCIP2017\INPUT\Airports\'
!     $ //'airport.txt', action='read')     
!c!     https://www.world-airport-codes.com/
!      open(5, file=
!     $ 'D:\H-Lokal\Eigene Dateien\Corona\Trafficdata\EUROCONTROL/'
!     $ //'Airportsextended.txt', action='read')   
!      print*,' open done'     
!     $,'D:\H-Lokal\Eigene Dateien\Corona\Trafficdata\EUROCONTROL/'
!     $ //'Airportsextended.txt'
      open(5, file'Airportsextended.txt', action='read')   
      print*,' open done'     
     $,'H:\Eigene Dateien\Corona_EUR\EUROCONTROL/'
     $ //'Airportsextended.txt'
     
!     H:\Eigene Dateien\Corona_EUR\EUROCONTROL
! 21 Nov 2020: manual combination of 
! https://ext.eurocontrol.int/ddr/datasets
! ENV PreOPS AIRAC 2008 16JUL2020 With Airspace Closure.zip
! ENV PreOPS AIRAC 2008 16JUL2020 With Airspace Closure

! plus Teorh's airport_coordinates_2020.csv
! see  Airport_Theo.f 
      N=0
      N777=0
      DO I=1,1000000!NAIRPORT
  777 continue
      read(5,*,end=999)airp(I),yairport(I),xairport(I)!,zairport(I)
      print*,' I,airp(I)',I,airp(I),yairport(I),xairport(I)
      xairport(I)=xairport(I)/60.
      yairport(I)=yairport(I)/60.
      if(I.ge.8060) THEN
      do J=1,I-1
!      print*,' I,J,airp(I),airp(J)',I,J,airp(I),airp(J)
      if(airp(I).eq.airp(J)) THEN
      xairport(j)=xairport(I)
      yairport(j)=yairport(I)
      print*,' goto 777'
      N777=N777+1
      goto 777
      end if
      end do
      end if
      N=I
 
      end do
  999 continue
      print*,' N, NAIRPORT,N777',N, NAIRPORT,N777
      open(7,file='airportlist.txt')
      do I=1,N
      write(7,'(A4,1x,2F15.6)') airp(I),yairport(I),xairport(I)
      end do
      print*,' airportlist done'
      close(7)
      stop
      airportold='Leer'
      NAIPRF=0
      NAIPRNF=0
      NAIRPqF=0
      NAPNF=0
      depold=depold0
      desold=depold0
      ydepold=-999.
      ydesold=-999.
      xdepold=-999.
      xdesold=-999.  
!      open(49,file='airport_notfound.txt')
      end
       subroutine findairport(airport,xap,yap)
      implicit none
      integer I,J,NUsed
      character*4 airport
      real xap,yap
      INCLUDE "CairportE.h"
!      print*,' airport',airport
      if(airport.eq.airportold) THEN
      xap=xairport(1)
      yap=yairport(1)      
      NAPUSED(1)=NAPUSED(1)+1
!      flap=FLhftairport(1)
      NAIRPqF=NAIRPqF+1
!      print*,' NAIRPqF',NAIRPqF
      return
      else
      do I=1,Nairport
      if(airport.eq.airp(I)) THEN
!      print*,' airport,I ',airport,I
      xap=xairport(I)
      yap=yairport(I)
      NUsed=NAPUSED(I)+1
  
!      flap=FLhftairport(I)
      do J=I,2,-1
      airp(J)=airp(J-1)
      xairport(J)=xairport(J-1)
      yairport(J)=yairport(J-1)
      NAPUSED(J)=NAPUSED(j-1)
!      FLhftairport(J)=FLhftairport(J-1)
      end do
      airp(1)=airport
      xairport(1)=xap
      yairport(1)=yap
      NAPUSED(1)=NUsed
!      FLhftairport(I)=flap
!      xap=xairport(1)
!      yap=yairport(1)
!      flap=FLhftairport(1)
      airportold=airport
      NAIPRF=NAIPRF+1
      return
      end if
      end do
      xap=-999.
      yap=-999.
!      flap=-99999
      call addairportnotfound(airport)
      NAIPRNF=NAIPRNF+1
!      print*, 'airport not found (quick,found,not) ' 
!     $ ,airport,' ',NAIRPqF,NAIPRF,NAIPRNF
!      write(49,'(I10,1x,a4)') NAIPRNF,airport
      return
      end if
      end
      
      subroutine findairportdepdes(dep,des,xdep,ydep,xdes,ydes)
      implicit none
      integer J
      character*4 dep,des
      real xdep,ydep,xdes,ydes
      INCLUDE "CairportE.h"
      if(depold.ne.depold0) THEN
      if(dep.eq.depold.and.des.eq.desold) THEN
      xdep=xdepold
      xdes=xdesold
      ydep=ydepold
      ydes=ydesold
      return
      end if
      end if
      call findairport(dep,xdep,ydep)
      call findairport(des,xdes,ydes)
      xdepold=xdep
      xdesold=xdes
      ydepold=ydep
      ydesold=ydes
      depold=dep
      desold=des
    
      end
      
      subroutine addairportnotfound(airport)
      implicit none
      integer I,ifound
      character*4 airport
      INCLUDE "CairportE.h"
      if(NAPNF.eq.0) then
      NAPNF=1
      APNF(1)=airport
      noairportfl(1)=1
      return
      end if
      ifound=0
      do I=1,NAPNF
      if(APNF(I).eq.airport) THEN
      ifound=I
      noairportfl(I)=noairportfl(I)+1
      exit
      end if
      end do
      if(ifound.eq.0) then
      NAPNF=NAPNF+1
      if(NAPNF.gt.NAPNFx) THEN
      print*,' addairportnotfound: NAPNF.gt.NAPNFx ',NAPNF
      stop
      end if
      APNF(NAPNF)=airport
      noairportfl(NAPNF)=1
      end if
      end
      subroutine outairportnotfound
      implicit none
      integer I,NAPUSEDtot
      INCLUDE "CairportE.h"
      close(18)
      open(18,file='airportnotfound.txt')
      write(18,'(a6,1x,a4,1x,a6)') 'No', 'Airp','number'
      print*,' number of unknown airports=', NAPNF
      if(NAPNF.gt.0)print*, ' see file: '//'airportnotfound.txt'
      do I=1,NAPNF
      if(apnf(I).eq.'AFIL') THEN
      print*,' AFIL =airfield  generic name'
      else
      write(18,'(I6,1x,a4,1x,I6)') I,APNF(I),noairportfl(I)
      end if
      end do
      close(18)
      NAPUSEDtot=0
      do I=1,NAIRPORT
      if(NAPUSED(I).gt.0)then
!      print*,' I,airp(I)',I,airp(I),NAPUSED(I)
      NAPUSEDtot=NAPUSEDtot+NAPUSED(I)
      end if
      end do  
      print*,' NAPUSEDtot',NAPUSEDtot
      
      end
      