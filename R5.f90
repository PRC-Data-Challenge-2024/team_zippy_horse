! R5 tests the analysis method used in R6, here for the given tow in the FFLISTC data
! coded by Ulrich Schumann, Oct 2024
! makes use of the PS model data and the airport data and 
! the input prepared in jobs R2 to R4. 
! coded by Ulrich Schumann, Oct 2024

 MODULE mo_types
  type TFLIGHT ! 
    INTEGER No,flight_id
     character*10 timestamp
     character*14 timestamp2
     real latitude,longitude,altitude,groundspeed,track,vertical_rate 
     INTEGER icao24
     real u_component_of_wind,v_component_of_wind  &
     ,temperature,specific_humidity 
  end type  TFLIGHT   
  
  type TFLIGHTadd ! 
    INTEGER ITW,ioriginal,INVALID
    REAL segmW,cosaw,sinaw,TASW,GSDISTW,dgsdtW,Machw,CLRW  &
    ,massw(3),ffw(3),massfrom(5)
  

  end type  TFLIGHTadd   
  
  type TFLIGHTaddreduced! 
    INTEGER ITW,ioriginal
   end type  TFLIGHTaddreduced   
  
  
       
     type TFLIGHTLISTs ! 

      INTEGER flight_id
     CHARACTER*10 cdatex
     CHARACTER*32 callsign
     character*4 adep
     character*40 name_adep
     character*2 country_code_adep
     character*4 ades
     character*40 name_ades
     character*2 country_code_ades
     character*20 actual_offblock_time
     character*20 arrival_time
     character*4 aircraft_type
     character*1 wtc ! weightclass
     character*32 airline
     Integer flight_duration,taxiout_time,flown_distance 
      real tow   
     REAL xdep,ydep,zdep,xdes,ydes,zdes
     INTEGER itwdep,itwdes,IFLinitialcruise,IFLmax
  
   
  end type  TFLIGHTLISTS  
  
  
     type Tclimbdata      
     real  clrm,a12,a22,b1,b2,x1,x2 
     INTEGER nclr
     end type Tclimbdata
  
  
END MODULE mo_types

MODULE mo_directories ! many code details
  USE mo_types,only: TFLIGHT,TFLIGHTLISTS,TFLIGHTadd,Tclimbdata
  IMPLICIT NONE
      Type(TFLIGHT),DIMENSION(:),ALLOCATABLE      :: FLIGHT ! data
      TYPE(TFLIGHTLISTS) :: FLLISTS 
        type (Tclimbdata),DIMENSION(:,:),ALLOCATABLE  :: climbdataca
     TYPE(TFLIGHTadd),DIMENSION(:),ALLOCATABLE :: FLIGHTadd
     INTEGER IUTC0,istopall,nwmax,NPLOT,IPS
      INTEGER,PARAMETER ::idtmax0=1
     INTEGER,PARAMETER::nfcmax=369013
     INTEGER,PARAMETER:: NWX=nfcmax
     INTEGER,PARAMETER::nfsmax=nfcmax!158149 
     INTEGER,PARAMETER:: IDT0=1
     INTEGER,PARAMETER:: Nsmooth=1!!!0
     REAL :: distlimit
     real,parameter:: clrmax=60.
     real,parameter:: clrmin=-30.
     real,parameter:: flUAS =1000. ! lower limit of upper air space in feet 
      real,parameter::  flmin=-500.
     INTEGER,PARAMETER::NMethod=5
     INTEGER,PARAMETER:: NPS=69  !!!! NPS=NPS in psbeta3.inc
     integer,parameter:: nairlinex=50
     real, dimension(NPS):: clrmintyp,clrmaxtyp
  
       real,parameter:: minfactor=0.90,maxfactor=1.0
     real,DIMENSION(Nmethod,NPS,nairlinex):: meanerrorac
     
     INTEGER IFLinitialcruise,IFLmax
     
     INTEGER nflylow
  
        real massxfromLF(nfcmax)
        real massresult
         real meanLFd(365)
         integer imassvalid(6)
         real massresult6(6)
         INTEGER Is
 
         INTEGER Jairline
          logical, parameter:: LPR=.false.
          real clrout
          INTEGER nclrout
          REAL flightairdist
           INTEGER,DIMENSION(nps,nairlinex)::ibestm
   

END MODULE mo_directories 


module mo_cocip_parameters
!  constant  model parameters

  INTEGER IACNUMBcc
  REAL,PARAMETER:: CP=1004. ! specific heat capacity of air in J/(kg K)
  REAL,PARAMETER:: G=9.80665 ! gravity
  REAL,PARAMETER:: P0=101325.! surface pressure in Pa
  REAL,PARAMETER:: GAMMA=1.4! diabatic coefficient
  REAL,PARAMETER:: PI=3.141592653589793,PIR180=1.745329251994330E-02
  REAL,PARAMETER:: RADIUS=6371229. ! Earth radius in m
  REAL,PARAMETER:: UNITY=PIR180*RADIUS ! distance per longitide in m at equator
  REAL,Parameter:: RHOICE=917. ! Ice particle density in kg/m3
  REAL,Parameter:: CPICE=2050. ! specific heat capacity of ice in J/(kg K)
  REAL,Parameter:: HLATENTI=2800.E3 ! latent heat of ice to vapor melting in J/kg
  REAL,PARAMETER:: FEXTSW=7.08286238E6 ! extinction per size for SW radiation
!      AK=1.31! real refractive index
!      AL=0.55E-6! wave length / m
!      FEXTSW/LW=4.*pi*(AK-1.)/AL mit AL=ALSW/LW
  REAL,PARAMETER:: betaT=0.0065 ! = -(mean tropospheric lapse rate) in K/m
  REAL,PARAMETER:: EIH2Oref=1.237  ! H2O emission index for reference JetA1 fuel)
  real,PARAMETER:: RVOLZUREFF=0.9 !  rvol/reff
  REAL,PaRAMETER:: mHref=0.138!  reference hydrogen mass fraction
!     REAL,PARAMETER:: mH =0.153! example value for an actual (SAF) hydrogen mass fraction
  REAL,PARAMETER:: mH =mHref! Hydrogen content of jet-A1 reference fuels (ICAO8888)
  REAL,PARAMETER:: mc=1.-mh! Carbon content
  real,Parameter:: molo=15.99,molH=1.00797,molC=12.011,molN=14.007 ! molar masses in g/mol
  real,Parameter:: molCO2=2.*molo+molc,molair=28.97,molno2=2.*molo+moln
  REAL,PARAMETER:: EIH2OCC=mh*(2.*molH+molO)/(2.*molH)! water vapor (H2O) emission index
  REAL,Parameter:: LCVref=43.1E6! J/kg! lower combustion heat for Jet-A1 kerosene (reference value)
  REAL,PARAMETER:: LCVCC=43.E6!LCVref + 1.07E6*(mH-mHref)/0.015!(see Teoh et al.,EST,2022)
! in J/kg! Latent heat of combustion = Lower Calorific Value
  REAL,PARAMETER:: R0=287.05!  air gas constant in J/(kg K),from WMO
  REAL,PARAMETER:: R1=461.51! water vapor gas consntant in J/(kg K),from WMO 23
! World Meteorological Organization,2011
! General Meteorological Standards and Recommended Practices. (Updated in 2012),
! Technical Regulations,Volume I,Appendix A (WMO-No. 49),Geneva
! http://library.wmo.int/pmb_ged/wmo_49-v1-2012_en.pdf
  REAL,PARAMETER:: TKSL=288.15! surface temperature of ICAO standard atmosphere (ISA)
  REAL,PARAMETER:: HTP=11000. ! tropopause heigh of ISA
  REAL,PARAMETER:: TKTP=TKSL-HTP*betaT ! tropopause temperature in ISA in K
  REAL,PARAMETER:: PTP= 22631.7009  ! ISA tropopause pressure in Pa
  REAL,PARAMETER:: cpe_cp=1.2378
  REAL,PARAMETER:: cpe=cpe_cp*cp
  REAL,PARAMETER:: cpkero=2.*cp
  REAL,PARAMETER:: etacombustion=0.99
 
end module mo_cocip_parameters


PROGRAM main

  USE mo_directories,ONLY: FLIGHT,FLLISTS,FLIGHTadd  &
  ,NWX,NPLOT,IPS,distlimit  &
   ,nmethod  &
  ,massxfromLF,meanLFd  &
  ,NPS,is  &
  ,climbdataca  &
  ,lpr,nairlinex &
  ,clrmintyp,clrmaxtyp,nfcmax,ibestm
  USE mo_types,only: TFLIGHTLISTS
  IMPLICIT NONE
  INCLUDE "CPARA.inc"
  INCLUDE "psbeta3.inc"
  INTEGER istat,I,J  
  INTEGER JDAYJ0,IDAY
  real time0,time1,timetot
 
   
   
        
  CHARACTER*10 cdate    
  INTEGER IY,IMONTH,IDAYM  
  INTEGER  IWR,ILINE
  INTEGER yyyy,mm,dd,hh,minmin,sec  
  
  
   CALL CPU_TIME(time0)
             
  
 Print*,'#################################################'
 
     
    
  ALLOCATE(FLIGHT(NWX),stat=istat)
    Print*,' ALLOCATE: FLIGHT(NWX),stat=',istat 
  ALLOCATE(FLIGHTadd(NWX),stat=istat)
    Print*,' ALLOCATE: FLIGHTadd(NWX),stat=',istat 
 
 allocate(climbdataca(NPS,nairlinex),stat=istat) 
 Print*,' ALLOCATE: climbdataca(NPS,nairlinex),stat,NPS=',istat,NPS,nairlinex  
 
   clrmintyp(1:NPS)=5.
   clrmaxtyp(1:NPS)=15.
   timetot=0.
   
      open(5,file='Results2/ibestm.txt',action='read')
       do I=1,nairlinex
       read(5,'(100I2)') (ibestm(J,I),J=1,NPS)
       end do
       close(5)
       
      open(5,file='git/MassLF3.txt',action='read')
      if(LPR) print*,' nfcmax',nfcmax
      do I=1,nfcmax ! in R5: nfc!!!
      read(5,*) ILINE,massxfromLF(I)
       end do
      close(5)
      open(5,file='git/LFYEARs3.txt',action='read')
      do I=1,365
      read(5,*) Iline,iline,meanLFd(I)     
      end do
      close(5)
      print*,' done nfcmax',nfcmax
   
       call PSbeta3INIT
 
!https://www.embraercommercialaviation.com/wp-content/uploads/2017/02/Embraer_spec_195_web.pdf
   
!   A quick response to the mass ration
!
!E75S!!
!
!MZFM/MTOM is 0.8453 and OEM/MTOM is 0.5733
!
!E75L
!
!MZFM/MTOM is 0.81722 and OEM/MTOM is 0.55427
!
!There is a problem with the GLF5 because it is a business jet OEM is not defined.
!
!However, try these
!
!MZFM/MTOM is 0.6017 and OEM/MTOM is 0.5091
!bThe certified MMO for the B763 is 0.86 .




!######################################################################       
   
 

     
!We need to correct the PS table value of the MTOM for the B763: present value : 158758 kg. Correct value: 181.437 kg.
!See https://www.flugzeuginfo.net/acdata_php/acdata_7673_en.php
!Also check MMo (present 086. Correct: 0.80).
!And possibly other values.
!https://www.flugzeuginfo.net/acdata_php/acdata_7772_en.php

      call sloopflights

   
   deallocate(FLIGHT)
 
 
   
             CALL CPU_TIME(time1)
      timetot=(time1-time0)
     
 
     print*,' all DAY LOOPS timetot',timetot
  
  
     print*
      print*,' ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
     
  deallocate(climbdataca)

  
         
   end PROGRAM  main
  
    subroutine sloopflights
    
  USE mo_directories,ONLY: FLIGHT,FLLISTS,FLIGHTadd  &
  ,IUTC0,NWX,istopall,nwmax,NPLOT,IPS,distlimit  &
  ,NSMOOTH  &
  ,nmethod  &
  ,massxfromLF,meanLFd,imassvalid  &
  ,Jairline,NPS,IS  &
  ,climbdataca  &
  ,lpr,nairlinex,massresult6,clrout,nclrout,nfcmax,nfsmax,flightairdist &
  ,meanerrorac,IFLinitialcruise,IFLMAX,ibestm
  USE mo_types,only: TFLIGHTLISTS
  IMPLICIT NONE
  INCLUDE "CPARA.inc"
  INCLUDE "psbeta3.inc"
  INTEGER istat,NFL,NW,I,J,K  &
  ,ngoto888,NFLstep,NFLstepmax,itlastmass
  INTEGER*8 NWTOT
  INTEGER NFOUNDS,JDAYJ0,IDAY
  real time0,time1,times,timec
    real timessorttime,timeFLIGHTsort,timel1,timel0,fload
   
   real  mass(3),mach,FL,dhdt,dvdt,DTISA,ff,massx,massxCL,massxCT  &
   ,eta,etaLD,CL,CD,mfperm,temp,dvdttemp(NWX)
   real distance_between_points,distL,dh,windspeedmax,RT,er
 logical Lflightready(nfsmax)

  real*8 rms50
  Integer ISEQUENCE(5)
  Integer*8 nwr50
  real rms
        
  CHARACTER*10 cdate    
  INTEGER IY,IMONTH,IDAYM,Nout,Nlong,nwold,NITER,nlast  &
  ,ididt,Imass,Imethod,IWR49
  INTEGER  Imachhigh,IFLhigh,Ifl1,Ifl2,I2day,IWR
  INTEGER yyyy,mm,dd,hh,minmin,sec  &
  ,idt,nairlinemax,Imassclfound
  REAL  machmax,flmax,ffold,clr,rmsl
  real  massCL,massCT
       
 
  LOGICAL,PARAMETER:: LWRITE17=.false.
  LOGICAL,PARAMETER:: LWRITE7=.true.
  LOGICAL LNEWoem
  
  REAL,PARAMETER:: drittel=1./3.
  ISEQUENCE=0
   do I=1,5
    isequence(I)=I
   end do
   i=isequence(3)
   isequence(3)=isequence(4)
   isequence(4)=i
   rms50=0.D0
   nwr50=0
  ngoto888=0
  
              CALL DAYJ(2021,12,31,JDAYJ0,3)
       IUTC0=(JDAYJ0-1)*24*3600
 !      print*,' JDAYJ0',JDAYJ0
       istopall=0
        nwmax=0
         NPLOT=0
         NFLstep=0
       
         NFL=0
  NWTOT=0
        
 
  
       
  
             CALL CPU_TIME(time0)
             if(LWRITE17) then
               open(17,file='tesflR5test.txt')
               
                    write(17,*) 'No ','flight_id '  &
      ,'timestamp '  &
      ,'lat/deg ','lon/deg '  &
      ,'alt/ft ','GS_given/(m/s) '  &
      ,'track ','CLR_given/(m/s) '  & 
      ,'u_wind/(m/s) ','v_wind/(m/s) '  &
      ,'temperature/K ','humidity/(g/g) '  &
        ,'time_1Jan2000/s ','original ','Invalid '  &
     ,'segm_length/m ','cosa ','sina ','TAS/(m/s) ','GS_computed/(m/s) '  &
     ,'dv/dt/(m/s2) ','Mach ','CLR_computed/(m/s) '  &
      ,'massCLO/kg ','massref/kg ','Massmax/kg '  &
     ,'ffmin/(kg/s) ','ffref/(kg/s) ','ffmax/(kg/s) '   &
     ,'massxL/kg ','massxT/kg ',' massxCLo/kg ',' massxxxx/kg '  &
     ,'Err_massL ','Err_massT ','Err_massCLo ','Err_massxxx '  &
     ,'time/s '  &
     ,'time_FL1/s ','FL1/ft ',' time_FL2/s ','FL2/ft '
     end if
       
 
  Lflightready=.false.
  open(77,file='flighreadStest',form='unformatted')
  write(77) Lflightready
  close(77)
  
           OPEN(20,File='PMTOM_OEM_outofrangetest.txt')
        write(20,*) 'flight_id ',' test ',' ATYP ',' IPS ',' value ','(IPS) ','number '
 
 
  open(49,file='oemerrorstest.txt')
      write(49,*) 'icao ' ,'IPS ','jairline ' &
      , 'itime_sincestart/s ' ,'duration/s ','flown_distance/nml ' &
            , 'TOWest/kg ', 'imass ',' mass/kg ', 'OEM/kg ','MTOM/kg ','MZFM/kg ','flight_id '


   OPen(51,file='git/RMStestc.txt')
   write(51,*) 'NWR ','Iday ','Ips ','Jairline ',' flight_id ','tow ','error ','airdist/m ' ,'grounddist/m ','ICAO '
   
        NFOUNDS=0
    nairlinemax=0
 LOOPday: do iday=1,365!271,365!365!262!262!262!1,2!5!1,1!1,1!2
 
  open(77,file='flighreadStest',form='unformatted',action='read')
  read(77) Lflightready
  close(77)
       call monday(JDAYJ0+iday,IY,IMONTH,IDAYM)
       IUTC0=(JDAYJ0+iday-1)*24*3600
       write(cdate,'(I4,A1,I2.2,A1,I2.2)')IY,'-',IMONTH,'-',IDAYM
       print*,' cdate',cdate
  
 
 

            
  
  open(50,file='F:\FLLISTCtest\'//'outmassS_'//cdate//'.txt')
       write(50,*) 'rms ','flight_id ','ICAO ','IS ','IPS ','Jairline ' &
 ,'imassvalid1 ','imassvalid2 ','imassvalid3 ','imassvalid4 ' &
 ,'imassvalid5 ','imassvalid6 ','Ibestm '  &
 ,'massresult1/kg ','massresult2/kg ','massresult3/kg ' &
 ,'massresult4/kg ','massresult5/kg ','massresult6/kg ' & 
 ,'clr/(m/s) ','nclrsteps  ','massLF/kg ','given_TOW/kg ' &
 , 'FLmax/feet ', 'PMALTmax/feet ' &
  ,'flownairdist/m ','flown_Gdistance/m ' &
 ,'err1 ','err2 ','err3 ','err4 ','err5 ','err6 '
    
  NW=0
 
  Nout=0
    do IPS=1,NPS
   do jairline= 1, nairlinex
  climbdataca(IPS,Jairline)%clrm=0.
      climbdataca(IPS,Jairline)%a12=0.
      climbdataca(IPS,Jairline)%a22=0.
      climbdataca(IPS,Jairline)%b1=0.
      climbdataca(IPS,Jairline)%b2=0.
      climbdataca(IPS,Jairline)%nclr=0.
      climbdataca(IPS,Jairline)%x1=0.
      climbdataca(IPS,Jairline)%x2=0.
      end do
      end do
       
 
    open(5,file='F:\FLLISTCout2/climbdata2',form='unformatted',action='read')
   read(5)climbdataca
   close(5)
    open(5,file='F:\FLLISTCout2/meanerrorac',action='read')
      do I=1,nairlinex
       do K=1,Nmethod
       read(5,'(100E15.7)') (meanerrorac(K,J,I),J=1,NPS)
       end do
       end do
   close(5)


  print*,' open tried','F:\FLLISTS\'//cdate !!!'EU/FLC'//cdate
  
  OPen(5,file='F:\FLLISTC\'//cdate,action='read',form='unformatted') 
   print*,' opened','F:\FLLISTC\'//cdate !!!'EU/FLC'//cdate

 
       NFLstepmax=1000000
  dowhileilast: do WHILE (NFLstep.lt.NFLstepmax) 
   111  continue
   
    
   
        windspeedmax=0.
  
    
       
!            read(5,END=999)FLLISTS,NW,IS,Jairline
            IFLinitialcruise=0
             IFLMAX=0
            read(5,END=999)FLLISTS,NW,IS,Jairline
              if(LPR) print*,'FLLISTS',FLLISTS
            print*,'NW,IS,Jairline,nwr50',NW,IS,Jairline,nwr50
           
      
            nairlinemax=max(nairlinemax,Jairline)
       I2day=0
            imassvalid=0
            massresult6=0.
 
            
      
            
 
        if(LPR) print*,' NW,IS,Jairline ',NW,IS,Jairline
        if(LPR) print*,'FLLISTS ',FLLISTS    
        
        I=0
        if(I.gt.0 .and. lpr ) then
        
       Print*,'flight_id ',FLLISTS%flight_id
    Print*,' cdatex ',FLLISTS%cdatex
    
     Print*,' callsign ',FLLISTS%callsign
   
     Print*,' adep ',FLLISTS%adep
    
     Print*,' name_adep ',FLLISTS%name_adep
  
      Print*,'  country_code_adep ',FLLISTS%country_code_adep
   
     Print*,'  ades ',FLLISTS%ades
  
     Print*,'  name_ades ',FLLISTS%name_ades
 
     Print*,'country_code_ades ',FLLISTS%country_code_ades
   
     Print*,'  actual_offblock_time ',FLLISTS%actual_offblock_time
 
     Print*,' arrival_time/s ',FLLISTS%arrival_time
    
     Print*,'  aircraft_type ',FLLISTS%aircraft_type
    
     Print*,'  wtc ',FLLISTS%wtc
   
     Print*,'  airline ',FLLISTS%airline
     
 
     Print*,'  flight_duration/min ',FLLISTS%flight_duration
     Print*,'  taxiout_time/min ',FLLISTS%taxiout_time
     Print*,'  flown_distance/nml ',FLLISTS%flown_distance
    
!????????????????????      Print*,'  tow/kg ',FLLISTS%tow
 
     Print*,'  xdep ',FLLISTS%xdep
     Print*,'  ydep ',FLLISTS%ydep
     Print*,'  zdep/feet ',FLLISTS%zdep
     Print*,'  xdes ',FLLISTS%xdes
     Print*,'  ydes ',FLLISTS%ydes
     Print*,'  zdes(feet ',FLLISTS%zdes
 
      Print*,'  itwdep/s ',FLLISTS%itwdep  
      Print*,'  itwdes/s ',FLLISTS%itwdes  
 !     Print*,'  IFLinitialcruise/feet ',IFLinitialcruise  
 !     Print*,'  IFLmax/feet ',IFLMAX  
  
      end if
     
     
    
       do I=1,NW
       read(5) FLIGHT(I)
        windspeedmax=max(windspeedmax,FLIGHT(I)%u_component_of_wind**2+FLIGHT(I)%v_component_of_wind**2)
       windspeedmax=sqrt(windspeedmax)
        NW=I
        end do  
     
               
       windspeedmax=sqrt(windspeedmax)
       if(windspeedmax.gt.200.) then
       print*,' windspeedmax.gt.200.,windspeedmax,nw=',  &
       windspeedmax,nw  &
       ,' FLIGHT(1)%flight_id= ',FLIGHT(1)%flight_id
       stop
       end if
      
 !       if(248751880 .ne. FLLISTS%flight_id) goto 111
!        istop=1
           if(LPR) print*,' I2Day,NW',I2Day,NW,' windspeedmax',windspeedmax
      
!             distlimit=sqrt(1.4*R0*240.)+windspeedmax
             distlimit=310.+windspeedmax
 !            sqrt(1.4*R0*240.)=310.56
           
    
           if(LPR) print*
        call sFLIGHTadd(NW) ! ! reads integer time ITW and sets IORIGINAL
        
      
     nwgt1: if(NW.gt.30) then
       NFL=NFL+1 
!!!!       if(NFL.gt.100 )exit
       NFLstep=NFLstep+1
  
        
      NWTOT=NWTOT+NW
!      if(NFL.gt.10) goto 111
      
 !!     

      I=0
     ifigt0pr: if(I.gt.0 .and. LPR ) then      
     print*,' FLLISTS%itwdep-iutc0,FLLISTS%xdep,ydep'  &
     ,FLLISTS%itwdep-iutc0,FLLISTS%xdep,FLLISTS%ydep
         do I=1,max(1,NW/10)
      print*,'nwgt1: I,it,lon,lat,alt,gs,clr',FLIGHT(I)%flight_id, &
     I,FLIGHTadd(I)%ITW-IUTC0,FLIGHT(I)%longitude,FLIGHT(I)%latitude  &
     ,FLIGHT(I)%altitude,FLIGHT(I)%groundspeed,FLIGHT(I)%vertical_rate 
      end do
      print*
     do I=max(1,NW/10)+1,(NW*9)/10,max(1,(8*NW)/10000)
      print*,'nwgt1: I,it,lon,lat,alt,gs,clr',FLIGHT(I)%flight_id, &
      I,FLIGHTadd(I)%ITW-IUTC0,FLIGHT(I)%longitude,FLIGHT(I)%latitude  &
      ,FLIGHT(I)%altitude,FLIGHT(I)%groundspeed,FLIGHT(I)%vertical_rate 
      end do
      print*
       do I=(NW*9)/10+1,NW
      print*,'nwgt1: I,it,lon,lat,alt,gs,clr',FLIGHT(I)%flight_id, &
      I,FLIGHTadd(I)%ITW-IUTC0,FLIGHT(I)%longitude,FLIGHT(I)%latitude  &
      ,FLIGHT(I)%altitude,FLIGHT(I)%groundspeed,FLIGHT(I)%vertical_rate 
      end do
                print*,' FLLISTS%itwdes-iutc0,FLLISTS%xdes,ydes'  &
     ,FLLISTS%itwdes-iutc0,FLLISTS%xdes,FLLISTS%ydes
      Print*
      Print*,' if(FLIGHT(I)%groundspeed.le.0.) start :,NW=',NW
      do I=1,NW
      if(FLIGHT(I)%groundspeed.le.0.) then
      print*,'zero: I,it,lon,lat,alt,gs,clr',FLIGHT(I)%flight_id, &
      I,FLIGHTadd(I)%ITW-IUTC0,FLIGHT(I)%longitude,FLIGHT(I)%latitude  &
      ,FLIGHT(I)%altitude,FLIGHT(I)%groundspeed,FLIGHT(I)%vertical_rate
       end if      
      end do
      print*,' if(FLIGHT(I)%groundspeed.le.0.) then   done'

     
      stop
      end if ifigt0pr
!   
!    print*,' FLLISTS%itwdes-iutc0,FLLISTS%xdes,ydes'  &
!    ,FLLISTS%itwdes-iutc0,FLLISTS%xdes,FLLISTS%ydes
! 
!      print*,' FLLISTS',FLLISTS
      Print*,' NFL',NFL
   
!      stop
   
   
       call eliminate_zerogs(NW) ! ! eliminates  data points with zero ground speed
         if(LPR) print*,' after eliminate_zerogs,nw,nfl',nw,nfl
!         stop
      if(NW.lt.10) then  
    
      Print*,' Nw.lt.10',NW
        
    

!      stop
      ngoto888=ngoto888+1
      print*,' goto 888 1'
      goto 888
        end if     
!
                    
        call addends(NW) ! includes airport coordinates at beginning and end of trajectory

      
      

      if(LPR) print*,'  vor LICgt0,nfl',nfl
    
           call ACNUMBPSbeta(FLLISTS%aircraft_type,IPS) ! find IPS number for given aircrat-type
        if(IPS.eq.0 .or.ips.gt.nps) then
        print*,' IPS.eq.0',IPS
        stop
        end if
        
        do I=1,NW
              FLIGHT(I)%altitude=MIN(FLIGHT(I)%altitude,PMalthft(IPS)*100.)
        end do
              
!?????????????????        if(FLLISTS%TOW.gt. 1.01*PMTOM(IPS))then
!  write(20,*)FLIGHT(1)%flight_id,' MTOM  ',FLLISTS%aircraft_type,' ',IPS,FLLISTS%TOW,PMTOM(IPS)
!         end if
!        if(FLLISTS%TOW.lt. POEM(IPS))then
!  write(20,*)FLIGHT(1)%flight_id,' OEM   ',FLLISTS%aircraft_type,' ',IPS,FLLISTS%TOW,POEM(IPS)
!          print*, 'TOW<oem,ID=',FLIGHT(1)%flight_id,' OEM   ',FLLISTS%aircraft_type,' ',IPS,FLLISTS%TOW,POEM(IPS)
!           end if
        
        if(nw.lt.10) then
        print*,' IPS=0,NW',IPS,NW
     ngoto888=ngoto888+1
  print*,' goto 888 2'
      goto 888
       end if         
      
 
 
   NFOUNDS=NFOUNDS+1
  
    if(LPR)Print*,' before correctend  Nw,nfl',NW,nfl  
   if(LPR) print*,'FLLISTS%flight_id',FLLISTS%flight_id,' NFL',NFL

         call correctend(NW) 
! eliminates unrealistic FL changes at the end of the flight and extends flights to airports
    if(LPR) then
       print*,'after  correctend: flight(1)%flight_id',flight(1)%flight_id 
       print*,' after corretend,flight(1)%flight_id',flight(1)%flight_id
       print*,'1stpart  Nw,nfl',NW,nfl           
        print*,' FLLISTS',FLLISTS
        print*,' after correctend,nw,nfl',nw,nfl
    end if
      Print*,' NFL',NFL
      
   
    
    do I=1,NW
    flightadd(I)%INVALID=0
    if(FLIGHT(I)%specific_humidity.eq.0) then
    flightadd(I)%INVALID=1
    flightadd(max(1,I-1))%INVALID=1
    flightadd(min(NW,I+1))%INVALID=1
    end if
    end do
    
  
    do I=1,NW-1
    distL=distance_between_points(FLIGHT(I)%longitude,FLIGHT(I)%latitude  &
    ,FLIGHT(I+1)%longitude,FLIGHT(I+1)%latitude)*RADIUS
    dh=FLIGHT(I+1)%altitude-FLIGHT(I)%altitude  
    ididt=(flightadd(I+1)%ITW-flightadd(I)%ITW)
    ididt=max(ididt,1)
    if(LPR) then
    if(DISTL.lt.10.) print*,'DISTL.lt.10. meter,,DISTL,Ididt,nfl',DISTL,Ididt,nfl
    if(abs(dh)/ididt .gt.50.)  print*,'abs(dh)/ididt .gt.50.,nfl',dh,nfl
     end if
    end do

    if(LPR) print*,' nw before sdistcheck,NW,nfl',NW,nfl
   
     call sdistcheck(NW) ! if dist .lt. 10 -> invalid

     call eliminate_Tjumps(NW) !  eliminates waypoints with unrealistic temperature jumps
     if(LPR) print*,' nw before eliminateINVALID,nw,nfl',NW,nfl
     call eliminateINVALID(NW) ! eliminates as invalid flagged points
        
    if(LPR) print*,' before FLIGHTsort,nw,nfl',NW,nfl
        if(NW.lt.10) then 
         Print*,' Nw.lt.10',NW
         ngoto888=ngoto888+1 
         print*,' goto 888 3'
         goto 888
       end if   
     CALL CPU_TIME(timel0)
    call FLIGHTsort(NW) ! sorts flight entries for growing ITW
     CALL CPU_TIME(timel1)
     if(LPR) print*,' after FLIGHTsort,NW,nfl',NW,nfl
     
   
      
     CALL checkmonoton(NW) ! stops if waypoint times do not growo monotonically,
      
      Print*,'NFL',NFL
 
     timeFLIGHTsort=timeFLIGHTsort+(timel1-timel0)
     
   
    if(LPR) print*,' before ssorttime,NW,nfl',NW,nfl

    
                  
        
     CALL CPU_TIME(timel0)
    call ssorttime(NW) ! eliminates double time entries, fills gaps with creategreatcircle
     CALL CPU_TIME(timel1)
     NWOLD=NW
     if(LPR) then
     print*,' after ssorttime,NW',NW
        Print*
        end if
 
 
         if(NW.lt.10) then 
      Print*,' Nw.lt.10',NW
       ngoto888=ngoto888+1
     print*,' goto 888 4'
      goto 888
       end if   
    
       if(LPR) print*,' after ssorttime,NW',NW
  
  
      if(LPR) print*,' before sflchange',NW
     call SFLchange(NW)  ! eliminates jumps in FL   
!     do I=1,NW
!     print*,'flight(I)%altitude',flight(I)%altitude
!     end do
      if(LPR) print*,' after SFLchange,NW',NW

   
         
  
        if(LPR) print*,'flight(1)%flight_id.eq. -248766251,nfl',flight(1)%flight_id,nfl
    
         
     
     if(LPR) print*,'after ssorttime: FLIGHTadd(NW)%ITW,nfl',NW,FLIGHTadd(NW)%ITW,nfl
     timessorttime=timessorttime+(timel1-timel0)    
   if(LPR) print*,' timessorttime,timeFLIGHTsort',timessorttime,timeFLIGHTsort
 
    if(NW.lt.10) then 
      Print*,' Nw.lt.30',NW
       ngoto888=ngoto888+1
       print*,' goto 888 5'
      goto 888
       end if   
         
      
   call checkmonoton(NW) ! checks again for cases with FLIGHTadd(I)%ITW.le.FLIGHTadd(I-1)%ITW
   
      
      
   if(LPR) print*,'after checkmonoton: Nw',nw
    if(LPR) print*,'after checkmonoton: FLIGHTadd(NW)%ITW',NW,FLIGHTadd(NW)%ITW &
    ,'ITW-IUTC0',FLIGHTadd(NW)%ITW-IUTC0
   call check_IDT(NW) ! tests whether time steps are larger than zero,stop othrwise
    if(LPR) print*,'after check_IDT: FLIGHTadd(NW)%ITW,nfl',NW,FLIGHTadd(NW)%ITW,nfl
    

     I=nw
          
     if(LPR) print*,' before eliminateINVALID',NW
  
   
    nggt22:  if(NW.gt.1) then
  
 if(LPR) print*,'before correctend check_IDT: FLIGHTadd(NW)%ITW'  &
 ,NW,FLIGHTadd(NW)%ITW,' ',FLIGHT(nw)%timestamp,' NFL=',NFL
 

   NLONG=NW
   NWOLD=NW
   NITER=0
   nlast=10000
    dolong: DO WHILE (Nlong.gt.0.and. NW.gt. (2+NWOLD)/2) 
    
    call sgstastest(NW)
! computes      flightadd(NW)%cosaw,sinaw,GSDISTW
       flightadd(1:NW)%INVALID=0
         do I=1,NW-1
        if (flightadd(I)%GSDISTW.gt.distlimit) then
!        print*,'invalid',I,flightadd(I)%GSDISTW,distlimit
      
        flightadd(I)%INVALID=1
        flightadd(I+1)%INVALID=1
 !       print*,' flightadd(I)%GSDISTW',flightadd(I)%GSDISTW
        end if
    end do
 
        NLONG=sum(flightadd(1:NW)%INVALID)
         call eliminate_FL(NW)
    if(nlong.gt.0) then    
     CALL eliminateINVALID(NW)
     end if                                                                        
         if(NW.lt.10) then 
      Print*,' Nw.lt.10',NW
       ngoto888=ngoto888+1
   print*,' goto 888 6'
      goto 888
       end if   
    NITER=NITER+1
    if(LPR) print*,' N_INTERRUPT,nw,nwold,NITER',NLONG,nw,nwold,NITER
    if(nlast.eq.nlong) exit
    nlast=NLONG
    end do dolong
 
    
    
    if(NW.le.(2+NWOLD)/2) then
    if(LPR) print*,' (NW.le. (2+NWOLD)/2) -> stop,nw,nwold',nw,nwold
     if(LPR) print*,'START CHECK FOR LIMITS,nfl=',nfl
    
!         do I=1,NW
!      print*,'distlimit: I,lon,lat,alt,gs,clr,GSDISTW,lim', &
!      I,FLIGHT(I)%longitude,FLIGHT(I)%latitude,FLIGHT(I)%altitude  &
!     ,FLIGHT(I)%groundspeed,FLIGHT(I)%vertical_rate,flightadd(I)%GSDISTW,distlimit
!      if (flightadd(I)%GSDISTW.gt.distlimit) then
!      print*,' flightadd(I)%GSDISTW.gt.distlimit'
!      end if
!      end do
       if(LPR)  THEN
       print*,'end CHECK FOR LIMITS'
       print*,' FLLISTS',FLLISTS
       print*,' NFL',NFL
       Print*,'  NW.le. (2+NWOLD)/2),nw,nwol,nfl',nw,nwold,nfl
      endif
     if(NW.lt.10) then 
      Print*,' Nw.lt.10',NW
       ngoto888=ngoto888+1
    print*,' goto 888 7'
      goto 888
       end if   
    end if

       call sgstas(NW,IPS)
       if(LPR) print*,' after sgstas: nw,sum(flightadd(1:NW)%invalid),nfl',nw,sum(flightadd(1:NW)%invalid),nfl
 
! computes      flightadd(NW)%segmw,TASW,cosaw,sinaw,CLRW,dgsdtW,Machw
    if(NW.lt.10) then 
      Print*,' Nw.lt.10',NW
       ngoto888=ngoto888+1
       print*,' goto 888 8'
 
      goto 888
       end if   
!       do I=1,max(1,Nsmooth)
!    call smoothdt2(NW)
!    end do
    call smooth(NW)
    call smoothdvdt(NW)
 
   if(LPR) print*,' after smooth,NW,FLLISTS%flight_id,nfl',NW,FLLISTS%flight_id,nfl
   if(LPR) print*,'nw,sum(FLIGHTadd(1:NW)%invalid),nfl',nw,sum(FLIGHTadd(1:NW)%invalid),nfl
 
    call sflcruise(NW)
    
 
    
     if(NW.lt.10) then 
      Print*,' Nw.lt.10',NW
       ngoto888=ngoto888+1
   print*,' goto 888 9'
      goto 888
       end if   
     do I=2,NW-1
!
        if (flightadd(I)%GSDISTW.gt.distlimit) then
        flightadd(I-1)%INVALID=1
        flightadd(I)%INVALID=1
        flightadd(I+1)%INVALID=1
        if(LPR) print*,' before sgstas flightadd(I)%GSDISTW',flightadd(I)%GSDISTW
          end if
    end do
    if(LPR) print*,' before eliminateINVALID N_INTERRUPT,nw',sum(flightadd(1:NW)%INVALID),nw
    call eliminateINVALID(NW)
       if(LPR) print*,' before sgastas N_INTERRUPT,nw,nfl',sum(flightadd(1:NW)%INVALID),nw,nfl
       if(NW.lt.10) then 
      Print*,' Nw.lt.10',NW
       ngoto888=ngoto888+1
    print*,' goto 888 10'
      goto 888
       end if   
         call sgstas(NW,IPS)
 
!      call OUTFLC(NW)


     do I=2,NW-1
!      print*,'I,flightadd(I)%GSDISTW',I,flightadd(I)%GSDISTW
        if (flightadd(I)%GSDISTW.gt.distlimit) then
        flightadd(I-1)%INVALID=1
        flightadd(I)%INVALID=1
        flightadd(I+1)%INVALID=1
 !       print*,' flightadd(I)%GSDISTW',flightadd(I)%GSDISTW
  
        end if
    end do
    if(LPR) print*,' N_INTERRUPT,nw,nfl',sum(flightadd(1:NW)%INVALID),nw,nfl
    
  
       
    
      
   
       if(NW.lt.10) then 
      Print*,' Nw.lt.10',NW
       ngoto888=ngoto888+1
       print*,' goto 888 11'
   
      goto 888
       end if   
 
    call sdistcheck(NW) ! adds INVALID and SEGMWs
    
    
          
!      call addmass(NW)
       RT=FLLISTS%flown_distance*1820
       if(LPR) print*,' rt',rt
       lnewoem=.true.
       FLoad=meanLFd(IDAY)
       Mass(2)=massxfromLF(IS)
       if(rt.gt.20000.E3) then
       Print*,' rt unbelievable large:', RT,ips,Mass(2),FLoad
       stop
       end if
       if(Fload.le.0..or.Rt.le.10..or. IPS.le.0) then
       Mass(1)=massxfromLF(IS)
       else
       call STakeoffmass1(IPS,Rt,FLoad,Mass(1),lnewoem)
       print*,' NW', NW
!       if(NW.gt.1) then
!        call OUTFLC(NW,IPS,'test.txt')
!       end if
!       stop
       end if
       imassvalid(1)=1
    
       Mass(2)=massxfromLF(IS)
       
       Mass(3)=PMTOM(IPS)*0.97
       
       if(LPR) print*,' IPS,',IPS,' ',PICAO(IPS),' ',RT,FLLISTS%flown_distance
       if(LPR) print*,' mass',mass,'FLLISTS%flight_id',FLLISTS%flight_id
    
 
       do Imass=1,3
      flightadd(1:NW)%massw(IMASS)=0.
      FLIGHTadd(1:NW)%ffw(IMASS)=0      
     end do
      FLIGHTadd(NW)%massfrom(2)=0! clw
     FLIGHTadd(NW)%massfrom(5)=0! ctw
   
     iwr=0
    
       do I=1,NW
       if(FLIGHTadd(I)%invalid.eq.0) then
    dvdttemp(I)=drittel*(FLIGHTadd(max(1,I-1))%dgsdtW+FLIGHTadd(I)%dgsdtW+FLIGHTadd(min(NW,I+1))%dgsdtW)
      dvdttemp(I)=max(Min(dvdttemp(I),1.),-0.5)
      else
      dvdttemp(I)=0.
      end if
      end do
     FLIGHTadd(1:NW)%dgsdtW=dvdttemp(1:NW)
     Imachhigh=0
     IFLhigh=0
     machmax=0
     flmax=0.
     do I=10,NW-10
     if(FLIGHTadd(I)%Ioriginal.gt.0 .and. FLIGHT(I)%u_component_of_wind.ne.0) then
     if(FLIGHTadd(I)%machw.gt.PMMO(IPS)*1.02) then
     Imachhigh=Imachhigh+1
     machmax=max(machmax,FLIGHTadd(I)%machw)
     end if
     if(FLIGHT(I)%altitude.gt.100.*PMalthft(IPS)) then
     IFLhigh=IFLhigh+1
     flmax=max(flmax,FLIGHT(I)%altitude)    
     end if
     end if     
     end do
     if(Imachhigh.gt.0) then 
     write(20,*) FLIGHT(1)%flight_id,' MMO   ',FLLISTS%aircraft_type,' ',IPS,machmax,PMMO(IPS),Imachhigh
     end if
     if(IFLhigh.gt.0) then 
     write(20,*) FLIGHT(1)%flight_id,' FLmax ',FLLISTS%aircraft_type,' ',IPS,flmax,100.*PMalthft(IPS),IFLhigh
     end if
     
!     call eliminate_FL(NW)
     Imassclfound=0
     massxCL=0.3*PMTOM(IPS)+0.7*POEM(IPS)
     massxCT=massxCL
     
     
!!!!!!!!!!!!!!!!!!     

     loopmass1to3:  do Imass=1,3
     
    flightadd(1:NW)%massw(imass)=mass(imass)
    lvalidps: if(IPS.le.NPS-2) then
    ffold=0.
    IWR49=0
    flmax=0.
    itlastmass=-999
     loopmssnw: do I=1,NW
     
     idt=FLIGHTadd(I)%itw-FLIGHTadd(max(1,I-1))%itw
     loopmassnwvalid: if(FLIGHTadd(I)%invalid.eq.0) then
     if(itlastmass.eq.-999) itlastmass=FLIGHTadd(i)%itw
    mach=FLIGHTadd(I)%machw
    FL=FLIGHT(I)%altitude/100.
    dhdt=FLIGHT(I)%vertical_rate
!    dvdt=FLIGHTadd(I)%dgsdtW
    Temp=FLIGHT(I)%temperature
    DTISA=Temp-tisa(FL*30.48)
!    dhdt=0
     dvdt=0
     flmax=max(flmax,flight(I)%altitude)
    machgt017: if(mach.gt.0.17) then
!    dvdt=min(dvdt,0.5)
!      dvdt=max(dvdt,-0.5)
    call psbeta3(IPS,mass(IMass),mach,FL,dhdt,dvdt,DTISA,ff,eta  & 
       ,etaLD,CL,CD,massCL,massCT)      
      massct=massct*0.7
           massCL=massCL/(1.-meanerrorac(2,IPS,Jairline)) ! Imethod=2 
           massCT=massCT/(1.-meanerrorac(5,IPS,Jairline)) ! imethod=5
       
     
      
       
!!!! massCL is used for method 2 
!!!! massCT is used for method 5 
      
      ifm1: if(IMASS.ge.3) then
         ifm2: if(I.lt.NW/4) then
          ifm3: if(Imassclfound.le.3) then
           ifm4: if(flightadd(I)%Ioriginal.eq.1  .and. flightadd(I)%Invalid.eq.0  .and. dhdt.gt.10.) then            
            ifm5: if(POEM(IPS).lt.massCL.and. massCL.lt.0.97*PMTOM(IPS)) then
        imassvalid(2)=1
        Imassclfound=Imassclfound+1
        massxCL=MIN(massCL,massxCL) ! Imethod=2
        massresult6(2)=massxCL
     
            end if ifm5
           end if ifm4
          end if ifm3
         end if ifm2  
      
         
   end if ifm1

  
  ltfm1: if(Imass.eq.3) then
   if(flight(I)%altitude.gt.flmax-100.) then
    if(flight(I)%altitude.gt.0.25*100.*PMalthft(IPS).and.dhdt.ge.0.) then
     if(flightadd(I)%Ioriginal.eq.1 .and. flightadd(I)%Invalid.eq.0) then
       if(1.01*POEM(IPS).lt.massCT.and. massCT.lt.0.97*PMTOM(IPS)) then
        imassvalid(5)=1
         massxCT=min(massCT,massxCT)
          massresult6(5)=massxCT
      end if
     end if
    end if
   end if
  end if ltfm1
    
         
  
   
       if(ff.lt.mfmaxTO_kg_s(IPS))then
       FLIGHTadd(I)%ffw(IMASS)=ff
       else
       FLIGHTadd(I)%ffw(IMASS)=ffold
       end if
       ffold=FLIGHTadd(I)%ffw(IMASS)
      
       flightadd(I)%massw(IMASS)=mass(IMASS)
       idt=FLIGHTadd(I)%itw-itlastmass
       itlastmass=FLIGHTadd(I)%itw
            mass(IMass)=mass(IMass)-FLIGHTadd(I)%ffw(IMASS)*idt!flightadd(I)%SEGMW
           if(imass.ge.2  &
            .and. mass(IMass).lt.POEM(IPS)) then
             if(IWR49.eq.0) then
           print*,'I=',I,'imass,mass.lt.OEM,mass,massLF,imass,mass=',imass,mass(IMass)  &
            ,'OEM=',POEM(IPS),'TOWLF=',massxfromLF(IS)  &
            ,' ',FLLISTS%aircraft_type,' ',FLLISTS%flight_id  &
            ,'dist/km=',FLLISTS%flown_distance*1.830
            
            write(49,*) picao(IPS),' ',IPS,jairline &
            ,flightadd(I)%ITW-flightadd(1)%ITW &
            ,flightadd(NW)%ITW-flightadd(1)%ITW &
            ,FLLISTS%flight_duration*60 &
            , FLLISTS%flown_distance &
           ,massxfromLF(IS) &
           , IMass &
           ,mass(IMass) &
           ,POEM(IPS) &
           ,PMTOM(IPS),PMZFM(IPS) &
           ,FLLISTS%flight_id
                     
            
            IWR49=1
             end if
           end if
            mass(IMass)=max(mass(IMass),0.9*POEM(IPS))
     end if machgt017
     end if loopmassnwvalid
     end do loopmssnw
     end if lvalidps
   end do loopmass1to3
   if(imassvalid(2).gt.0) then
    massxCL=max(massxCL,Flightadd(1)%massw(1))      
    FLIGHTadd(nw)%massfrom(2)=massxCL ! CLw  
     massresult6(2)=massxCL
    end if
    
   if(imassvalid(5).gt.0) then
    massxCT=max(massxCT,Flightadd(1)%massw(1))
    FLIGHTadd(nw)%massfrom(5)=massxCT! CTw  
    massresult6(5)=massxCT
    end if
     
   
   
   
    if(LPR) print*,' POEM(IPS)',POEM(IPS),' ',FLLISTS%aircraft_type,' IPS=',IPS,' NFL=',NFL
 
   call M3_SMassfromDO(NW,IPS)
  
   
   call M4_climbdata(NW,IPS)
 
       
   if(LWRITE17) then
   write(17,*)FLLISTS,NW
!     call disttest2(NW)
    Ifl1=IFLinitialcruise
    Ifl2=IFLMAX
    I=1
    write(17,*) FLIGHT(I),flightadd(I),flightadd(I)%ITW-IUTC0  &
    ,flightadd(Ifl1)%ITW-IUTC0,FLIGHT(Ifl1)%altitude  &
    ,flightadd(Ifl2)%ITW-IUTC0,FLIGHT(Ifl2)%altitude

    do I=2,NW-1
    if(flightadd(I)%invalid.eq.0) then
    write(17,*) FLIGHT(I),flightadd(I),flightadd(I)%ITW-IUTC0  

    end if
    end do
    I=NW
       write(17,*) FLIGHT(I),flightadd(I),flightadd(I)%ITW-IUTC0  
    
   end if

  
   if(LWRITE17) then
   write(17,*)
   close(17)
   print*,' stop after write 17'
   stop
   end if
   if(LPR) print*,' tesflR4 done'
 
   if(LPR) print*,' N_INTERRUPT,nw,nfl',sum(flightadd(1:NW)%INVALID),nw,nfl 
    
    
   if(LPR) print*,' nggt22: written,NW,nfl=',NW,nfl,'flight_id=',FLIGHT(1)%flight_id

  end if  nggt22
  
 

    end if nwgt1
 
 888 continue
 
 
   print*,'############################################'
   if(LPR) print*
    Print*,'iday',iday,' NFOUNDS',NFOUNDS  &
    ,' NFL=',NFL,'  ngoto888',ngoto888,' NW=',NW

    
     NW=Max(NW,1)
    flightairdist=FLLISTS%flown_distance*1852  
    massresult6(1)=massxfromLF(IS)
    imassvalid(1)=1
    if(NW.gt.100) then    
   call M1_MassfromLF(NW,IPS,IDAY)
!  print*,' massresult6(1),FLLISTS%tow',massresult6(1),FLLISTS%tow
       end if
   K=ibestm(Ips,Jairline)
   K=Max(1,min(5,K))
    lselect: if(imassvalid(K).gt.0) then
   massresult6(6)=massresult6(K)
   else   
   if(imassvalid(isequence(1)).gt.0) then
   massresult6(6)=massresult6(isequence(1))
   else   
   if(imassvalid(isequence(2)).gt.0) then
   massresult6(6)=massresult6(isequence(2))
   else  
   if(imassvalid(isequence(3)).gt.0) then
    massresult6(6)=massresult6(isequence(3))
   else   
   if(imassvalid(isequence(4)).gt.0) then
    massresult6(6)=massresult6(isequence(4))
   else  
   if(imassvalid(isequence(5)).gt.0) then
    massresult6(6)=massresult6(isequence(5))
    else
    massresult6(6)=0.3*PMTOM(IPS)+0.7*POEM(IPS)
    end if
   end if
    end if
   end if
    end if
   end if lselect
   
  
      if(LPR)PRint*,'IPS,jairline,ibestm(Ips,Jairline),massresult6(6),FLLISTS%tow', &
   IPS,jairline,ibestm(Ips,Jairline),massresult6(6),FLLISTS%tow
 
    print*,' (massresult6(1:6)-FLLISTS%tow)/FLLISTS%tow' &
    , (massresult6(1:6)-FLLISTS%tow)/FLLISTS%tow
   Imassvalid(6)=1
! computes massresult6(1)  and flightadd(NW)%massfrom(1), 
! based on load factor and flown air distance
! also computes flightairdist
 

    rms=rms50/max(1,nwr50)
    rms=sqrt(rms)
    print*,' nwr50,mean(rms)',nwr50,rms    
     write(50, &
  '(F12.5,I10,1x,a4,1x,I10,2i3,1x,7I2,6f10.0,f10.1,I10,6F10.0,6F12.5)') & 
     rms,FLLISTS%flight_id,PICAO(IPS),is,IPS,jairline,imassvalid (1:6)&
    ,ibestm(Ips,Jairline) &
    ,massresult6(1:6),clrout,nclrout,massxfromLF(IS)  &
     ,FLLISTS%tow,flmax,100.*PMalthft(IPS) &
     ,flightairdist,FLLISTS%flown_distance*1852.  &
     ,(massresult6(1:6)-FLLISTS%tow)/FLLISTS%tow
     rmsl=(massresult6(6)-FLLISTS%tow)/FLLISTS%tow
     print*,'err6',rmsl
     NWR50=NWR50+1
     rms50=rms50+rmsl**2
     write(51,*)NWR50,Iday,Ips,Jairline,FLLISTS%flight_id & 
     ,FLLISTS%tow,rmsl,flightairdist,FLLISTS%flown_distance*1852.,' ',PICAO(IPS)
     
!     if(flightairdist.gt.2.*FLLISTS%flown_distance*1852.) stop
!!! ??? flight_id added 18 10 2024
     Lflightready(IS)=.true.
     end do dowhileilast
  999 continue
   close(5)

  
   
   close(50) ! file='outmassnew_'//cdate//'.txt')

    rms=rms50/max(1,nwr50)
    rms=sqrt(rms)
    print*,' nwr50,mean(rms)',nwr50,rms
!   if(Nwr50.gt.1000) stop
   open(77,file='flighreadStest',form='unformatted')
   
  write(77) Lflightready
  close(77)
    J=0
   do I=1,nfsmax
   if(.not.Lflightready(I)) then
   J=j+1
   end if
   end do
   print*,'number of  missing flights, J=', J
   
      Print*,' all read done,NFL,NWTOT=',NFL,NWTOT  &
  ,' FLIGHT(1)%flight_id',FLIGHT(1)%flight_id,' Nwr50=',Nwr50
    rms=rms50/max(1,nwr50)
    rms=sqrt(rms)
    print*,' nwr50,mean(rms)',nwr50,rms
  
   end do LOOPday
   
   
     close(49)
     close(20)
   print*,' after LOOPday,final stop,NFL=',NFL 
   close(17)
   J=0
   do I=1,nfsmax
   if(.not.Lflightready(I)) then
   J=j+1
   print*,' still missing',I
   end if
   end do
   print*,'number of  missing flights, J=', J
   
   end subroutine sloopflights
    
    
   subroutine M1_MassfromLF(NW,IPS,IDAY) ! -> massresult6(1) and flightadd(NW)%massfrom(1)
           USE mo_directories, ONLY: FLIGHT, flightadd ,massxfromLF,meanLFd &
           ,imassvalid,lpr,massresult6,meanerrorac,Jairline,NPS
  IMPLICIT NONE
                         
  INTEGER,INTENT(IN):: NW,IPS,IDAY
  REAL massresult
    INTEGER,Parameter:: IMETHOD=1
        if(LPR) print*,' nw,ips,iday', nw,ips,iday
       
         call M1_LOADF_wind(NW,IPS,IDAY,massresult)
          flightadd(NW)%massfrom(1)=massresult ! lfw
    
         massresult=massresult/(1.-meanerrorac(IMETHOD,IPS,Jairline))
        imassvalid(IMETHOD)=1
        massresult6(IMETHOD)=massresult   
        
         flightadd(NW)%massfrom(1)=massresult         
       
                                                                
            
              
     end subroutine M1_MassfromLF


      
      
            
      subroutine OUTFLC(NW,IPS,cfile)
           USE mo_directories,ONLY: FLIGHT,FLLISTS,FLIGHTadd,IUTC0,NPLOT &
           ,lpr,nps,IFLinitialcruise,IFLMAX
           IMPLICIT NONE
           INTEGER,INTENT(IN)::NW,IPS
           character*(*),Intent(IN)::cfile
       INCLUDE "psbeta3.inc"
            INTEGER I,Ifl1,Ifl2
!           REAL z0 ! test of integral vertical velocity compared to altitude -> vertical rate comes in m/s
         
 !        do I=1,NW
 !        if(FLIGHT(I)%timestamp(1:4).ne.'2022') then
 !        print*,'I,NW,FLIGHT(I)%timestamp',I,NW,FLIGHT(I)%timestamp  &
 !        ,FLIGHTadd(I)%Ioriginal,FLIGHTadd(I)%invalid
 !        stop
 !        end if
 !        end do
         
     
           open(10,file=cfile)
        
        write(10,*) 'No ','flight_id '  &
      ,'timestamp '  &
      ,'lat/deg ','lon/deg '  &
      ,'alt/ft ','GS_given/(m/s) '  &
      ,'track ','CLR_given/(m/s) ' ,'ICAO24 ' & 
      ,'u_wind/(m/s) ','v_wind/(m/s) '  &
      ,'temperature/K ','humidity/(g/g) '  &
        ,'time_1Jan2000/s  ','original ','Invalid '  &
     ,'segm_length/m ','cosa ','sina ','TAS/(m/s) ','GS_computed/(m/s) '  &
     ,'dv/dt/(m/s2) ','Mach ','CLR_computed/(m/s) '  &
      ,'massCLO/kg ','massref/kg ','Massmax/kg '  &
     ,'ffmin/(kg/s) ','ffref/(kg/s) ','ffmax/(kg/s) '   &
     ,'massxLF/kg ' ,'massxCL/kg ','massxCT/kg ',' massxCLo/kg ',' massxxxx/kg '  &
     ,'Err_massL ','Err_massT ','Err_massCLo ','Err_massxxx '  &
     ,'time/s '  &
     ,'time_FL1/s ','FL1/ft ',' time_FL2/s ','FL2/ft ','ATYP ','Distce/nml '
    I=1
       Ifl1=IFLinitialcruise
    Ifl2=IFLMAX
    if(ifl1.gt.0 .and. ifl2.gt.0) then
    write(10,*) FLIGHT(I),flightadd(I),flightadd(I)%ITW-IUTC0  &
    ,flightadd(Ifl1)%ITW-IUTC0,FLIGHT(Ifl1)%altitude  &
    ,flightadd(Ifl2)%ITW-IUTC0,FLIGHT(Ifl2)%altitude  &
    , ' ',PICAO(IPS),' ',FLLISTS%flown_distance 
    else
       write(10,*) FLIGHT(I),flightadd(I),flightadd(I)%ITW-IUTC0 
    end if

    do I=2,NW-1
    if(flightadd(I)%invalid.eq.0) then
    write(10,*) FLIGHT(I),flightadd(I),flightadd(I)%ITW-IUTC0  
    end if
    end do
    I=NW
       write(10,*) FLIGHT(I),flightadd(I),flightadd(I)%ITW-IUTC0  
     write(10,*)
       if(LPR) print*,' plot for FLIGHT(1)%flight_id done ',FLIGHT(1)%flight_id,' NW=',NW ,' cfile=',cfile
  
       close(10)
       print*,'outflc done,see: ',cfile,' NW=',NW
       END SUBROUTINE  OUTFLC 
       
                  
      
    
      
    
   subroutine FLIGHTsort(NW)
! sorts waypoints by times ITW
   USE mo_types,only: TFLIGHT,TFLIGHTadd
   USE mo_directories,ONLY: FLIGHT,FLIGHTadd,FLLISTS,iutc0,lpr
      IMPLICIT NONE
      INTEGER,INTENT(INout)::NW
     INteger,DIMENSION(NW)::arr,brr
     INTEGER I,J
     TYPE(TFLIGHT),DIMENSION(NW):: FLIGHTc
     TYPE(TFLIGHTadd),DIMENSION(NW):: FLIGHTaddc
      FLIGHTaddc(1:NW)=FLIGHTadd(1:NW)
      FLIGHTc(1:NW)=FLIGHT(1:NW)
        
    I=0
    do J=1,NW
      I=I+1
    arr(I)=FLIGHTadd(J)%ITW   
    Brr(I)=J
    end do
     if(LPR)then
      if(I.le.10) then
       print*,'FLIGHTsort: after check for early/late: NW<10,nwold, nwnew',NW,I
       print*,'FLLISTS%flight_id',FLLISTS%flight_id
      end if
     end if
       NW=I
    if(NW.gt.2) then
    call sort2(NW,arr,brr)
    
    do I=1,NW
    J=BRR(I)
   FLIGHT(I)=FLIGHTc(J)
!   if(FLIGHT(I)%latitude.lt.-90) then
!   print*,' FLIGHT(I)%latitude.lt.0.',I,FLIGHT(I)%latitude
!   stop
!   end if
   FLIGHTadd(I)=FLIGHTaddc(J)
    end do
    end if
 
    END SUBROUTINE  FLIGHTsort
 
      SUBROUTINE SORT2(N,ARR,BRR)
      IMPLICIT NONE
      INTEGER N,M,NSTACK
      INTEGER ARR(N),A,BRR(N),B,TEMP
      PARAMETER (M=7,NSTACK=50)
      INTEGER I,IR,J,JSTACK,K,L,ISTACK(NSTACK)
!      REAL A,B,TEMP
      JSTACK=0
      L=1
      IR=N
1     IF(IR-L.LT.M) THEN
        DO J=L+1,IR
          A=ARR(J)
          B=BRR(J)
          DO I=J-1,1,-1
            IF(ARR(I).LE.A)GOTO 2
            ARR(I+1)=ARR(I)
            BRR(I+1)=BRR(I)
           end do
          I=0
2         ARR(I+1)=A
          BRR(I+1)=B
       end do
        IF(JSTACK.EQ.0)RETURN
        IR=ISTACK(JSTACK)
        L=ISTACK(JSTACK-1)
        JSTACK=JSTACK-2
      ELSE
        K=(L+IR)/2
        TEMP=ARR(K)
        ARR(K)=ARR(L+1)
        ARR(L+1)=TEMP
        TEMP=BRR(K)
        BRR(K)=BRR(L+1)
        BRR(L+1)=TEMP
        IF(ARR(L+1).GT.ARR(IR))THEN
          TEMP=ARR(L+1)
          ARR(L+1)=ARR(IR)
          ARR(IR)=TEMP
          TEMP=BRR(L+1)
          BRR(L+1)=BRR(IR)
          BRR(IR)=TEMP
        end if
        IF(ARR(L).GT.ARR(IR))THEN
          TEMP=ARR(L)
          ARR(L)=ARR(IR)
          ARR(IR)=TEMP
          TEMP=BRR(L)
          BRR(L)=BRR(IR)
          BRR(IR)=TEMP
        end if
        IF(ARR(L+1).GT.ARR(L))THEN
          TEMP=ARR(L+1)
          ARR(L+1)=ARR(L)
          ARR(L)=TEMP
          TEMP=BRR(L+1)
          BRR(L+1)=BRR(L)
          BRR(L)=TEMP
        end if
        I=L+1
        J=IR
        A=ARR(L)
        B=BRR(L)
3       CONTINUE
          I=I+1
        IF(ARR(I).LT.A)GOTO 3
4       CONTINUE
          J=J-1
        IF(ARR(J).GT.A)GOTO 4
        IF(J.LT.I)GOTO 5
        TEMP=ARR(I)
        ARR(I)=ARR(J)
        ARR(J)=TEMP
        TEMP=BRR(I)
        BRR(I)=BRR(J)
        BRR(J)=TEMP
        GOTO 3
5       ARR(L)=ARR(J)
        ARR(J)=A
        BRR(L)=BRR(J)
        BRR(J)=B
        JSTACK=JSTACK+2
        IF(JSTACK.GT.NSTACK) then
        Print*,'NSTACK TOO SMALL IN SORT2'
        stop
        end if
        IF(IR-I+1.GE.J-L)THEN
          ISTACK(JSTACK)=IR
          ISTACK(JSTACK-1)=I
          IR=J-1
        ELSE
          ISTACK(JSTACK)=J-1
          ISTACK(JSTACK-1)=L
          L=I
        end if
      end if
      GOTO 1
      END SUBROUTINE  SORT2
!  (C) Copr. 1986-92 Numerical Recipes Software 2.02



   SUBROUTINE sdist(NW)
    USE mo_types,only: TFLIGHT
    use mo_directories,only: flightadd,flight,lpr
    use mo_cocip_parameters,only: UNITY,PIR180
    IMPLICIT NONE
    INTEGER,INTENT(IN):: NW
    INTEGER I,IZERO,idtmin,idtmax
    real distmin,distmax,UNITX,dist,distx,disty
    INTEGER itold,IDT,nidt1,nidt2,itlast!,ddold
    INTEGER IT(NW)
    INTEGER,PARAMETER:: is=1
    INTEGER NGSDEV,idtl,idtMean
    REAL GS
    real *8 sumdt,GSMEAN1,GSMEAN2,GSDEV
      if(LPR) Print*,' sdist begin,NW=',NW 
    idtmin=100000
    idtmax=0
    GSMEAN1=0.
    GSMEAN2=0.
    GSDEV=0.
    distmin=1.e10
    distmax=0.
    NGSDEV=0
    IZERO=0
!    ddold=-1
    itold=-1
    nidt1=0
    nidt2=0
    Idtl=1
     do I=1,NW
    
!    read(FLIGHT(I)%timestamp,'(I4,1x,I2,1x,I2)') yyyy,mm,dd

!    read(FLIGHT(I)%timestamp2,'(4(I2,1x))')hh,minmin,sec
!    If(dd.ne.ddold) call DAYJ(yyyy,mm,dd,IDAYY,is)
!    ddold=dd
!    IT(I)=(IDAYY-1) *24*3600+hh*3600+minmin*60+sec
    IT(I)=flightadd(I)%ITW
    if(I.gt.1) then
    idtl=IT(I)-IT(I-1)
 
    idtmin=MIN(Idtl,idtmin)
    idtmax=Max(Idtl,idtmax)
 
    end if
  
  
     end do
     idtMean=0
     if(NW.gt.1) idtMean=(IT(NW)-IT(1))/(Nw-1)
     print*,'NW,idtmean,idtmin,idtmax',NW,idtmean,idtmin,idtmax

      return
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!      
      
      
     idtmin=1000000
     idtmax=0
         
    do I=1,NW-1
    idt=IT(I+1)-IT(I)
    if(IDT.le.0) then
    IZERO=izero+1
    
    UNITX=0.5*(FLIGHT(I)%latitude+FLIGHT(I+1)%latitude)
    UNITX=UNITY*cos(UNITX*PIR180)


    distx=(FLIGHT(I+1)%longitude-FLIGHT(I)%longitude)*UNITX
    disty=(FLIGHT(I+1)%latitude-FLIGHT(I)%latitude)*UNITY
    dist=distx**2+disty**2
    dist=sqrt(dist)
        print*,'####idt.le.0:  IZERO,nw,idt,dist',IZERO,nw,idt,dist
    else
    idtmin=min(idtmin,IDT)
    idtmax=max(IDTmax,IDT)
    if(IDT.eq.1) then
    nidt1=nidt1+1
    else
        nidt2=nidt2+1
        print*,' I,NW,NIDT2',I,NW,nidt2
    end if
    end if
    end do
    print*,' IZERO,idtmin,idtmax',IZERO,idtmin,idtmax
    if(LPR) print*,'nidt1',nidt1
    if(LPR) print*,'nidt2',nidt2
    itlast=IT(2)
    do I=1,NW-1
 
    UNITX=0.5*(FLIGHT(I)%latitude+FLIGHT(I+1)%latitude)
    UNITX=UNITY*cos(UNITX*PIR180)

    distx=(FLIGHT(I+1)%longitude-FLIGHT(I)%longitude)*UNITX
    disty=(FLIGHT(I+1)%latitude-FLIGHT(I)%latitude)*UNITY
    dist=distx**2+disty**2
    idt=IT(I+1)- itlast
     dist=sqrt(dist)
    if(dist.gt.1..and.IDT.gt.0) THEN        
    if(LPR) print*,'I,lat,lon',I,FLIGHT(I)%longitude,FLIGHT(I)%latitude  &
    ,'alt,GS,CLR,track'  &
    ,FLIGHT(I)%altitude,FLIGHT(I)%groundspeed  &
    ,FLIGHT(I)%vertical_rate  &
     ,FLIGHT(I)%track
    distmin=MIN(distmin,dist)
    distmax=max(distmax,dist)
    itlast=IT(I)
    else
    IZERO=IZERO+1
!    print*,'IZERO,I,NW,distmin,distx,disty,UNITX,UNITY'  &
!    ,IZERO,I,NW,distmin,distx,disty,UNITX,UNITY  &
 !   ,' x1,x2,y1,y2'  &
!    ,FLIGHT(I)%longitude,FLIGHT(I+1)%longitude  &
!    ,FLIGHT(I)%latitude,FLIGHT(I+1)%latitude
     end if
     IDTL=IT(I+1)-itlast
     if(IDTL.gt.0) then
     GS=dist/max(IDTL,1) ! in m/s
!     GS=GS*(3600./1852.)! in miles/hour'
     if(LPR) print*,'I,NW,gs,FLIGHT(I+1)%groundspeed'  &
     ,I,NW,gs,FLIGHT(I+1)%groundspeed
     if(LPR) print*,' idtl,GS,gdev',idtl,GS,GS-FLIGHT(I+1)%groundspeed
         
     
     GSMEAN1=GSMEAN1+GS*IDTL
     GSMEAN2=GSMEAN2+FLIGHT(I+1)%groundspeed*IDTL
     sumdt=sumdt+IDTL
     GSDEV=GSDEV+IDTL*(GS-FLIGHT(I+1)%groundspeed)**2
     NGSDEV=NGSDEV+IDTL
     end if
    end do
    print*,' sdist: NW,distmin,distmax',NW,distmin,distmax
    print*,'NW,IZERO',NW,IZERO
    Print*,' NGSDEV',NGSDEV
    GSDEV=GSDEV/NGSDEV
    GSDEV=sqrt(gSDEV)
    print*,' sumdt,GSMEAN1/sumdt,GSMEAN21/sumdt,GSDEV'  &
    ,sumdt,GSMEAN1/sumdt,GSMEAN2/sumdt,GSDEV
    print*,' stop'
    stop
    END SUBROUTINE  sdist
    
    
    subroutine sFLIGHTadd(NW)
! reads ITW and sets IORIGINAL
      USE mo_directories,ONLY: FLIGHT,FLIGHTadd
      IMPLICIT NONE
      INTEGER,INTENT(IN)::NW
      INTEGER I,yyyy,mm,dd,hh,minmin,sec,ddold,IDAYY
      INTEGER,parameter:: IS=15
      ddold=-1
      do I=1,NW
      FLIGHTadd(I)%massw(1)=0.
      FLIGHTadd(I)%massw(2)=0.
      FLIGHTadd(I)%massw(3)=0.
      FLIGHTadd(I)%ffw(1) =0.
      FLIGHTadd(I)%ffw(2) =0.
      FLIGHTadd(I)%ffw(3) =0.
       FLIGHTadd(I)%massfrom =0.
     
  !      print*,' FLIGHT(I)%timestamp',FLIGHT(I)%timestamp
!      print*,'fligth(I)',FLIGHT(I)
    read(FLIGHT(I)%timestamp,'(I4,1x,I2,1x,I2)') yyyy,mm,dd
    read(FLIGHT(I)%timestamp2,'(4(I2,1x))')hh,minmin,sec
    If(dd.ne.ddold) call DAYJ(yyyy,mm,dd,IDAYY,is)
    FLIGHTadd(I)%ITw=(IDAYY-1) *24*3600+hh*3600+minmin*60+sec
    FLIGHTadd(I)%IORIGINAL=1
   end do
!    write(12,*)
    END SUBROUTINE  sFLIGHTadd
    

        subroutine ssorttime(NW)
! purpose: ssorttime orders the flight data such that the time is monotonically increasing
      USE mo_directories,ONLY: FLIGHT,FLLISTS,FLIGHTadd,NWX,lpr
      IMPLICIT NONE
      INTEGER,INTENT(INout)::NW
      INTEGER I,NWnew,J,K,J1,J2
      INTEGER INEW(NWX) ! index field for 2 purposes
      REAL oldheight1,oldheight2
       
! note: the waypoints got already sorted in FLIGHTsort:
!           FLIGHTadd(I)%ITW.eq.FLIGHTadd(I-1)%ITW >= 0
!          do I=1,NW
!          print*,'I,FLIGHTadd(I)%ITW',I,FLIGHTadd(I)%ITW
!          end do
!          stop
          
          J=1
          INEW(J)=1
          do I=2,NW
          if1: if(FLIGHTadd(I)%ITW.eq.FLIGHTadd(I-1)%ITW)then
          else if1
          if2: if((flight(I)%longitude.eq.flight(I-1)%longitude)  &
          .and.(flight(I)%latitude.eq.flight(I-1)%latitude)) then
          if(LPR)   print*,' lon lat eqaul'
          else if2
          J=J+1
          INEW(J)=I ! here new index
          end if  if2
          end if if1
          end do
         
          
          NW=J  ! reduced number of waypoints
          if(LPR) print*,' NW = reduced number of waypojts',NW
           do I=1,NW
          K=INEW(I)
          if(I.ne.K) then
           FLIGHTadd(I)%ITW=FLIGHTadd(K)%ITW
          FLIGHTadd(I)%Ioriginal=FLIGHTadd(K)%Ioriginal
          FLIGHT(I)=FLIGHT(K)
          end if
          end do  
          NWnew=j          
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! double values are eliminated
! now we assume a fixed timestep of IDT= 1 s     

   
     
     !!!! here add airports at 1 and NW+1 
     
     
     
     
        
           if(NWnew.ge.NWX) then
          print*,'ssorttime: NWnew.ge.NWX',NWnew,nwx
          Print*,'FLIGHTadd(1)%ITW',FLIGHTadd(1)%ITW
          Print*,' NW,FLIGHTadd(NW)%ITW',NW,FLIGHTadd(NW)%ITW
          stop
          end if
  nwgt10:  if(NWnew.gt.10) then
          INEW(1:NWNEW)=0 ! here INEW is = 1  if the waypoint is filled 
! copy existing data to final positions for constant time step idt=1
          do K=NW,1,-1
          I=FLIGHTadd(K)%ITW-FLIGHTadd(1)%ITW+1
          I=max(I,1)
          I=min(I,NWx)
          FLIGHTadd(I)%ITW=FLIGHTadd(K)%ITW
          FLIGHTadd(I)%Ioriginal=FLIGHTadd(K)%Ioriginal
          FLIGHT(I)=FLIGHT(K)
          INEW(I)=1          
          end do
   
   oldheight1= flight(1)%altitude
   flight(1)%altitude=flight(2)%altitude
    oldheight2= flight(NWnew)%altitude
    flight(NWnew)%altitude=flight(NWnew-1)%altitude
     
!!! copy ISTART(J)+1 to NLAST ->   NWNEW- ...NWNEW
         I=1
         do while (I.lt.NWnew)! I=1,NWnew-1 
!         J1=I
         J2=I
         if(INEW(I).eq.1) then
! already filled
         else
     
         do WHILE (INEW(J2+1).eq.0 .and. J2+1.lt.NWnew) 
         J2=J2+1
         end do
!         print*,'IW1,IW2',I-1,J2+1,' NWnew',NWnew
         call creategreatcircle(I-1,J2+1)
         end if
         I=J2+1
         end do


  
   flight(1)%altitude=oldheight1
   flight(NWnew)%altitude=oldheight2
    
    
     NW=NWNEW 
     
   end if nwgt10

    END SUBROUTINE  ssorttime
    

    


      SUBROUTINE snavigation(NW,airportx,airporty,xw,yw,salpha,calpha)
! https://en.wikipedia.org/wiki/Great-circle_navigation
!  
!  alpha= Final COURSE
      use mo_cocip_parameters,only: PIR180
      IMPLICIT NONE
      INTEGER,INTENT(IN)::NW
      REAL,INTENT(IN)::airportx,airporty,xw(NW),yw(NW)
      REAL,INTENT(OUT)::salpha(NW),calpha(NW)
      REAL phi1,phi2(NW),x(NW),y(NW),L12(NW)
      phi1=airporty*pir180
      phi2=yw*PIR180
      L12=(xw-airportx)*PIR180
      x=cos(phi1)*sin(L12)
      y=sin(phi2)*cos(phi1)*cos(L12)-cos(phi2)*sin(phi1)
      salpha= sin(atan2(y,x))
      calpha= cos(atan2(y,x))
!TEST     salpha=1.
!TEST    calpha=0.
      END SUBROUTINE  snavigation      
	
       SUBROUTINE intermediate_point(frac,x1,y1,x2,y2,x3,y3)
       use mo_cocip_parameters,only:PIR180,UNITy
!    """ This function calculates the intermediate point along the course laid out by p1 to p2.  frac is the frac
!    of the distance between p1 and p2,where 0 is p1,0.5 is equivalent to midpoint(*),and 1 is p2.
!    :param p1: tuple point of (lon,lat)
!    :param p2: tuple point of (lon,lat)
!    :param frac: the frac of the distance along the path.
!    :return: point (lon,lat)
!    """
       IMPLICIT NONE
       real,intent(IN):: x1,y1,x2,y2,frac
       real,intent(OUT)::  x3,y3
       real distance_between_points
       real lon1,lat1,lon2,lat2,lon3,lat3,delta,a,b,x,y,z,coslat1,coslat2,UNITX
       if(x1.eq.x2.and.y1.eq.y2) then
       x3=x1
       y3=y1
       else
        lat1=y1*PIR180
        coslat1=cos(lat1)
       UNITX=UNITY*coslat1
       delta=((x2-x1)*UNITX)**2 +((y2-y1)*UNITy)**2
       if(delta.lt.1.e8) then
       x3=x1+(x2-x1)*frac
       y3=y1+(y2-y1)*frac
       else
       lon1=x1*PIR180
  
!     _point_to_radians(_error_check_point(p1))
       lon2=x2*PIR180
       lat2=y2*PIR180
       delta = distance_between_points(x1,y1,x2,y2)!/RADIUS
       
       a = sin((1 - frac) * delta) / sin(delta)
       b = sin(frac * delta) / sin(delta)
       coslat2=cos(lat2)
       x = a * coslat1 * cos(lon1) + b * coslat2 * cos(lon2)
       y = a * coslat1 * sin(lon1) + b * coslat2 * sin(lon2)
       z = a * sin(lat1) + b * sin(lat2)
       lat3 = atan2(z,sqrt(x * x + y * y))
       lon3 = atan2(y,x)
       x3=lon3/PIR180
       y3=lat3/PIR180
       end if
       end if
!    return _point_to_degrees((lon3,lat3))
      END SUBROUTINE  intermediate_point

      real function distance_between_points(x1,y1,x2,y2)
      use mo_cocip_parameters,only: PIR180
      IMPLICIT NONE
      

      real,intent(IN):: x1,y1,x2,y2
      real lon1,lat1,lon2,lat2,d_lat,d_lon,a,c,dist,coslat1
      
      
!      https://pypi.org/project/great-circle-calculator/#files
!      
      
!    """ This function computes the distance between two points in the unit given in the unit parameter.  It will
!    calculate the distance using the haversine unless the user specifies haversine to be False.  Then law of cosines
!    will be used
!    :param p1: tuple point of (lon,lat)
!    :param p2: tuple point of (lon,lat)
!    :param unit: unit of measurement. List can be found in constants.eligible_units
!    :param haversine: True (default) uses haversine distance,False uses law of cosines
!    :return: Distance between p1 and p2 in the units specified.
!    """
       lon1=x1*PIR180
       lat1=y1*PIR180
!     _point_to_radians(_error_check_point(p1))
       lon2=x2*PIR180
       lat2=y2*PIR180
 !      if (Lhaversine) THEN
       d_lat=lat2 - lat1
       d_lon = lon2 - lon1
       coslat1=cos(lat1)
       dist=d_lat**2+(d_lon*coslat1)**2 ! in radians
!!       PRINT*,'dist.gt.0.001?',dist
       if(dist.gt. 1.E-03) then     
!!       PRINT*,'dist.gt.0.001?  JA!'       
        a = sin(d_lat *0.5)**2 + coslat1*cos(lat2)*sin(d_lon *0.5)**2
        c = 2. * atan2(sqrt(a),sqrt((1. - a)))
        dist =  c
        else
        dist=sqrt(dist)
        end if
!      else
!!    # Spherical Law Of Cosines
!      dist = acos(sin(lat1)*sin(lat2)+cos(lat1)*cos(lat2)  &
!      *cos(lon2 - lon1))
!      end if
      distance_between_points=dist!*RADIUS
      end function  distance_between_points
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      subroutine creategreatcircle(IW1,IW2)
! IW1 = index of last index before the gap
! IW2 = inde of first point after the gap
! inserts  waypoints between IW1 and IW2
      USE mo_directories,ONLY: FLIGHT,FLLISTS,FLIGHTadd,IUTC0,flmin
      USE mo_cocip_parameters,only: RADIUS
      IMPLICIT NONE
      INTEGER,INTENT(IN)::IW1,IW2
      INTEGER NEW
      INTEGER IT1,IT2,IT3,IALT
      REAL x1,y1,x2,y2,x3,y3,frac
      REAL distance_between_points,dist
      INTEGER I
      REAL  daL,dgs,duw,dvw,dte,dhu,dtr,dfL
      LOGICAL Lfeet
   
   
      x1=FLIGHT(IW1)%Longitude
      y1=FLIGHT(IW1)%Latitude
      x2=FLIGHT(IW2)%Longitude
      y2=FLIGHT(IW2)%Latitude
       if(y1.Lt.-90..or. y2.Lt.-90.) then
      print*,' y2.Lt.-90.,frac,x1,y1,x2,y2=',frac,x1,y1,x2,y2
      print*,'iw1,iw2',iw1,iw2
      Print*,'FLIGHT(IW1)',FLIGHT(IW1)
      Print*,'FLIGHT(IW2)',FLIGHT(IW2)
      Print*,'FLIGHT(IW2-1)',FLIGHT(IW2-1)
      print*,' stop'
      stop
      end if
      dist=distance_between_points(x1,y1,x2,y2)*RADIUS
!      if(dist.Lt.0.01) then
!      print*,'creategreatcircLe: dist.Lt.0.01,dist=',dist
!      print*,' FLIGHT(IW2)%aLtitude,FLIGHT(IW2)%aLtitude-FLIGHT(IW1)%aLtitude'  &
!      ,FLIGHT(IW2)%aLtitude,FLIGHT(IW2)%aLtitude-FLIGHT(IW1)%aLtitude
!      stop
!      end if
      IT1=FLIGHTadd(IW1)%ITW
      IT2=FLIGHTadd(IW2)%ITW
      NEW=IT2-IT1-1
    
! NEW = number of new points 
 
    
! IW1 = Last index before the gap 
 
   

 
    
      daL=FLIGHT(IW2)%aLtitude-FLIGHT(IW1)%aLtitude
      
       if(FLIGHT(IW1)%Longitude.Lt.-15..or.FLIGHT(IW2)%Longitude.Lt.-15.) daL=0.! north atLantic fLights
       
      dgs=FLIGHT(IW2)%groundspeed-FLIGHT(IW1)%groundspeed
      duw=FLIGHT(IW2)%u_component_of_wind-FLIGHT(IW1)%u_component_of_wind
      dvw=FLIGHT(IW2)%v_component_of_wind-FLIGHT(IW1)%v_component_of_wind
      dte=FLIGHT(IW2)%temperature-FLIGHT(IW1)%temperature
      dhu=FLIGHT(IW2)%specific_humidity-FLIGHT(IW1)%specific_humidity
      dtr=FLIGHT(IW2)%track-FLIGHT(IW1)%track
      dfL=0.3048*daL
  
 
      IT3=IT1
      Lfeet=.faLse.
         if(FLIGHT(IW1)%aLtitude.gt.18000.) then
      if(abs(fLight(IW1)%verticaL_rate).Lt.1.) then
      Lfeet=.true.
       end if
      end if
      DO I=IW1+1,IW2-1
      IT3=IT3+1
      frac=(I-IW1)/fLoat(Iw2-Iw1)  
      
      caLL intermediate_point(frac,x1,y1,x2,y2,x3,y3)
     
      FLIGHT(I)%NO=FLIGHT(IW1)%NO  
      FLIGHT(I)%fLight_id=FLIGHT(IW1)%fLight_id  
      FLIGHT(I)%Longitude=x3
      FLIGHT(I)%Latitude=y3
      if(y3.Lt.-90.) then
      print*,' y3.Lt.0.,frac,x1,y1,x2,y2,x3,y3=',frac,x1,y1,x2,y2,x3,y3
      Print*,' stop'
      print*,' FLLISTS',FLLISTS
      stop
      end if
     
      FLIGHT(I)%aLtitude=FLIGHT(IW1)%aLtitude+frac*daL
!      if(FLIGHT(I)%aLtitude.lt. flmin) then
!      print*,'creategreatcircle: FLIGHT(I)%aLtitude<flmin,,Iw1,iw2,FLIGHT(I)%aLtitude'  &
!      ,Iw1,iw2,FLIGHT(I)%aLtitude,flmin,'id=',FLIGHT(I)%flight_id
!      print*,' FLLISTS',FLLISTS
!      print*,
!      end if
      if(Lfeet) then
      IALT= FLIGHT(I)%aLtitude
      IALT=(IALT+500)/1000
      FLIGHT(I)%aLtitude=IALT*1000
      if(FLIGHT(I)%aLtitude.lt.-300.) then
      print*,'creategreatcircle 2: FLIGHT(I)%aLtitude<0.,Iw1,iw2,FLIGHT(I)%aLtitude,IALT'  &
      ,Iw1,iw2,FLIGHT(I)%aLtitude,IALT,'id=',FLIGHT(I)%flight_id
      print*,' FLLISTS',FLLISTS
      stop
      end if
      end if
      FLIGHT(I)%groundspeed=FLIGHT(IW1)%groundspeed+frac*dgs
      FLIGHT(I)%u_component_of_wind=FLIGHT(IW1)%u_component_of_wind +frac*duw
      FLIGHT(I)%v_component_of_wind=FLIGHT(IW1)%v_component_of_wind +frac*dvw
      FLIGHT(I)%temperature=FLIGHT(IW1)%temperature+frac*dte
      FLIGHT(I)%specific_humidity=FLIGHT(IW1)%specific_humidity +frac*dhu
      FLIGHT(I)%track=FLIGHT(IW1)%track+frac*dtr
      FLIGHT(I)%verticaL_rate=dfL/fLoat(IT2-IT1)   
      FLIGHTadd(I)%ITW=IT3
      fLightadd(I)%IoriginaL=0
      FLIGHTadd(I)%segmW=dist/NEW 
      FLIGHT(I)%timestamp= FLIGHT(Iw2)%timestamp 
      FLIGHT(I)%timestamp2= FLIGHT(Iw2)%timestamp2
      end do
        
  
      END SUBROUTINE  creategreatcircLe
      
      
      SUBROUTINE checkmonoton(NW)
      USE mo_directories,ONLY: FLIGHT,FLIGHTadd,NWX,IUTC0,istopall
      IMPLICIT NONE
      INTEGER,INTENT(IN)::NW
      INTEGER I,II
      
  
      do I=2,NW
      If(FLIGHTadd(I)%ITW.lt.FLIGHTadd(I-1)%ITW)  THEN
            istopall=istopall+1
            II=I
      print*,' checkmonoton: FLIGHTadd(I)%ITW.lt.FLIGHTadd(I-1)%ITW'  &
      ,'I,NW=',I,NW,'ITW(I),ITW(I-1)=',FLIGHTadd(I)%ITW,FLIGHTadd(I-1)%ITW &
      ,' istopall,II=',istopall,II
      stop
      end if
      end do
  
      if(istopall.gt.0) then
      Print*,'checkmonoton:  check for time monotonicity,istopall,I,NW =',istopall,II,NW
      print*,' FLIGHT(1)%flight_id',FLIGHT(1)%flight_id! 248753821
      open(11,file='test5_checkmonotontest.txt')
      do I=1,NW
      write(11,'(2I12)')I,FLIGHTadd(I)%ITW-IUTC0
      end do
      close(11)
      print*,'checkmonoton: test2 written,stop',NW
      stop
      end if
      END SUBROUTINE  checkmonoton
      
      SUBROUTINE check_IDT(NW)
      USE mo_directories,ONLY: FLIGHT,FLIGHTadd,NWX,IUTC0,idtmax0,istopall,nwmax
      IMPLICIT NONE
      INTEGER,INTENT(IN)::NW
      INTEGER I,idt,idtmaxmax,istop
      istop=0

      if(NW.ge.NWX) THEN
      print*,'check_IDT: NW.ge.NWX,NW,NWX=',NW,NWX
      stop
      end if
      nwmax=max(NW,nwmax)
       idtmaxmax=0
      do I=2,NW
      idt=FLIGHTadd(I)%ITW-FLIGHTadd(I-1)%ITW
      if(idt.le.0) then
      istop=I
      print*,'check_IDT: idt.le.0',idt,' at istop=',istop
      stop
      end if
      idtmaxmax=max(idtmaxmax,idt)
      
      end do
    
   
 
      if(istopall.gt.0) then
        Print*,'check_IDT:  check for time monotonicity,istopall =',istopall
      Print*,' idtmaxmax=',idtmaxmax,' nwmax=',nwmax
         open(10,file='test5_check_IDTtest.txt')
      write(10,'(3A12)') 'I','ITW','DT'
      do I=1,NW
      write(10,'(3I12)') I,FLIGHTadd(I)%ITW-IUTC0,FLIGHTadd(I)%ITW-FLIGHTadd(Max(1,I-1))%ITW
      enddo
      close(10)
           print*,'stop in check_IDT for NW,istopall',NW,istopall
      
      stop
      end if
      END SUBROUTINE  check_IDT   
      
!      SUBROUTINE DTHIST(NW)
!      USE mo_directories,ONLY: FLIGHTadd,NWX,nhist
!      IMPLICIT NONE
!      INTEGER,INTENT(IN)::NW
!      INTEGER I,idt
!  
!      if(NW.ge.NWX) THEN
!      print*,'DTHIST: NW.ge.NWX,NW,NWX=',NW,NWX
!      stop
!      end if
!       do I=2,NW
!      idt=FLIGHTadd(I)%ITW-FLIGHTadd(I-1)%ITW
!      if(IDT.gt.0.and. idt .le.nwx) then 
!      nhist(IDT)=nhist(IDT)+1
!      end if
!      end do
!      END SUBROUTINE  DTHIST
      
      
      
      subroutine eliminatezerodist(NW)
! 
! eliminates points that have zeri distance to next one
      use mo_cocip_parameters,only: UNITY,PIR180
      USE mo_directories,ONLY: FLIGHT
      IMPLICIT NONE
      INTEGER,INTENT(INout)::NW
      REAL x1,y1,x2,y2
      REAL dist,dx,dy,UNITX
      INTEGER I,NWNEW,inew(NW),K,J
      logical leliminate,lcopy
!      return
    
     
      NWNEW=1
      lcopy=.false.
      inew(NWNEW)=1
      do I=2,NW  
      x1=FLIGHT(I-1)%longitude
      y1=FLIGHT(I-1)%latitude
      x2=FLIGHT(I)%longitude
      y2=FLIGHT(I)%latitude
      dist=abs(x2-x1)+abs(y2-y1)  
      UNITX=0.5*(y1+y2)*PIR180
      UNITX=UNITY*cos(UNITX)
      dx=(x2-x1)*UNITX
      dy=(y2-y1)*UNITY
      dist=dx*dx+dy*dy
      
      
      leliminate = dist.lt.10. 
      if(leliminate) then
       lcopy=.true.
       else
      NWNEW=NWNEW+1
      inew(NWNEW)=I
      end if
      end do
      
      if(lcopy) then
      do J=1,NWNEW
      K=INEW(J)
      if(K.ne.J) then
      FLIGHT(J)=FLIGHT(INEW(J))
      end if
      end do
      end if
      
     
   
      NW=NWNEW
     
      
      END SUBROUTINE  eliminatezerodist
  



    
    
      SUBROUTINE sgstastest(NW)
! computes      flightadd(NW)%cosaw,sinaw,GSDISTW
    use mo_directories,only: flightadd,flight,distlimit,lpr
    use mo_cocip_parameters,only: UNITY,PIR180,RADIUS,gamma,R0 
 
      IMPLICIT NONE
! computes GS->tas 
      INTEGER,INTENT(IN):: NW
       INTEGER I,IDT
  
      real x1
      real y1
      real x2
      real y2     
      real u
      real v
       real unitx,dx,dy,alpha,gsx,gsy,VTAS,dist  &
          ,distance_between_points
          if(NW.le.1) then
           print*,' sgstastest(NW) <= 1 :',NW
!          stop
          end if
       do I=1,NW
      if(abs(flight(I)%vertical_rate).gt.50.001) then
!      print*,'eliminateINVALID: flight(I)%vertical_rate,inv,iori'  &
!      ,flight(I)%vertical_rate  &
!      ,flightadd(I)%INVALID, flightadd(I)%ioriginal
!      flight(I)%vertical_rate=0.
      FLIGHTadd(I)%INVALID=1
!      if(FLIGHTadd(I)%ioriginal.eq.1) stop
      end if
      end do
       do I=1,NW-1 
      idt=flightadd(I+1)%ITW-flightadd(I)%ITW      
      y1=FLIGHT(I)%latitude   
      y2=FLIGHT(I+1)%latitude
      x1=FLIGHT(I)%longitude
      x2=FLIGHT(I+1)%longitude      
      UNITX=0.5*(y1+y2)*PIR180
      UNITX=UNITY*cos(UNITX)
      dx=(x2-x1)*UNITX
      dy=(y2-y1)*UNITY
      dist=dx*dx+dy*dy
      
       if(dist.gt.1.E8) then
 
    dist= distance_between_points(x1,y1,x2,y2)*RADIUS
    if(LPR) print*,' I,dist',I,dist,FLIGHT(I)%flight_id
    else
     dist=sqrt(dist)
    end if
    
       flightadd(I)%segmw=dist
        FLIGHTadd(I)%GSDISTW=dist/max(1,idt)
       if(FLIGHTadd(I)%GSDISTW.gt.distlimit*2.) then
       if(LPR) print*,' in sgstastest: I,dist,id',I,FLIGHTadd(I)%GSDISTW,flight(I)%flight_id
       end if
    if(IDT.gt.0) then
    flightadd(I)%GSDISTW=dist/idt
      else
    flightadd(I)%GSDISTW=flight(I)%groundspeed
      end if
    
      if(dist.gt. 10.) THEN
      alpha=atan2(dy,dx)
!      print*,'alpha',alpha
       FLIGHTadd(I)%sinaw=sin(alpha)
       FLIGHTadd(I)%cosaw=cos(alpha)
!
      else
       FLIGHTadd(I)%sinaw=0.
       FLIGHTadd(I)%cosaw=1.
       FLIGHTadd(I)%INVALID=1
      end if
 
      
      end do     
      flightadd(NW)%segmw=1.
 
      FLIGHTadd(NW)%cosaw=FLIGHTadd(NW-1)%cosaw
      FLIGHTadd(NW)%sinaw=FLIGHTadd(NW-1)%sinaw
 
       FLIGHTadd(NW)%GSDISTW=0.
  
          
      END SUBROUTINE  sgstastest
      
    
    
      SUBROUTINE sgstas(NW,IPS)
    use mo_directories,only: flightadd,flight,distlimit,nps,LPR
    use mo_cocip_parameters,only: UNITY,PIR180,RADIUS,gamma,R0 
 
      IMPLICIT NONE
      INCLUDE "psbeta3.inc"
! computes GS->tas 
      INTEGER,INTENT(IN):: NW,IPS
       INTEGER I,IDT
  
      real x1
      real y1
      real x2
      real y2     
      real u
      real v
       real unitx,dx,dy,alpha,gsx,gsy,VTAS,dist  &
          ,distance_between_points
          real grdsp(NW)
          if(NW.le.1) then
          print*,' sgstas(NW)',NW
!          stop
          end if
       do I=1,NW-1 
       idt=flightadd(I+1)%ITW-flightadd(I)%ITW      
      y1=FLIGHT(I)%latitude   
      y2=FLIGHT(I+1)%latitude
      x1=FLIGHT(I)%longitude
      x2=FLIGHT(I+1)%longitude      
      UNITX=0.5*(y1+y2)*PIR180
      UNITX=UNITY*cos(UNITX)
      dx=(x2-x1)*UNITX
      dy=(y2-y1)*UNITY
      dist=dx*dx+dy*dy
      
       if(dist.gt.1.E8) then
 
    dist= distance_between_points(x1,y1,x2,y2)*RADIUS
    IF(LPR)   print*,'sgstas: I,dist',I,dist,FLIGHT(I)%flight_id   
    else
     dist=sqrt(dist)
    end if
    
       flightadd(I)%segmw=dist
        FLIGHTadd(I)%GSDISTW=dist/max(1,idt)
       if(FLIGHTadd(I)%GSDISTW.gt.distlimit) then
       FLIGHTadd(I)%invalid=1
       FLIGHTadd(min(NW,I+1))%invalid=1
!       print*,' in sgstas: I,dist',I,dist
       end if
    if(IDT.gt.0) then
    flightadd(I)%GSDISTW=dist/idt
    FLIGHTadd(I)%CLRW=0.3048*(flight(I+1)%altitude-flight(I)%altitude)/idt
    else
    flightadd(I)%GSDISTW=flight(I)%groundspeed
    FLIGHTadd(I)%CLRW=flight(I)%vertical_rate
    FLIGHTadd(I)%invalid=1
    FLIGHTadd(I+1)%invalid=1
    end if
    
      if(dist.gt. 10.) THEN
      alpha=atan2(dy,dx)
!      print*,'alpha',alpha
       FLIGHTadd(I)%sinaw=sin(alpha)
       FLIGHTadd(I)%cosaw=cos(alpha)
!
      else
      alpha=0.
!      print*,' stop (DX*DX+DY*DY)',dx,dy
      FLIGHTadd(I)%TASW=vtas
      FLIGHTadd(I)%sinaw=0.
       FLIGHTadd(I)%cosaw=1.
       FLIGHTadd(I)%invalid=1
      end if
      gsx=flight(I)%groundspeed*FLIGHTadd(I)%cosaw
      gsy=flight(I)%groundspeed*FLIGHTadd(I)%sinaw
      U=0.5*(FLIGHT(I)%u_component_of_wind+FLIGHT(I+1)%u_component_of_wind)
      V=0.5*(FLIGHT(I)%v_component_of_wind+FLIGHT(I+1)%v_component_of_wind)
      VTAS=sqrt((gsx-U)**2+(gsy-V)**2)
      FLIGHTadd(I)%TASW=vtas     
  
      end do     
      flightadd(NW)%segmw=1.
      FLIGHTadd(NW)%TASW=FLIGHTadd(NW-1)%TASW
      FLIGHTadd(NW)%cosaw=FLIGHTadd(NW-1)%cosaw
      FLIGHTadd(NW)%sinaw=FLIGHTadd(NW-1)%sinaw
      FLIGHTadd(NW)%CLRW=FLIGHTadd(NW-1)%CLRW
       FLIGHTadd(NW)%GSDISTW=0.
       FLIGHTadd(NW)%dgsdtW=0
       
       
         do I=1,NW
       if(FLIGHTadd(I)%ioriginal.eq.1) then
       grdsp(I)=FLIGHT(I)%groundspeed
       else
       grdsp(I)=FLIGHTadd(I)%GSDISTW
       end if
       end do
       do I=1,NW-1
       FLIGHTadd(I)%dgsdtW=(grdsp(I+1)-grdsp(I))/(FLIGHTadd(I+1)%itw-FLIGHTadd(I)%itw) 
       end do
       FLIGHTadd(NW)%dgsdtW=0
       do I=2,NW
       FLIGHTadd(I)%dgsdtW=((grdsp(I)-grdsp(I-1))/(FLIGHTadd(I)%itw-FLIGHTadd(I-1)%itw) +FLIGHTadd(I)%dgsdtW)*0.5
!       print*,'FLIGHTadd(I)%dgsdtW',I,FLIGHTadd(I)%dgsdtW
       if(abs(FLIGHTadd(I)%dgsdtW).gt.3.) FLIGHTadd(I)%invalid=1
       end do
    
           
       
     
 !     do I=1,NW-1
 !     FLIGHTadd(I)%dgsdtW=(FLIGHTadd(I+1)%TASW-FLIGHTadd(I)%TASW)/max(1,(FLIGHTadd(I+1)%ITW-FLIGHTadd(I)%ITW)) 
 !      end do
 !     FLIGHTadd(NW)%dgsdtW=FLIGHTadd(NW-1)%dgsdtW
      do I=1,NW
        Flightadd(I)%Machw= FLIGHTadd(I)%TASW/sqrt((gamma*R0)*FLIGHT(I)%temperature) 
        Flightadd(I)%Machw=MIN(Flightadd(I)%Machw,pMMO(IPS))
        end do   
          
      END SUBROUTINE  sgstas
      
      
         
    
      SUBROUTINE sdistcheck(NW)
    use mo_directories,only: flightadd,flight
    use mo_cocip_parameters,only: UNITY,PIR180,RADIUS,gamma,R0 
 
      IMPLICIT NONE
! computes GS->tas 
      INTEGER,INTENT(IN):: NW
        INTEGER I,IDT
  
      real x1
      real y1
      real x2
      real y2     

       real unitx,dx,dy,dist
          if(NW.le.1) then
          print*,' sdistcheck',NW
!          stop
          end if
       do I=1,NW-1 
      idt=flightadd(I+1)%ITW-flightadd(I)%ITW      
      y1=FLIGHT(I)%latitude   
      y2=FLIGHT(I+1)%latitude
      x1=FLIGHT(I)%longitude
      x2=FLIGHT(I+1)%longitude      
      UNITX=0.5*(y1+y2)*PIR180
      UNITX=UNITY*cos(UNITX)
      dx=(x2-x1)*UNITX
      dy=(y2-y1)*UNITY
      dist=dx*dx+dy*dy
      dist=sqrt(dist)
      
       flightadd(I)%segmw=dist
   
      if(dist.gt. 10.) THEN
       else
  
   flightadd(I)%INVALID=1
 
      end if
 !     print*,'in sdistcheck invalid,dist,x1,x2,y1,y2,segmw'  &
 !      ,dist,x1,x2,y1,y2,flightadd(I)%segmw,flightadd(I)%INVALID
          
      end do     
!        stop
      END SUBROUTINE  sdistcheck
    
    
    
  subroutine testdist
    USE mo_directories,ONLY: lpr
       USE mo_cocip_parameters,only: RADIUS,UNITy,PIR180
       IMPLICIT NONE
       REAL x1,x2,y1,y2,dist1,dist,distance_between_points
       INTEGER I
     x1=0
     y1=80.
     y2=80.
     do I=1,100
     x2=I*0.2
     dist=(((x2-x1)*cos(0.5*(y1+y2)*PIR180))**2+(y2-y1)**2)
     dist1=sqrt(dist)*UNITy
     dist=distance_between_points(x1,y1,x2,y2)*RADIUS
     print*,' dist1,dist',dist1,DIST,(dist1-DIST)/dist
     end do
         if(LPR)print*,' end of testdist'
!    stop
  END SUBROUTINE  testdist
  
       
      subroutine smooth(NW)
      USE mo_directories,ONLY: FLIGHT,FLIGHTadd,nsmooth,iutc0,lpr
      IMPLICIT NONE
      INTEGER,INTENT(INout)::NW
      INTEGER I,yyyy,mm,dd,hh,minmin,sec,ddold,IDAYY,NNew,J,NMean,iori
      INTEGER,parameter:: IS=17
      INTEGER*8 ITw8
      ddold=-1
        if(LPR) print*,' nw',Nw

 
    NNew=0
    do I=1,NW,nsmooth
    if(flightadd(I)%INVALID.eq.0) then 
    NNew=nnew+1
    NMean=1    
    FLIGHT(NNew)=FLIGHT(I)
    ITw8=FLIGHTadd(I)%ITw   
     J=I+1
!     print*,' I,J,NW,NNew',I,J,NW,NNew
!     print*,' FLIGHTadd(I)%ITw',FLIGHTadd(I)%ITw
!     print*,' nsmooth',nsmooth
!     print*,' FLIGHTadd(min(J,NW))%ITw-FLIGHTadd(I)%ITw', &
!     FLIGHTadd(min(J,NW))%ITw-FLIGHTadd(I)%ITw
     iori=FLIGHTadd(I)%IORIGINAL
      do while    (J.lt.NW.and. FLIGHTadd(min(J,NW))%ITw-FLIGHTadd(I)%ITw.le. nsmooth.and. NMean.lt.nsmooth) 
 
    
    FLIGHT(NNew)%latitude=FLIGHT(NNew)%latitude+FLIGHT(J)%latitude
    FLIGHT(NNew)%longitude=FLIGHT(NNew)%longitude+FLIGHT(J)%longitude
    FLIGHT(NNew)%altitude=FLIGHT(NNew)%altitude+FLIGHT(J)%altitude
    FLIGHT(NNew)%groundspeed=FLIGHT(NNew)%groundspeed+FLIGHT(J)%groundspeed
    FLIGHT(NNew)%track=FLIGHT(NNew)%track+FLIGHT(J)%track
    FLIGHT(NNew)%vertical_rate=FLIGHT(NNew)%vertical_rate  &
    +FLIGHT(J)%vertical_rate
    FLIGHT(NNew)%u_component_of_wind=FLIGHT(NNew)%u_component_of_wind  &
    +FLIGHT(J)%u_component_of_wind
    FLIGHT(NNew)%v_component_of_wind=FLIGHT(NNew)%v_component_of_wind  &
    + FLIGHT(J)%v_component_of_wind
    FLIGHT(NNew)%temperature=FLIGHT(NNew)%temperature+FLIGHT(J)%temperature
    FLIGHT(NNew)%specific_humidity=FLIGHT(NNew)%specific_humidity  &
    +FLIGHT(J)%specific_humidity
    ITw8=ITw8+FLIGHTadd(J)%ITw
      NMean=NMean+1  
      iori=min(IORI,FLIGHTadd(J)%IORIGINAL)

    
        J=J+1
    end do
!     print*,'I,J,NW,NMean,NNew',I,J,NW,NMean,NNew
    FLIGHT(NNew)%latitude=FLIGHT(NNew)%latitude/NMean
    FLIGHT(NNew)%longitude=FLIGHT(NNew)%longitude/NMean
    FLIGHT(NNew)%altitude=FLIGHT(NNew)%altitude/NMean
    FLIGHT(NNew)%groundspeed=FLIGHT(NNew)%groundspeed/NMean
    FLIGHT(NNew)%track=FLIGHT(NNew)%track/NMean
    FLIGHT(NNew)%vertical_rate=FLIGHT(NNew)%vertical_rate/NMean
    FLIGHT(NNew)%u_component_of_wind=FLIGHT(NNew)%u_component_of_wind/NMean
    FLIGHT(NNew)%temperature=FLIGHT(NNew)%temperature/NMean
    FLIGHT(NNew)%v_component_of_wind=FLIGHT(NNew)%v_component_of_wind/NMean
        FLIGHT(NNew)%specific_humidity=FLIGHT(NNew)%specific_humidity/NMean
    FLIGHTadd(NNew)%ITw=ITw8/NMEAN
     FLIGHTadd(NNew)%IORIGINAL=IORI
 
!          print*,' FLIGHTadd(NNew)%ITw-iutc0',FLIGHTadd(NNew)%ITw-iutc0
     end if

     
    end do
    NW=NNew
    if(LPR) print*,'smooth,nw=',nw
    
    END SUBROUTINE  smooth
    
    
      subroutine smoothdt2(NW)
      USE mo_directories,ONLY: FLIGHT,FLIGHTadd,nsmooth,iutc0,lpr
      IMPLICIT NONE
      INTEGER,INTENT(IN)::NW
      INTEGER I
      real,dimension(NW):: lon,lat
      real,Parameter:: drittel=1./3.
    
        if(LPR) print*,'smoothdt2: nw',Nw
    LAT(1)=FLIGHT(1)%latitude
    lat(NW)=FLIGHT(NW)%latitude
    LON(1)=FLIGHT(1)%longitude
    LON(NW)=FLIGHT(NW)%longitude
    do I=2,NW-1    
    lat(I)=drittel*(FLIGHT(I-1)%latitude+FLIGHT(I)%latitude+FLIGHT(I+1)%latitude)
    lon(I)=drittel*(FLIGHT(I-1)%longitude+FLIGHT(I)%longitude+FLIGHT(I+1)%longitude)
    end do 
    FLIGHT(1:NW)%latitude=lat(1:NW)    
    FLIGHT(1:NW)%longitude=lon(1:NW)    
    if(LPR) print*,'smoothdt2,nw=',nw
    
    END SUBROUTINE  smoothdt2
    
      subroutine smoothdvdt(NW)
      USE mo_directories,ONLY: FLIGHTadd,nsmooth,lpr
      IMPLICIT NONE
      INTEGER,INTENT(IN)::NW
      INTEGER I
      real,dimension(NW):: acc
      real,Parameter:: drittel=1./3.
    
        if(LPR) print*,'smoothdvdt: nw',Nw
    acc(1)=FLIGHTadd(1)%dgsdtw
    acc(NW)=FLIGHTadd(NW)%dgsdtw
   
    do I=2,NW-1  
    if(FLIGHTadd(I-1)%invalid+FLIGHTadd(I)%invalid+FLIGHTadd(I+1)%invalid.eq.0) then
    acc(I)=drittel*(FLIGHTadd(I-1)%dgsdtw+FLIGHTadd(I)%dgsdtw+FLIGHTadd(I+1)%dgsdtw)
    else
    acc(I)=FLIGHTadd(I)%dgsdtw
    end if
  
    end do 
    FLIGHTadd(1:NW)%dgsdtw=acc(1:NW)    

    if(LPR) print*,'smoothdvdt,nw=',nw
    
    END SUBROUTINE  smoothdvdt
    
    
          subroutine sFLcruise(NW)
       USE mo_directories,ONLY: FLIGHT,FLLISTS,FLIGHTadd,Nsmooth,lpr &
       ,IFLMAX,IFLinitialcruise,IS
      IMPLICIT NONE
      INTEGER,INTENT(IN)::NW
      INTEGER I,Istart,J,JSIC
      real FLmax
      JSIC=0
        if(LPR) print*,'sFLcruise: nw',Nw
            Istart=max(1,100/Nsmooth)
     IFLMAX=1           
    FLmax=0
    do I=1+Istart,NW-Istart  
       if(FLIGHTadd(I-1)%ioriginal.gt.0 .and.FLIGHTadd(I)%ioriginal.gt.0) then    
    if(FLIGHT(I)%altitude.eq.FLIGHT(I-1)%altitude.and. FLIGHT(I-1)%vertical_rate.eq.0.  &
    .and. FLIGHT(I)%vertical_rate.eq.0.) then
    if(FLIGHT(I)%altitude.gt.flmax)then
    IFLMAX=I
    flmax=FLIGHT(I)%altitude
    end if
    end if
    end if
    end do
    if(LPR) print*,' flmax',flmax
  
   IFLinitialcruise=1
    if(IFLMAX.gt.1) then
 
     JSIC=0
!     print*,' IFLMAX,min(IFLMAX,NW/4),1+Istart'  &
!     ,IFLMAX,min(IFLMAX,NW/2),1+Istart
  
    DOI: do I=min(IFLMAX,NW/2),1+Istart,-1
    if(FLIGHT(I)%altitude.lt.FLIGHT(IFLMAX)%altitude-800.  &
    .and. FLIGHT(I)%altitude.gt.max(flmax-10000.,3000.)) then
    if(FLIGHTadd(I-1)%ioriginal.gt.0.and. FLIGHTadd(I)%ioriginal.gt.0) then    
    if(FLIGHT(I)%altitude.eq.FLIGHT(I-1)%altitude.and.  &
    abs(FLIGHT(I)%vertical_rate).lt. 1. .and. abs(FLIGHT(I-1)%vertical_rate).lt. 1.) then
    JSIC=1
!    print*,'JSIC',JSIC
    doj: do J=I,1+Istart,-1
     if(FLIGHTadd(J)%ioriginal.gt.0) then    
     if(abs(FLIGHT(J)%vertical_rate).lt. 1.)THEN
    if(FLIGHT(J)%altitude.eq.FLIGHT(I)%altitude) then
    JSIC=JSIC+1
!    print*,'JSIC',JSIC
    if(jsic.gt.30/nsmooth) IFLinitialcruise=J
    end if
    end if
    end if
    end do doj
    if(jsic.gt.30/nsmooth) then
    exit
    end if 
    end if 
    end if 
    end if
    end do DOI

    end if

    
!    write(21,*) FLLISTS%flight_id,' ',FLLISTS%aircraft_type,' '  &
!    ,IFLinitialcruise,FLIGHT(IFLinitialcruise)%altitude  & 
!    ,IFLMAX,FLIGHT(IFLMAX)%altitude  &
!    ,(FLLISTS%flown_distance*1.830)
!    print*,FLLISTS%flight_id,' ',FLLISTS%aircraft_type,' '  &
!    ,IFLinitialcruise,FLIGHT(IFLinitialcruise)%altitude  & 
!    ,IFLMAX,FLIGHT(IFLMAX)%altitude  &
!    ,(FLLISTS%flown_distance*1.830)

    END SUBROUTINE  sFLcruise
    
    
      subroutine correctend(NW)
! corrects for unrealistic flight levels at start or departure
      USE mo_directories,ONLY: FLIGHT,FLIGHTadd,FLLISTS,flUAS,lpr
      IMPLICIT NONE
       INTEGER,INTENT(INout)::NW
      INTEGER I,nwfraction,nwold,nwend
      real flmin
      nwold=NW
      nwfraction=min(NW/10,100)
      nwfraction=max(nwfraction,1)
      
        flmin=minval(FLIGHT(2:nwfraction)%altitude)
        if(LPR) print*,' FLLISTS%xdep,FLLISTS%ydep',FLLISTS%xdep,FLLISTS%ydep

        if(LPR) print*,' flmin at departure',flmin,' fluas',fluas
        
        ifflmindep: IF(flmin.gt.fluas+FLLISTS%zdep) then
        NW=NW+1
        do I=NW,2,-1
        FLIGHTadd(I)=FLIGHTadd(I-1)
        FLIGHT(I)=FLIGHT(I-1)
        end do
        I=1
        FLIGHT(I)=FLIGHT(I+1)
        FLIGHT(I)%timestamp=FLLISTS%actual_offblock_time
        FLIGHT(I)%timestamp2=FLLISTS%actual_offblock_time(12:20)
        FLIGHT(I)%longitude=FLLISTS%xdep-0.01
        FLIGHT(I)%latitude=FLLISTS%ydep-0.01
        FLIGHT(I)%altitude=FLLISTS%zdep
        FLIGHTadd(I)=FLIGHTadd(I+1)
         FLIGHTadd(I)%ITW=FLLISTS%itwdep
         FLIGHTadd(I)%ioriginal=0
         FLIGHTadd(I)%INVALID=0
         FLIGHTadd(I)%segmW=100. 
         FLIGHT(I)%specific_humidity=1.e-5
        end if ifflmindep         
      
      
      
      
        NWEND=NW-nwfraction
        flmin=minval(FLIGHT(nwend:NW-1)%altitude)
        if(LPR) print*,' flmin at destination',flmin,' fluas',fluas
!        do I=nwend,NW
!        print*,'I,alt,lon,lat',I,FLIGHT(I)%altitude,FLIGHT(I)%longitude,FLIGHT(I)%latitude
!        end do
         if(LPR) print*,'correctend: FLLISTS%xdes,FLLISTS%ydes',FLLISTS%xdes,FLLISTS%ydes
        if(LPR) print*,' flmin,fluas',flmin,fluas
        ifflmin: IF(flmin.gt.fluas+FLLISTS%zdes) then
        NW=NW+1
        I=NW
        FLIGHT(I)=FLIGHT(I-1)
        FLIGHT(I)%timestamp=FLLISTS%arrival_time
        FLIGHT(I)%timestamp2= FLLISTS%arrival_time(12:20)
        FLIGHT(I)%longitude=FLLISTS%xdes+0.01
        FLIGHT(I)%latitude=FLLISTS%ydes+0.01
        FLIGHT(I)%altitude=FLLISTS%zdes
         FLIGHTadd(I)=FLIGHTadd(I-1)
         FLIGHTadd(I)%ITW=FLLISTS%itwdes
         FLIGHTadd(I)%ioriginal=0
         FLIGHTadd(I)%INVALID=0
         FLIGHTadd(I)%segmW=100.  
         FLIGHT(I)%specific_humidity=1.e-5         
        else ifflmin
        do I=NWEND+1,NW
        if(FLIGHT(I)%altitude.gt.FLIGHT(I-1)%altitude+300) THEN
        NW=I-1
        exit
        end if
        end do
 
    
        end if ifflmin

    
    END SUBROUTINE  correctend
    
 
      
      
 
      
      
      subroutine eliminateINVALID(NW)
! 
! eliminates points that have been flagged as invalid before
      use mo_cocip_parameters,only: UNITY,PIR180
      USE mo_directories,ONLY: FLIGHT,FLIGHTADD
      IMPLICIT NONE
      INTEGER,INTENT(INout)::NW
        INTEGER I,k,nwnew
      INTEGER inew(NW)
      logical lcopy
   
     
      NWNEW=0
      lcopy=.false.
       do I=1,NW     
       
      if(flightadd(I)%INVALID.eq.1) then
       lcopy=.true.
 
       else
      NWNEW=NWNEW+1
      inew(NWNEW)=I
      end if
  
      end do
      
      if(lcopy) then
      do I=1,NWNEW
      K=INEW(I)
      if(K.ne.I) then
      FLIGHT(I)=FLIGHT(INEW(I))
      flightadd(I)=flightadd(INEW(I))
      end if
      end do
      end if     
   
      NW=NWNEW
      
      END SUBROUTINE  eliminateINVALID
      
  
        
        
      subroutine SFLchange(NW)
! purpose: 
      USE mo_directories,ONLY: FLIGHT,FLIGHTadd,clrmax,clrmin,flUAS,lpr
      IMPLICIT NONE
      INTEGER,INTENT(INout)::NW
      INTEGER I,J,K,J1,J2,ijump(NW),IJUMP1,IJUMP2,njumps,ichange
      REAL dfL       
      ijump=0   
      njumps=0
      ichange=0
    
      if(LPR) print*,' FLIGHT(1)%altitude,FLIGHT(2)%altitude'  &
      ,FLIGHT(1)%altitude,FLIGHT(2)%altitude
   
      do I=1,NW-1
      dfL=FLIGHT(I+1)%altitude-FLIGHT(I)%altitude
      if(abs(dfl).gt.clrmax) then
      ijump(I)=1
      njumps=njumps+1
      end if
 !     print*,' I,ijump(I)',I,ijump(I),FLIGHT(I+1)%altitude,FLIGHT(I)%altitude
      end do   
   
   
      lsum: if(njumps.gt.0) then
      altcorrect: do I=2,NW-1
      jjj: if(ijump(I).gt.0) then
      IJUMP2=I
      IJUMP1=I-1
      dfL=(FLIGHT(I)%altitude-FLIGHT(I-1)%altitude)
      
      lori: if(flightadd(IJUMP1)%ioriginal.eq.0) then
      do WHILE((dfl.gt.clrmax.or. dfl.lt.clrmin).and. IJUMP1.gt.1)
      IJUMP1=IJUMP1-1
       dfl=(FLIGHT(IJUMP2)%altitude-FLIGHT(IJUMP1)%altitude)/(IJUMP2-IJUMP1)
      if(IJUMP1.LE.1) exit
      end do
      
      else lori
      
      do WHILE((dfl.gt.clrmax.or. dfl.lt.clrmin).and. IJUMP2.lt.NW)
      IJUMP2=IJUMP2+1
      dfl=(FLIGHT(IJUMP2)%altitude-FLIGHT(IJUMP1)%altitude)/(IJUMP2-IJUMP1)
      if(IJUMP2.GE.NW) exit
      end do      
      end if lori
     
      do J=IJUMP1+1,IJUMP2-1
      FLIGHT(J)%altitude= FLIGHT(IJUMP1)%altitude  & 
      +(J-IJUMP1)*((FLIGHT(IJUMP2)%altitude-FLIGHT(IJUMP1)%altitude)/(IJUMP2-IJUMP1))
      ichange=ichange+1
      end do
    
      end if jjj
      end do altcorrect
      end if lsum
      if(LPR) print*,' after lsum: ichange=',ichange
      
      dfl=(FLIGHT(NW)%altitude-FLIGHT(NW-1)%altitude)
!      highabovedes: if(dfl.lt.clrmin) then
       
      IJUMP1=NW-1
      IJUMP2=NW
    
      do WHILE(dfl.lt.clrmin)
      IJUMP1=IJUMP1-1
      dfl=(FLIGHT(IJUMP2)%altitude-FLIGHT(IJUMP1)%altitude)/(IJUMP2-IJUMP1)
      if(IJUMP1.eq.1) exit
      end do
      
 
   
      do J=IJUMP1+1,IJUMP2-1
      FLIGHT(J)%altitude= FLIGHT(IJUMP1)%altitude  & 
      +(J-IJUMP1)*((FLIGHT(IJUMP2)%altitude-FLIGHT(IJUMP1)%altitude)/(IJUMP2-IJUMP1))
      if(LPR) print*,' J,FLIGHT(J)%altitude',J,FLIGHT(J)%altitude
      ichange=ichange+1
      flightadd(J)%ioriginal=0
      end do
           
 !     end if    highabovedes 
      if(LPR) print*,' after highabovedes: ichange=',ichange
      dfl=(FLIGHT(2)%altitude-FLIGHT(1)%altitude)
!       highabovedep:    if(FLIGHT(1)%altitude.gt.flUAS  &
!       .or.FLIGHT(2)%altitude.gt.flUAS) then
  
      IJUMP1=1
      IJUMP2=2
      dfl=(FLIGHT(2)%altitude-FLIGHT(1)%altitude)
 !     print*,'1: IJUMP1,IJUMP2,dfl,clrmax',IJUMP1,IJUMP2,dfl,clrmax
      do WHILE(dfl.gt.clrmax)
      IJUMP2=IJUMP2+1
      dfl=(FLIGHT(IJUMP2)%altitude-FLIGHT(IJUMP1)%altitude)/(IJUMP2-IJUMP1)
      if(IJUMP2.ge.NW/2) exit
      if(LPR) print*,' IJUMP1,IJUMP2,dfl,clrmax',IJUMP1,IJUMP2,dfl,clrmax
      end do
  
         do J=IJUMP1+1,IJUMP2-1
      FLIGHT(J)%altitude= FLIGHT(IJUMP1)%altitude  & 
      +(J-IJUMP1)*((FLIGHT(max(1,IJUMP2))%altitude-FLIGHT(IJUMP1)%altitude)/(IJUMP2-IJUMP1))
       ichange=ichange+1
      flightadd(J)%ioriginal=0
       end do    
      
 !    end if    highabovedep 
      
      if(LPR) print*,' after highabovedep: ichange=',ichange
      if(LPR) print*,' FLIGHT(1:9)%altitude',FLIGHT(1:9)%altitude
      if(LPR) print*,' FLIGHT(NW-8:NW)%altitude',FLIGHT((NW-8):NW)%altitude
      if(LPR) print*,' after highabovedep,-248766251=?',flight(1)%flight_id
        END SUBROUTINE  SFLchange   
      
         subroutine addends(NW)
 ! includes airport coordinates at beginning and end of trajectory
           USE mo_directories,ONLY: FLIGHT,FLLISTS,FLIGHTadd,lpr
           use mo_cocip_parameters,only: UNITY,PIR180
           IMPLICIT NONE
           INTEGER,INTENT(InOUT)::NW
       REAL x1,x2,y1,y2,dist
       INTEGER NWold,I1,i2,I
       NWold=NW
       if(LPR) print*,'addends: NW,flight(1)%altitude,flight(2)%altitude'  &
       ,NW,flight(1)%altitude,flight(2)%altitude
       if(LPR) print*,'addends: NW,flight(NW-1)%altitude,flight(NW)%altitude'  &
       ,NW,flight(NW-1)%altitude,flight(NW)%altitude
   
       I1=0
       I2=0
         x1=FLLISTS%xdep
         y1=FLLISTS%ydep
         x2=flight(1)%longitude
         y2=flight(1)%latitude
        dist=(((x2-x1)*cos(0.5*(y1+y2)*PIR180))**2+(y2-y1)**2)
        dist=sqrt(dist)*UNITy
       
        nw1: if(dist.gt.10000.) then
         if(LpR)        print*,'1:dist',dist
        I1=1
        do I=NW+1,2,-1
        flight(I)=flight(I-1)
        FLIGHTadd(I)=FLIGHTadd(I-1)
        end do
        flight(1)=flight(2)
        flight(1)%longitude=FLLISTS%xdep
        flight(1)%latitude=FLLISTS%ydep
        flight(1)%altitude=FLLISTS%zdep
        flight(1)%groundspeed=0.
        flight(1)%vertical_rate=0.
          if(LPR)         print*,'test: FLLISTS%ITWdep,FLIGHTadd(1)%ITW-',FLLISTS%ITWdep,FLIGHTadd(1)%ITW
       
         FLIGHTadd(1)%ITW=MIN(FLLISTS%ITWdep,FLIGHTadd(1)%ITW-1)
        FLIGHTadd(1)%ioriginal=0
        FLIGHTadd(1)%invalid=0
        
  
        if(LPR) print*,'addends: flight(1)%altitude,flight(2)%altitude'  &
       ,flight(1)%altitude,flight(2)%altitude
        NW=NW+1
        end if NW1
        x1=FLLISTS%xdes
         y1=FLLISTS%ydes
         x2=flight(NW)%longitude
         y2=flight(NW)%latitude
        dist=(((x2-x1)*cos(0.5*(y1+y2)*PIR180))**2+(y2-y1)**2)
        dist=sqrt(dist)*UNITy
    
        if(dist.gt.10000.) then
        if(LPR)   print*,' 2:dist',dist
        I2=1
        NW=NW+1
         flight(NW)=flight(NW-1)
        flight(NW)%longitude=FLLISTS%xdes
        flight(NW)%latitude=FLLISTS%ydes
        flight(NW)%altitude=FLLISTS%zdes
        flight(NW)%groundspeed=0.
        flight(NW)%vertical_rate=0.
        FLIGHTadd(NW)%ITW=FLLISTS%ITWdes+60
        FLIGHTadd(NW)%ioriginal=0
        FLIGHTadd(NW)%invalid=0
        end if
        if(Nw.ne.nwold) THEN
        
     
     
         if(LPR) THEN
          print*,' addends NWold,NW=',NWold,NW,' ends added: at dep,des=',I1,I2
          print*,'addends: NW,flight(1)%altitude,flight(2)%altitude'  &
         ,NW,flight(1)%altitude,flight(2)%altitude
          print*,'addends: NW,flight(NW-1)%altitude,flight(NW)%altitude'  &
         ,NW,flight(NW-1)%altitude,flight(NW)%altitude
          print*,' flight(1)%flight_id',flight(1)%flight_id
          print*,' END  in addends,nw,nwold',nw,nwold
         end if
         END IF
         END SUBROUTINE  addends 
      
      
       subroutine eliminate_early_late_nolongerneeded(NW)
           USE mo_directories,ONLY: FLIGHT,FLLISTS,FLIGHTadd,lpr
     
           IMPLICIT NONE
        
           INTEGER,INTENT(InOUT)::NW
       INTEGER NWold,I1,i2,I,INEW,itdep,itdes
   
       NWOLD=NW
    
       I1=1 
    
       if(FLIGHTadd(1)%itw.lt.FLLISTS%itwdep)  then
        do I=1,NW
         if(FLIGHTadd(I)%itw.ge.FLLISTS%itwdep) then
        i1=I
        exit
        end if
        end do
        end if
        
        I2=NW 
        itdes=FLLISTS%itwdes     
    
        if(FLIGHTadd(NW)%itw.gt.FLLISTS%itwdes)  then
        if(LPR) print*,' FLIGHTadd(NW)%itw.gt.FLLISTS%itwdes',FLIGHTadd(NW)%itw,FLLISTS%itwdes
        do I= NW,I1,-1
        if(FLIGHTadd(I)%itw.le.FLLISTS%itwdes) then
        i2=I
        exit
        end if
        end do
        end if
        
        ifi1: if(I1.gt.1) then
        INEW=0
        do I=I1,I2
        INEW=INEW+1
        flight(Inew)=flight(I)
        flightadd(Inew)=flightadd(I)
         end do 
         NW=INeW
         else ifi1
         NW=I2
        end if ifi1
        if(I1.gt.1.or.I2.ne.NW) then
        
   if(LPR) print*,'eliminate_early_late: early/late parts eliminated: I1,i2,NW,NWOLD',I1,i2,NW,NWOLD
 
        end if
        END SUBROUTINE  eliminate_early_late_nolongerneeded
        
        
               subroutine eliminate_zeroalt(NW)
           USE mo_directories,ONLY: FLIGHT,FLIGHTadd,iutc0,clrmax,nsmooth,lpr
     
           IMPLICIT NONE
   
           INTEGER,INTENT(InOUT)::NW
       INTEGER NWold,I1,i2,I,INEW,itdep,itdes
       NWOLD=NW
       I1=0
   
        do I=1,NW
        if(FLIGHT(I)%altitude.eq.0.) then
        i1=I
        exit
        end if
        end do
         if(i1.gt.0) then 
        I2=i1-1
        do I= i1,nw
        if(FLIGHT(I)%altitude.ne.0) then
        i2=I2+1
         FLIGHTadd(I2)=FLIGHTadd(I)
         FLIGHT(I2)=FLIGHT(I)
        end if
        end do
 
        if(LPR) print*,' I2,nw',i2,nw
               NW=I2
!        do I=1,nw
!        print*,' I,FLIGHTadd(I)%itw-iutc0,alt',I,FLIGHTadd(I)%itw-iutc0,FLIGHT(I)%altitude
!        end do
           end if
       if(LPR)  print*,' eliminate_zeroalt,nwold,nw',nwold,nw
        I1=NW
        NWOLD=NW
        do I=max(2,NW-NSMOOTH+10),NW
        if(FLIGHT(I)%altitude.gt.FLIGHT(I-1)%altitude+clrmax) then
        i1=I-1
        exit
        end if
        end do
        if(I1.ne.NW) then
        if(LPR) print*,' end partelimiated',i1,NWOLD
        end if
        NW=I1
        
        
        END SUBROUTINE  eliminate_zeroalt
        
        
          subroutine eliminate_zerogs(NW)
 ! eliminates  data points with zero ground speed (FLIGHT(I)%groundspeed)
           USE mo_directories,ONLY: FLIGHT,FLIGHTadd,iutc0,clrmax,nsmooth
     
           IMPLICIT NONE
   
           INTEGER,INTENT(InOUT)::NW
       INTEGER NWold,I1,i2,I,INEW,itdep,itdes
       NWOLD=NW
 !      print*,' eliminate_zerogs', NW
        do I=1,NW
        if(FLIGHT(I)%groundspeed.eq.0.) then
        i1=I
        exit
        end if
        end do
       I1=1
        I2=NW/10
        do I=I2,1,-1
        if(FLIGHT(I)%groundspeed.eq.0.) then
        i1=I
        exit
        end if
        end do
 !       print*,'I1',I1
         if(i1.gt.1) then 
        I2=0
        do I= i1,nw
           i2=I2+1
         FLIGHTadd(I2)=FLIGHTadd(I)
         FLIGHT(I2)=FLIGHT(I)
        end do
 
!        print*,' I2,nw',i2,nw
               NW=I2
        end if
        
        I1=NW
 !       print*,'I1 before test alt,NW=I1=',I1
        do I=max((3*NW)/4,NW-100),NW
        if(FLIGHT(I)%altitude.gt.FLIGHT(I-1)%altitude+clrmax) then
        i1=I-1
!        print*,'FLIGHT(I)%altitude.gt.FLIGHT(I-1)%altitude+clrmax'  &
!        ,FLIGHT(I)%altitude,FLIGHT(I-1)%altitude,clrmax
        exit
        end if
        end do
!         print*,'I1 after test alt,NW=I1=',I1
        if(I1.ne.NW) then
 !       print*,' end part eliminated',i1,NWOLD
        end if
        NW=I1
        if(Nw.ne.nwold) print*,' eliminate_zerogs_', NWOLD,NW
        
        END SUBROUTINE  eliminate_zerogs
      
       
   
         
        
          subroutine eliminate_Tjumps(NW)
!          eliminates waypoints with unrealistic temperature jumps
           USE mo_directories,ONLY: FLIGHT,FLIGHTadd,lpr,nps
     
           IMPLICIT NONE
       INCLUDE "CPARA.inc"
   
           INTEGER,INTENT(In)::NW
       INTEGER I,nchanged,nchangeddvdt,idtmax,nvalid
       real tempdiffmax,tempdiff,dvdtmax,dvdt
   
 !      print*,' eliminate_zerogs', NW
         tempdiffmax=0
         nchanged=0
         nchangeddvdt=0
         idtmax=0
         dvdtmax=0.
         nvalid=0
         do I=1,NW
           if(abs(flight(I)%vertical_rate).gt.50.) then
         flightadd(I)%INVALID=1
         flight(I)%vertical_rate=0.
        end if
        end do
        do I=2,NW-1
        if(flightadd(I)%INVALID.eq.0) then
        nvalid=nvalid+1
        dvdt=abs(flightadd(I)%dgsdtW)
        dvdtmax=max(dvdtmax,dvdt)
        if(dvdt.gt.1.) then
        flightadd(I)%INVALID=1
        nchangeddvdt=nchangeddvdt+1
        end if      
      
        if(flightadd(I+1)%itw-flightadd(I-1)%itw.gt.0) then
        if(flightadd(I+1)%itw-flightadd(I-1)%itw.le.120) Then
        tempdiff=flight(I)%temperature-(flight(I-1)%temperature+flight(I+1)%temperature)*0.5
        tempdiffmax=max(tempdiffmax,tempdiff)
        If(abs(tempdiff).gt.5.) THEN
        flightadd(I)%INVALID=1
        nchanged=nchanged+1
        idtmax=max(idtmax,flightadd(I+1)%itw-flightadd(I-1)%itw)
        end if
        tempdiff=flight(I)%temperature-TISA(flight(I)%altitude*0.3042)
          If(abs(tempdiff).gt.30.) THEN
        flightadd(I)%INVALID=1
        nchanged=nchanged+1
         end if
        
        
        end if
        end if
        end if
        end do
        if(LPR) print*,' idtmax,tempdiffmax,nchanged',  &
        idtmax,tempdiffmax,nchanged
        if(LPR) print*,' NW,nvalid,dvdtmax,nchangeddvdt',NW,nvalid,dvdtmax,nchangeddvdt
   
        
        END SUBROUTINE  eliminate_Tjumps
      
       
       
       
                
        
      subroutine eliminate_FL(NW)
       USE mo_directories,ONLY: FLIGHT,FLIGHTadd,lpr
       IMPLICIT NONE
       INTEGER,INTENT(In)::NW
       INTEGER I,J,ninvalids,idt,Jmax,idtsum
       REAL,Parameter::qfeet=1./0.3042,dzlimit=200.
       REAL z,dz
       INTEGER,PARAMETER::NSTEP=10
       ninvalids=0
         do I=1,NW,nstep
       z=flight(I)%altitude
       dz=0.
       idtsum=0
       Jmax=min(NW-1,I+nstep-1)
       do J=I,JMax
       idt=(flightadd(J+1)%itw-flightadd(J)%itw)
       idtsum=idtsum+idt
       z=z+qfeet*flight(J)%vertical_rate*idt
       dz=dz+(z-flight(J)%altitude)*idt
!       print*,'I,J,idt,qfeet*flight(J)%vertical_rate,flight(J)%altitude,z,dz'  &
!       ,I,J,idt,qfeet*flight(J)%vertical_rate,flight(J)%altitude,z,dz
       end do
       if(abs(dz).gt.idtsum*dzlimit) THen
!       print*,'I,jmax,dz,dzlimit',I,jmax,dz,(1+Jmax-I)*dzlimit
       do J=I,JMax
       flightadd(J)%invalid=1
       end do
       ninvalids=ninvalids+JMax-I+1
       end if
       end do
       if(LPR) print*,'ninvalids', ninvalids,nstep,dzlimit
    
       end subroutine  eliminate_FL
       
      
    subroutine disttest2(NW)
           USE mo_directories,ONLY: FLIGHT,FLIGHTadd,lpr
           use mo_cocip_parameters,only: UNITY,PIR180,RADIUS
           IMPLICIT NONE        
           INTEGER,INTENT(In)::NW
       REAL x1,x2,y1,y2,dist,distance_between_points
          INTEGER I,J
    J=0
    DO I=1,NW-1
    x1=FLIGHT(I)%longitude
    x2=FLIGHT(I+1)%longitude
    y1=FLIGHT(I)%latitude
    y2=FLIGHT(I+1)%latitude
    dist=distance_between_points(x1,y1,x2,y2)*RADIUS
    if(abs(dist-flightadd(I)%segmw).gt.max(10.,0.05*dist)) then
    if(LPR) print*,' check dist,segmw',I,dist,flightadd(I)%segmw  &
    ,(dist-flightadd(I)%segmw)/max(dist,1.),FLIGHT(I)%flight_id
    J=J+1
    end if
    end do
    if(LPR) print*,'disttest2: J=',J
    if(J.gt.0) stop
    END SUBROUTINE  disttest2
   
          
      SUBROUTINE STakeoffmass1(IPS,Rt,FLoad,TOM,lnewoem)
      USE mo_directories,ONLY: FLIGHT,FLLISTS,lpr,NPS,IS
      use mo_cocip_parameters,only: g,LCVCC
! as proposed by Ian Poll:A method for the estimation of the take-off mass
      IMPLICIT NONE     
      INTEGER,INTENT(IN):: IPS
      REAL,INTENT(IN)::Rt,FLoad
      REAL,INTENT(OUT):: TOM
      logical,intent(INOUT):: lnewoem
      REAL m
      INCLUDE "psbeta3.inc"
      real MZFM,MTOM,OEM,etaLD,fexp
      if(LPR) print*,' IPS,Rt,FLoad',IPS,Rt,FLoad
      
   
         
      fexp=exp(-(0.014+1.015*(g*RT/(etaL_Do(IPS)*LCVCC))))-0.05
      fexp=max(fexp,0.1)
      if(fexp.lt.0.1) then
      print*,'STakeoffmass1: fexp.lt.0.1,fexp=',fexp
      stop
      end if
      if(LPR) print*,'STakeoffmass1: g,RT,etaLD,LCVCC,fexp',g,RT,etaL_Do(IPS),LCVCC,fexp
      TOM=(Fload*PMZFM(IPS)+(1.-FLOAD)*POEM(IPS))/fexp    
       END SUBROUTINE  STakeoffmass1
          
!      SUBROUTINE SMassfromMach(Mach,PMdes,clo,p,PSwingarea,mass)     
!      USE mo_cocip_parameters,ONLY: g      
!      real,intent(IN):: Mach,PMdes,clo,p,PSwingarea
!      real,intent(OUT)::mass
!      real CY17,Cz17,DA17
!      CY17=mach/PMdes
!      Cz17=1-43.363*((CY17-1.)**3)-4.9728*((CY17-1.)**2)-1.2027*(CY17-1.)
!      DA17=CZ17*clo
!      mass=DA17*0.7*p*mach**2*PSwingarea/g
 !     END SUBROUTINE  SMassfromMach
 
 
       SUBROUTINE M3_SMassfromDO(NW,IPS)   
! MassfromDO from design optimum (Ian Poll,2.10.2024),see SMassfromMach
       USE mo_directories,ONLY: FLIGHT,FLLISTS,FLIGHTadd,Nsmooth  &
       ,lpr,nairlinex,jairline  &
       ,NPS,massresult6,imassvalid,meanerrorac,IFLMAX
       
       USE mo_cocip_parameters,ONLY: g
       
      IMPLICIT NONE
      INTEGER,INTENT(IN)::NW,IPS
        INCLUDE "psbeta3.inc"
    
      real PICAOPA,CY17,Cz17,p,x1,x2,flmax,riflmax,massCLO
       INTEGER I,nwchecked,itime,nsum,NWV,istart,J
       INTEGER ITV(nw)
       real  er,TFM,masssum,tfmsum
       INTEGER,parameter:: IMETHOD=3
       imassvalid(IMETHOD)=0
        istart=0
        flmax=0.
       
   ifl2gt3: if(IFLMAX.gt.1.and. FLLISTS%flown_distance.gt. 200) then
   nwv=0
   do I=1,NW
   if (FLIGHTadd(I)%ioriginal.eq.1.and. FLIGHTadd(I)%invalid.eq.0) then
   NWV=NWV+1
   ITV(NWV)=I
   end if
   end do
 
    do0: do I=1,nwv
    J=ITV(I)  
   if(FLIGHT(J)%vertical_rate.gt.5..and.  & 
   FLIGHT(J)%altitude.gt.0 .and. FLIGHT(J)%altitude.lt.3000) then
   istart=J
   exit
   end if
  end do do0  
   end if ifl2gt3  
 
    
  
       llong: if(istart.gt.0) then
       nsum=0
       masssum=0.
       tfmsum=0.
       er=0.
      nwchecked=0
  
    
 
      iflmax=PMalthft(IPS)
      iflmax=iflmax/10
      iflmax=iflmax*10
      riflmax=iflmax*100-51
      itime=0
       if(LPR) print*,'SMassfrom DO: NW,IPS',NW,IPS
    
       FLIGHTadd(1:NW)%massfrom(IMETHOD)=Flightadd(1)%massw(IMETHOD) ! CLO
    
   
 
                 flmax=-1.e10
      loopi: do J=Istart,NWv
      I=Itv(J)
           flmax=max(flmax,FLIGHT(I)%altitude)
     
      lcruise: if(abs(FLIGHT(I)%vertical_rate).lt.1.) then
    
   flhigh: if (flmax.gt.(0.84*100.)*PMalthft(IPS)) then
      
         lhigh: if(FLIGHT(I)%altitude.gt.flmax-100) then
  
    
      lalt: if(FLIGHT(I)%altitude.gt.PMalthft(IPS)*100.*0.6) then
    
        CY17=FLIGHTadd(I)%machw/PMdes(IPS)
   
     
      lcy17: if(abs(1.-cy17).lt.0.1) Then
      Cz17=1-43.363*((CY17-1.)**3)-4.9728*((CY17-1.)**2)-1.2027*(CY17-1.)
      p=PICAOPA(FLIGHT(I)%altitude*0.3048)
      massCLO=CZ17*p*(FLIGHTadd(I)%machw**2)*(PSwingarea(IPS)*clo(IPS)*0.7/g)
    
        if(massCLO.lt.flightadd(I)%massw(IMETHOD)) then
  
        if(massCLO.gt.flightadd(I)%massw(1)) then
        tfmsum=flightadd(1)%massw(2)-flightadd(I)%massw(2)
        FLIGHTadd(I)%massfrom(IMETHOD)= massCLO+tfmsum ! CLO
        massCLO=massCLO+tfmsum
       
 !           print*,' I,massCLO=',I,massclo,flightadd(I)%massw(IMETHOD),flightadd(I)%massw(1) 
   
        nsum=nsum+1
        masssum=masssum+massCLO
     
        end if
        end if
      end if lcy17
      end if lalt
      end if lhigh
      end if flhigh
 
      end if lcruise
   
      nwchecked=nwchecked+1
  
       end do loopi
             if(LPR) print*,' nsum',nsum
       lnsum: if(nsum.gt. 1) then
       massCLO   =masssum/nsum  
       massCLO=massCLO/(1.-meanerrorac(IMETHOD,IPS,Jairline))
!!!!!!!!!!!!!!!!!!!! statistical correction (AI_CLO) 

  
    
      
             I=1
  
     
         
 
              if(LPR) print*
   
       
     itime=flightadd(I)%ITW-flightadd(1)%ITW
!      write(45,*) PICAO(IPS),FLLISTS%TOW,POEM(IPS),PMdes(IPS),PSwingarea(IPS),clo(IPS)  &
!       ,FLLISTS%flight_duration,FLLISTS%taxiout_time,FLLISTS%flown_distance ,itime/60.  &
!      ,FLIGHT(I)%altitude,p,FLIGHT(I)%temperature,FLIGHTadd(I)%machw  &
!      ,flightadd(1)%massw(1),flightadd(1)%massw(IMETHOD),flightadd(I)%massw(1),flightadd(I)%massw(IMETHOD) &
!      ,massCLO,((massCLO-flightadd(1)%massw(2))/flightadd(1)%massw(2))
      
      
        if(LPR) print*,' massCLO',massCLO,flightadd(1)%massw(1) ,flightadd(1)%massw(IMETHOD)
       if(massCLO.gt.flightadd(1)%massw(1)  &
       .and. massCLO.lt.flightadd(1)%massw(IMETHOD))   THen
         imassvalid(IMETHOD)=1
 
         massresult6(IMETHOD)=massCLO
          FLIGHTadd(nw)%massfrom(3)=massCLO
            if(LPR) print*,' 3 set ',massCLO
            
      
      
       end if

        end if lnsum
       end if llong
 
       END SUBROUTINE  M3_SMassfromDO
       
       
     subroutine M1_LOADF_wind(NW,IPS,IDAY,TOM)
        USE mo_directories, ONLY: flightadd,IUTC0,meanLFd,NPS &
        ,flight,imassvalid,flightairdist,FLLISTS
  USE mo_cocip_parameters, ONLY: LCVCC
  IMPLICIT NONE
  INTEGER,INTENT(IN):: NW,IPS,IDAY
  REAL,INTENT(OUT):: TOM
  INCLUDE "CPARA.inc"
  INCLUDE "psbeta3.inc"
  INTEGER I,IDT,idtchange
  LOGICAL lnewoem
  real RT,fexp,qlm,TAS
       RT=0.
!       print*,' NW',NW,'flight(1)%flight_id',flight(1)%flight_id
       I=1
       idt=flightadd(I)%ITW-FLLISTS%itwdep
       TAS=flightadd(I)%TASW
       RT=RT+idt*TAS    
       DO I=2,NW
       idt=flightadd(I)%ITW-flightadd(I-1)%ITW
       TAS=(flightadd(I-1)%TASW+flightadd(I)%TASW)*0.5
       RT=RT+idt*TAS    
!       print*,' I,flightadd(I)%ITW,idt,Tas,rt',I,flightadd(I)%ITW,idt,Tas,rt
       end do
          I=NW
       idt=FLLISTS%itwdes-flightadd(I)%ITW
       TAS=flightadd(NW-1)%TASW
       RT=RT+idt*TAS    
!       print*,'FLLISTS%flown_distance*1820,RT',FLLISTS%flown_distance*1820, RT
!       I=1
!       print*, &
!       'xdep',flight(I)%longitude,FLLISTS%xdep
!       print*, &
!       'ydep',flight(I)%latitude,FLLISTS%ydep
!       print*, &
!       'ITWdes',flightadd(I)%itw-iutc0,FLLISTS%itwdep-iutc0
!       I=NW
!       print*, &
!       'xdes',flight(I)%longitude,FLLISTS%xdes
!       print*, &
 !      'ydes',flight(I)%latitude,FLLISTS%ydes
!       print*, &
!       'ITWdes',flightadd(I)%itw-iutc0,FLLISTS%itwdes-iutc0
!       stop
       flightairdist=RT
                     lnewoem=.true.
      call STakeoffmass1(IPS,Rt,meanLFd(IDAY),TOM,lnewoem)
      
      
 ! new 9.10.:
 !      qlm=PMTOM(IPS)/PMLM(IPS)
 !      TOM=PMLM(IPS)*qlm*(meanLFd(IDAY)*PMZFM(IPS)+(1.-meanLFd(IDAY))*POEM(IPS))/fexp
 !  
       TOM=min(tom,0.97*PMTOM(IPS))
       TOM=max(tom,POEM(IPS))
 !       Print*,'M1_LOADF_wind I,idt,rt,TAS,fexp,tom',I,idt,rt,TAS,fexp,tom
         end subroutine  M1_LOADF_wind
         
           
      
  
   
       
       subroutine M4_climbdata(NW,IPS)
         USE mo_directories, ONLY: FLIGHT, flightadd,FLLISTS,IUTC0,meanLFd  &
         ,Jairline,clrmintyp,clrmaxtyp  &
         ,imassvalid,climbdataca,lpr,NPS,massresult6,clrout,nclrout,meanerrorac
  USE mo_cocip_parameters, ONLY: LCVCC
  IMPLICIT NONE
  INTEGER,INTENT(IN):: NW,IPS
  INCLUDE "CPARA.inc"
  INCLUDE "psbeta3.inc"
  INTEGER I,ISTART,Iend,idt,idtchange,ntkmean,J,NWV,I2,J2,itr1,itr2
  real flmin,flmax
  real clr,dfl,tkmean
!        Ifl1=IFLinitialcruise
   integer ITV(max(1,NW/2))
   INTEGER,PARAMETER:: IMETHOD=4
   clrout=0.
   nclrout=0
   
       imassvalid(IMETHOD)=0
    flmax=min(21000.,21000.*PMalthft(IPS)/300.)
    flmin=max(10000.,flmax*10./21.)-25.
! 1) DETERMINe ITV(NWV)
   NWV=0
   ifl2gt3: if(NW/4.gt.1) then
   doprep: do I=1,NW/2
   if (FLIGHTadd(I)%ioriginal.eq.1.and. FLIGHTadd(I)%invalid.eq.0) then
   NWV=NWV+1
   ITV(NWV)=I
   end if
   end do doprep
   
! 2) DETERMINe ISTART
     istart=0
    if(lpr)  print*,' Nwv',nwv
   nwvgt1: if(nwv.gt.3) then   
! search for istart   
  
    do0: do I=1,nwv
    J=ITV(I)  
   if(FLIGHT(J)%vertical_rate.gt.6..and.  & 
   FLIGHT(J)%altitude.gt.1000 .and. FLIGHT(J)%altitude.le.flmin) then
   istart=I
   exit
   end if
  end do do0
  
  ! 3) DETERMINe IEND
  Iend=0
  if(lpr)  print*,' istart',istart
  istartgt0: if(istart.gt.0) then
 ! search for end of climb
  
     do1: do I=istart+1,nwv
    J=ITV(I)  
   if(FLIGHT(J)%altitude.ge.flmax) then
   Iend=I
   exit
   end if
  end do do1
 if(lpr)   print*,' iend,istart',iend,istart
  iendgt0: if(iend.gt.istart) then
  ! compute mean temperture
     tkmean=0
     ntkmean=0
    do I=Istart,iend
       J=ITV(I)  
       tkmean=tkmean+ FLIGHT(J)%temperature
        ntkmean=ntkmean+1
    end do
    
    
  ! check for intermediate constanat level flights
   idtchange=0
   do I=Istart,iend
    J=ITV(I)  
    ratelt1: if(FLIGHT(J)%vertical_rate.lt.1.) then
     itr1=I
     itr2=I
 
     do I2=I+1,iend
     J2=ITV(I2)  
     if(FLIGHT(J2)%vertical_rate.gt.1.) then
     itr2=I2
     exit
     end if
    tkmean=tkmean-FLIGHT(J2)%temperature
    ntkmean=ntkmean-1
    end do
   idtchange=idtchange+ FLIGHTadd(itr2)%ITW-  FLIGHTadd(itr1)%ITW
   end if ratelt1
   end do
   
      idt=flightadd(Iend)%ITW-flightadd(Istart)%ITW-idtchange
     if(LPR)    print*,' idt,idtchange',idt,idtchange
 ! final computations  
    idtsmall: if(2*idtchange.lt.idt) then
       dfl=(flight(Iend)%altitude-flight(Istart)%altitude)
     If(LPR)    print*,' NTKmean,idt',NTKmean,idt
   ntkmeangt0: if(NTKmean.gt.5 .and. idt.gt.100) then
      tkmean=tkmean/NTKmean
  
      clr=dfl*0.3048/IDT
   
  
! preliminary solution
              clrmintyp(IPS)=5.
              clrmaxtyp(IPS)=15.
   
! R3:              FLIGHTadd(NW)%massfrom(IMETHOD)=POEM(IPS)  &
! R3:   +(pmtom(IPS)-POEM(IPS))*(clr-clrmintyp(IPS))/(clrmaxtyp(IPS)-clrmintyp(IPS))
    if(climbdataca(IPS,jairline)%x2.lt.0.) THEN
     FLIGHTadd(NW)%massfrom(IMETHOD)=climbdataca(IPS,jairline)%x1+clr*climbdataca(IPS,jairline)%x2
     FLIGHTadd(NW)%massfrom(IMETHOD)=FLIGHTadd(NW)%massfrom(IMETHOD)/(1.-meanerrorac(IMETHOD,IPS,Jairline))
    
     
      imassvalid(IMETHOD)=1
            clrout=clr
      nclrout=ntkmean
    else
    FLIGHTadd(NW)%massfrom(IMETHOD)=POEM(IPS)  &
   +(pmtom(IPS)-POEM(IPS))*(clr-clrmintyp(IPS))/(clrmaxtyp(IPS)-clrmintyp(IPS))
    FLIGHTadd(NW)%massfrom(IMETHOD)=FLIGHTadd(NW)%massfrom(IMETHOD)/(1.-meanerrorac(IMETHOD,IPS,Jairline))
     imassvalid(IMETHOD)=0
          clrout=0
      nclrout=0
    end if
               massresult6(IMETHOD)=FLIGHTadd(NW)%massfrom(IMETHOD)
      if(LPR)          print*,'clr*ntkmean',clr*ntkmean.gt.1000.,clr*ntkmean,clr,ntkmean
  
      
   
   
    
    FLIGHTadd(NW)%massfrom(IMETHOD)=min(flightadd(1)%massw(3),FLIGHTadd(NW)%massfrom(IMETHOD))
    FLIGHTadd(NW)%massfrom(IMETHOD)=max(flightadd(1)%massw(1),FLIGHTadd(NW)%massfrom(IMETHOD))
    
    end if ntkmeangt0
    end if idtsmall
    end if iendgt0
    end if istartgt0 
    end if nwvgt1
    end if ifl2gt3
    end subroutine  M4_climbdata
    