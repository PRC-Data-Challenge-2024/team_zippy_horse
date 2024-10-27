
!! Here comes the first Fortran program "R1.f90" that we use: “R1.f90” 
! togethe with an airport inforrmation  subroutine.


!This program reads the contest and the final submission data sets in csv format from  a disk “F:\EU_parquet”. 
!Both for the flight lists (competition and submission parts, FLLISTC and FLLISTS, see type specification)
!and the FLIGHT trajectories.

!It combines these data sets, 
!eliminates flight waypoints before departure and after destination,
!completes by the airport information,
!Identifies the airline ,
!and puts these data our together in unformatted (quickly readable) files on “F:\EU_FLLISTC” and “F:\EU_FLLISTS”.
!Here C stands for the competition data and S for the submission data.

!coded by ulrich,schumann@dlr.de, latest change 24 October 2024

! This code is compiled and excecuted by

!  gfortran -I/usr/include -cpp -O3 -mcmodel=large   -fbacktrace   -march=native    -Wline-truncation  -ffixed-line-length-none &
!    -fbounds-check -Wuninitialized  R1_copyflight.f90 airport.f  -fdollar-ok   -o R1_copyflight    &&  ./R1_copyflight.exe



MODULE mo_types
  type TFLIGHT ! 
    INTEGER No,flight_id
     character*10 timestamp
     character*14 timestamp2
     real latitude,longitude,altitude,groundspeed,track,vertical_rate 
     INTEGER icao24
     real u_component_of_wind,v_component_of_wind &
     ,temperature,specific_humidity 
  end type  TFLIGHT   
  
  
  type TFLIGHTadd ! 
    INTEGER ITW,ioriginal
    REAL segmW,cosaw,sinaw,TASW,GSDISTW,dgsdtW,Machw,CLRW
  end type  TFLIGHTadd   
  
  type TFLIGHTaddreduced! 
    INTEGER ITW,ioriginal
   end type  TFLIGHTaddreduced   
  
  
  
    type TFLIGHTLISTc ! 
!flight_id,cdate,callsign,adep,name_adep,country_code_adep
!,ades,name_ades,country_code_ades
!,actual_offblock_time,arrival_time,aircraft_type,wtc
!,airline,flight_duration,taxiout_time,flown_distance,tow

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
     REAL xdep,ydep,xdes,ydes
     INTEGER itwdep,itwdes,IFLinitialcruise,IFLmax
  
 
     
   
  end type  TFLIGHTLISTc  
  
      type TFLIGHTLISTs ! 
     INTEGER flight_id
     CHARACTER*20 cdatex
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
     character*1 wtc
     character*32 airline
     Integer flight_duration,taxiout_time,flown_distance  
     REAL xdep,ydep,xdes,ydes
     INTEGER itwdep,itwdes
  
  
  end type  TFLIGHTLISTs  
     
END MODULE mo_types

MODULE mo_directories ! many code details
  USE mo_types,only: TFLIGHT,TFLIGHTLISTc,TFLIGHTLISTs,TFLIGHTadd
  IMPLICIT NONE
      Type(TFLIGHT),DIMENSION(:),ALLOCATABLE      :: FLIGHT ! data
     Type(TFLIGHT) :: FLIGHT1line ! data
     TYPE(TFLIGHTLISTc),DIMENSION(:),ALLOCATABLE :: FLLISTC 
     TYPE(TFLIGHTLISTs),DIMENSION(:),ALLOCATABLE :: FLLISTS 
     TYPE(TFLIGHTadd),DIMENSION(:),ALLOCATABLE :: FLIGHTadd
     INTEGER iline,IUTC0,istopall,nwmax,NPLOT
     REAL speedmax
     INTEGER,PARAMETER ::idtmax0=1
     INTEGER, PARAMETER:: NWX=100000
     INTEGER NHIST(NWX)
     INTEGER, PARAMETER:: IDT0=1
     
            integer,parameter:: nairlinex=100
         INTEGER Nairline,Nflairline(nairlinex)
         character*32 cairline(nairlinex)
            integer,parameter:: ncallsignx=200000
         INTEGER Ncallsign,Nflcallsign(ncallsignx)
         character*32 ccallsign(ncallsignx)
         INTEGER,PARAMETER:: NPS=68
         
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
  REAL,PARAMETER:: cpe_CP=1.2378
  REAL,PARAMETER:: cpe=cpe_CP*cp
  REAL,PARAMETER:: cpkero=2.*cp
  REAL,PARAMETER:: etacombustion=0.99
 
  
 
end module mo_cocip_parameters


PROGRAM main

  USE mo_directories, ONLY: FLIGHT,FLIGHT1line,FLLISTC,FLLISTS,FLIGHTadd,iline &
  ,IUTC0,NWX,istopall,nwmax,NHIST,NPLOT,speedmax &
  ,Nairline,cairline,Nflairline &
  ,Ncallsign,ccallsign,Nflcallsign,NPS
  USE mo_cocip_parameters
  IMPLICIT NONE
  INTEGER,PARAMETER:: NFCX=369014,NFSX=158150  ! both have a reserve of just one
  INTEGER istat,ILAST,NFL,NW,idold,NFs,I,IC,NFC,IS
  INTEGER*8 NFC8,NWTOT  
  real,parameter::TOWZERO=-999.
  INTEGER NFOUNDC,NFOUNDS,NOTFOUND &
  ,JDAYJ0,IDAY,NFOUND2days,doubleFOUND!,icold
  INTEGER IDAY1,IDAY2
  real time0,time1,times,timec,timetot
  real timess,times1,times0
  CHARACTER*10 cdate,cdates,cdatesdep
  INTEGER IY,IMONTH,IDAYM,Nout,nwr8,nwr9,Iday31122021,J,NIU &
  ,yyyy,mm,dd,hh,minmin,sec,Jairline,Jcallsign
   LOGICAL,PARAMETER:: LNEW=.true.
  LOGICAL,PARAMETER:: lwrite18=.false.
 LOGICAL,PARAMETER:: lwrite8=.true.
 
!!!  call testdist
        INTEGER nfoundday(365)
     
   
       nfoundday=0
       NIU=0
!!        print*,' here stop to avoid data overwriting'
!!        stop
       CALL CPU_TIME(time0)
       JDAYJ0=0
       call    DAYJ(2021,12,31,JDAYJ0)
       IUTC0=(JDAYJ0-1)*24*3600 ! preliminary values
 !      print*,' JDAYJ0',JDAYJ0
       istopall=0
       speedmax=0.
       nwmax=0
       NHIST=0
       NPLOT=0
       call AIRPORTINIT
       Print*,' AIRPORTINIT done'
 
       ALLOCATE(FLLISTC(NFCX),stat=istat)
    Print*,' ALLOCATE: FLLISTC(NFCX),stat=',istat 
       OPen(5,file='EU/challenge_setb.csv',action='read')
       CALL gtchallengesetC(NFCX,FLLISTC,NFC)
       PRINT*,'NFC,NFCX',NFC,NFCX
       print*,'fllistc(1)',fllistc(1)
      I=1
      print*,'I',I,'fllistc(I)',fllistc(I)
   
      Print*

       I=NFc
      print*,'I',I,'fllistc(I)',fllistc(I)
    

      close(5)
      CALL CPU_TIME(time1)
      timec=(time1-time0)
      print*,' timec',timec
 Print*,'#################################################'
        CALL CPU_TIME(time0)
       NFC8=NFC
       PRINT*,'NFC',NFC8
      Print*,'FLLISTC(NFC)%flight_id',FLLISTC(NFC)%flight_id
      print*,'NFC*FLLISTC(NFC)%flight_id',NFC8*FLLISTC(NFC)%flight_id
      
      
      
      NFOUND2days=0
      
      
          IDAY1=1
          IDAY2=2!5
           call  DAYJ(2022,9,17,IDAY1)
            call  DAYJ(2022,9,19,IDAY2)
            call  DAYJ(2021,12,31,Iday31122021)
            iday1=Iday1-Iday31122021
            iday2=Iday2-Iday31122021
            print*,'Iday1,iday2',Iday1,iday2
            j=0
   
            
 
  ALLOCATE(FLLISTS(NFSX),stat=istat)
    Print*,' ALLOCATE: FLLISTS(NFSX),stat=',istat 
 
      OPen(5,file='EU/final_submission_setb.csv',action='read')
      
      CALL gtchallengesets(NFSX,FLLISTS,NFs)
      PRINT*,'NFS',nfs
      print*,'fllists(1)',fllists(1)
      print*,'fllists(NFS)',fllists(NFS)
      close(5)
      
            CALL CPU_TIME(time1)
      times=(time1-time0)
      print*,' times',times
 Print*,'#################################################'
     
    
  ALLOCATE(FLIGHT(NWX),stat=istat)
    Print*,' ALLOCATE: FLIGHT(NWX),stat=',istat 
  ALLOCATE(FLIGHTadd(NWX),stat=istat)
    Print*,' ALLOCATE: FLIGHTadd(NWX),stat=',istat 
    
  
       
!######################################################################       
  
  NFOUNDC=0
  NFOUNDS=0
  NOTFOUND=0
  NFOUND2days=0
  doublefound=0
             CALL CPU_TIME(time0)
  
       
       
           open(20,file='depdestimes.txt')
          IDAY1=1
          IDAY2=365
!           call  DAYJ(2022,9,17,IDAY1)
!            call  DAYJ(2022,9,19,IDAY2)
           call  DAYJ(2022,1,1,IDAY1)
            call  DAYJ(2022,12,31,IDAY2)
            call  DAYJ(2021,12,31,Iday31122021)
            iday1=Iday1-Iday31122021
            iday2=Iday2-Iday31122021
            print*,'Iday1,iday2',Iday1,iday2
            nwr8=0
         nwr9=0
    
      Nairline=0
      Ncallsign=0
      IY=0
      IMONTH=0
      IDAYM=0
 ldodays: do iday=iday1,iday2
 
 
       call monday(JDAYJ0+iday-1,IY,IMONTH,IDAYM)
       write(cdatesdep,'(I4,A1,I2.2,A1,I2.2)')IY,'-',IMONTH,'-',IDAYM
       print*,' cdatedep',cdatesdep   
       
       IUTC0=(JDAYJ0+iday-1)*24*3600 ! correct value
       write(cdates,'(I4,A1,I2.2,A1,I2.2)')IY,'-',IMONTH,'-',IDAYM
       print*,' cdate',cdates   
!     cdate='2022-09-18'
        if(lwrite18) then
        open(18,file='EU/FLCTEST'//cdates//'.txt')
              write(18,*) 'No ','flight_id ' &
      ,'timestamp ' &
      ,'latitude ','longitude ' &
      ,'altitude ','groundspeed ' &
      ,'track ','vertical_rate ' & 
      ,'u_component_of_wind ','v_component_of_wind ' &
      ,'temperature ','specific_humidity ' &
      ,'ITW ','ioriginal ' &
    , 'segmW ','cosaw ','sinaw ','TASW ','GSDISTW ','dgsdtW ','Machw ','CLRW '
     end if
    if(lwrite8) then
        open(8,file='F:\FLLISTC\'//cdates,form='unformatted')
        open(9,file='F:\FLLISTS\'//cdates,form='unformatted')
     end if

  

   
       call monday(JDAYJ0+iday,IY,IMONTH,IDAYM)

       write(cdate,'(I4,A1,I2.2,A1,I2.2)')IY,'-',IMONTH,'-',IDAYM
       print*,' cdate',cdate
  !     cdate='2022-09-18'
!  OPEN(5,file='E:\EU/'//cdate//'.csv',action='read')
  OPen(5,file='F:\EU_parquet/'//cdate//'.csv',action='read')
  read(5,*)
  Print*,' file(5)=','EU/'//cdate//'.csv'
  FLIGHT1line%flight_id=-999
  idold=-999
  ILAST=0
  NFL=0
  NWTOT=0
  NW=0
       
  timess=0.
  iline=0
  Nout=0
   jcallsign=1
  dowhileilast: do WHILE (ILAST.eq.0 )! .and. NIU.le.0) 
   
   call READFP(NWX,ILAST,NW,NFL+1)

   Print*,' readfp: ILAST,NW,flight(1)%flight_id', ILAST,NW,flight(1)%flight_id
!   if(flight(1)%flight_id.eq.248764524) then 
    I=NW ! needed for printout for NW.le.1 at end of nwgt1
   
  
    nwgt1: if(NW.gt.1) then   
       
   ic=0
    CALL CPU_TIME(times0)
! searchic: searches for IC for which (FLLISTC(I)%flight_id.eq.FLIGHT(1)%flight_id)
   
    if(LNEW) then
     call searchic(flight(1)%flight_id,NFC,IC) ! searches of Index IC in fllistc using a log search
     else
   Call searchicold(flight(1)%flight_id,NFC,ic)  ! searches of Index IC in fllistc using a linear search
    end if  
     print*,'FLIGHT(1)%flight_id, IC',FLIGHT(1)%flight_id, IC
       CALL CPU_TIME(timeS1)
       
       
    timess=timess+(times1-times0)
    
    
   LICgt0: if(IC.gt.0) then
    print*,' -> IC',ic
   NFOUNDC=NFOUNDC+1  
   call SairlineC(IC,jairline) ! lists airlines
!   call ScallsignC(IC,jcallsign) ! lists callsigns, nit used here
    
    
   Print*,' FLLISTC(IC)%itwdes -IUTC0',FLLISTC(IC)%itwdes -IUTC0
   Print*,' FLLISTC(IC)%itwdep -IUTC0',FLLISTC(IC)%itwdep -IUTC0
  
 
   
     write(20,*) 'iday,IC=',iday,IC,' id=',flight(1)%flight_id
  
    
 

     NWGT0IC: if(NW.gt.1) then    
      call sFLIGHTadd(NW) ! adds ITW and IORIGIN to flightadd
      NFL=NFL+1
      
          call eliminatezerodistc(NW,IC) !  eliminates points that have zero distance to previous one or zero groundspeed
          ! also eliminates waypoints at times before departure or after arrival at destination,
          ! assuming that the given times (ITWdep,ITWdes) are accurate up to 1 minute.
     print*,' after eliminatezerodist, NW', NW
     NWGT0IC2: if(NW.gt.1) then    
     
      NWTOT=NWTOT+NW
      
! now write out flight-description (FLLISTC(IC)) together with flight trajectory (FLIGHT(1:NW)) 
    lwr8: if (lwrite8) then
      write(8)FLLISTC(IC),NW,IC,Jairline!,Jcallsign
      if(lwrite18)write(18,*)FLLISTC(IC),NW,IC
   
       dowr8und18: do I=1,NW       
       write(8) FLIGHT(I) 
       if(lwrite18)write(18,*)flight(I),flightadd(I),flightadd(I)%ITW-IUTC0
       end do  dowr8und18
        end if lwr8

      nwr8=nwr8+1      
   
       nfoundday(iday)=nfoundday(iday)+1
      if(lwrite18)then
      Print*,'write(18) done,nwr8',nwr8
      print*,'file=','EU/FLCTEST'//cdates//'.txt'
      Print*,'stop after frist write 18'
      stop
      endif
      
      end if NWGT0IC2
     end if NWGT0IC
 
   
 
       end if LICgt0
       
! search also in the final submission list
    Is=0
    CALL CPU_TIME(timeS0)
    if(LNEW) then
     call searchis(FLIGHT(1)%flight_id,NFs,Is)! -> Is
     else
   Call searchisold(FLIGHT(1)%flight_id,NFs,Is)
   end if
   
  print*,'after searchis,  FLIGHT(1)%flight_id,NFs,Is=', FLIGHT(1)%flight_id,NFs,Is
       
    CALL CPU_TIME(timeS1)
    timess=timess+(times1-times0)
!
       lisgt0: if(Is.gt.0) then
       print*,' -> IS ',iS
      NFOUNDs=NFOUNDs+1
      call SairlineS(IS,Jairline)
!      call ScallsignS(IS,Jcallsign)
    
     Print*,' FLLISTS(IS)%itwdep -IUTC0',FLLISTS(IS)%itwdep -IUTC0,' IS=', IS
     Print*,' FLLISTS(IS)%itwdes -IUTC0',FLLISTS(IS)%itwdes -IUTC0
 
      
 
    NWGT1S: if(NW.gt.1) then    
    call sFLIGHTadd(NW) ! adds ITW and IORIGIN
    
    
    call eliminatezerodists(NW,iS) 
    print*,' after eliminatezerodist, NW', NW
    NWGT1S2: if(NW.gt.1) then    
      NFL=NFL+1
      NWTOT=NWTOT+NW
          
          lwrs: if (lwrite8) then
    write(9)FLLISTS(IS),NW,IS,Jairline!,Jcallsign
    do I=1,NW
    write(9) FLIGHT(I)
    end do
   end if lwrs
  
   nwr9=nwr9+1
  
   nfoundday(iday)=nfoundday(iday)+1
   else
   print*,' reduced NW fromeliminatezerodist, Nw, nwr9',NW, nwr9
   end if NWGT1S2
   else NWGT1S
   print*,' too small NW , Nw, nwr9',NW, nwr9
!   stop
   end if NWGT1S
   end if  lisgt0  
 
  is0IS0: if(Ic.eq.0 .and. IS .eq.0) then
  
   NOTFOUND=NOTFOUND+1
    print*,' neither IS nor IC, NOTFOUND=',NOTFOUND
 
      end if is0IS0
      
       is0IS11: if(Ic.eq.1 .and. IS .eq.1) then
  
   doubleFOUND=doubleFOUND+1
    print*,'  IS both in IC and Is ',doubleFOUND
 
      end if is0IS11
  

   
    Print* &
    ,' idays,NFOUNDC,NFOUNDS,NOTFOUND,NFOUNDC+NFOUNDS,doubleFOUND' &
    ,iday,NFOUNDC,NFOUNDS,NOTFOUND,NFOUNDC+NFOUNDS,doubleFOUND
    print*,' NWR8,NWR9,',NWR8,NWR9
 !   if(NFOUNDC.gt.10) stop
!    if(NFOUND2days.ge.3) stop
 
   else nwgt1
      print*,'  eliminatezerodist reduced NW from ', I,' to ', NW
 !     stop
   end if nwgt1
!    end if
    
   end do dowhileilast
   close(5)
   
   Print*,' all read done, NFL,NWTOT,IC=', NFL,NWTOT,IC
   open(7,file='FLLISTC',form='unformatted')
   do I=1,NFC
   write(7)FLLISTC(I)
   end do   
 
   close(7)
   Print*,' FLLISTC.txt written, NFC=',nfc
   open(7,file='FLLISTS',form='unformatted')
   do I=1,NFS
   write(7)FLLISTS(I)
   end do 
  
   close(7)
   Print*,' FLLISTS.txt written, NFS=',NFS,' for IDAY, idays',IDAY

   if (lwrite8) then
   close(8)
   close(9)
   end if
   
      Print*,' FLLISTC.txt written, NFC=',nfc
 
 
   Print*,' end of iday',IDAY
!   stop
   
  end do ldodays
!   close(10)
!   close(11)
   
!######################################################################
   
       deallocate(FLLISTS)
   deallocate(FLLISTC)
   deallocate(FLIGHT)
   
 
     Print* &
    ,' NFOUNDC,NFOUNDS,NOTFOUND,NFOUNDC+NFOUNDS,doubleFOUND' &
    ,NFOUNDC,NFOUNDS,NOTFOUND,NFOUNDC+NFOUNDS,doubleFOUND
   
             CALL CPU_TIME(time1)
      timetot=(time1-time0)
      print*,' times',times
     print*,' timec',timec
     print*,' all DAY LOOPS timetot',timetot
     Print*,' timess', timess
     PRINT*,' LNEW', LNEW
     
     OPen(14,file='NHIST.txt')
     do I=1,NWX
     write(14,'(2I12)') I,nHIST(I)
     end do
     close(14)

 
          if(lwrite18) close(18)
          
          
        
          do IDAY=1,365
          if(nfoundday(iday).gt.0) then
          print*,' iday,nfoundday(iday)',iday,nfoundday(iday)
          end if
         
          end do
          print*,' nfoundday printed'
   Print*,'################################################# END of Pogram Main'
          call Sairlineprint
 !         call Scallsignprint

   end PROGRAM  main
  
  
  SUBROUTINE searchicold(flight_id,NFC,IC)
   USE mo_directories, ONLY: FLLISTC
   INTEGER,INTENT(IN)::flight_id,NFC
   INTEGER,INTENT(INOUT)::IC
   INTEGER I,IFOUND
   IFOUND=0
   IC=max(1,IC)
   IC=MIN(IC,NFC)
   do I=ic,NFC
   if(FLLISTC(I)%flight_id.eq.flight_id) then
   IC=I
   IFOUND=1
   exit
   end if
   end do
   if(IFOUND.eq.0) then
      do I=1,ic-1
   if(FLLISTC(I)%flight_id.eq.flight_id) then
   IC=I
   IFOUND=1
   exit
   end if
   end do
   end if
   if(IFOUND.eq.0) then
!   print*,'searchicolf failed for flight_id,NFC=',flight_id, NFC
   IC=0
   end if
   end SUBROUTINE searchicold
   
   
    
  
  SUBROUTINE searchisold(flight_id,NFS,IC)
   USE mo_directories, ONLY: FLLISTS
   INTEGER,INTENT(IN)::flight_id,NFS
   INTEGER,INTENT(INOUT)::IC
   INTEGER I,IFOUND
   IFOUND=0
   do I=ic,NFS
   if(FLLISTS(I)%flight_id.eq.flight_id) then
   IC=I
   IFOUND=1
   exit
   end if
   end do
   if(IFOUND.eq.0) then
      do I=1,ic-1
   if(FLLISTS(I)%flight_id.eq.flight_id) then
   IC=I
   IFOUND=1
   exit
   end if
   end do
   end if
   if(IFOUND.eq.0) then
!   print*,'searchisold failed for flight_id,NFS=',flight_id, NFS
   IC=0
   end if
   end SUBROUTINE searchisold
   
     SUBROUTINE searchic(flight_id1,NFC,IC)
   USE mo_directories, ONLY: FLLISTC
   IMPLICIT NONE
   INTEGER,INTENT(IN)::flight_id1,NFC
   INTEGER,INTENT(OUT)::IC
   INTEGER NSTEP
   IC=(NFC+1)/2
   NSTEP=IC-1!(NFC+1)/2
   do while(nstep.gt.0) 
   if(flight_id1.eq.FLLISTC(IC)%flight_id) exit
   IF(flight_id1.lt.FLLISTC(IC)%flight_id) then   
   IC=max(1,IC-NSTEP)
   else
   IC=min(NFC,IC+NSTEP)
   end if
   if(nstep.eq.1)exit
   NSTEP=(NSTeP+1)/2
   end do
   
    if(flight_id1.ne.FLLISTC(IC)%flight_id) IC=0

   end SUBROUTINE searchic
   
   SUBROUTINE searchis(flight_id1,NFC,IC)
   USE mo_directories, ONLY: FLLISTs
   IMPLICIT NONE
   INTEGER,INTENT(IN)::flight_id1,NFC
   INTEGER,INTENT(OUT)::IC
   INTEGER NSTEP
!   LOGICAL,PARAMETER:: LPR=.false.,ltest=.false.  

 
   IC=(NFC+1)/2
   NSTEP=IC-1!(NFC+1)/2
 
   do while(nstep.gt.0) 
!   print*,'searchis:  IC,flight_id1,FLLISTS(IC)%flight_id' &
!   ,IC,flight_id1,FLLISTS(IC)%flight_id,' NSTEP',NSTEP
   if(flight_id1.eq.FLLISTS(IC)%flight_id) exit
   IF(flight_id1.lt.FLLISTS(IC)%flight_id) then   
 
   IC=Max(1,IC-NSTEP)
   else
      IC=MIN(IC+NSTEP,NFC)
   end if
   if(nstep.eq.1)exit
    NSTEP=(NSTEP+1)/2
 
   end do
   
!   print*,'searchis:  IC,flight_id1,FLLISTS(IC)%flight_id' &
!   ,IC,flight_id1,FLLISTS(IC)%flight_id
    if(flight_id1.ne.FLLISTS(IC)%flight_id) IC=0
!    Print*,'end of searchis: IC', IC
   end SUBROUTINE searchis
   
         
  SUBROUTINE READFP(NWX,ILAST,NW,nflout)
    USE mo_directories, ONLY: FLIGHT,FLIGHT1line
    IMPLICIT NONE
    INTEGER,INTENT(IN):: NWX,nflout
    INTEGER, INTENT(INOUT):: ILAST
    INTEGER, INTENT(OUT):: NW
    INTEGER I,istat
! reads trajectory data FLIGHT with maxm o fNWX data points (actual NW)
!  for given flight number nflout
! ILAST= 0 if no file-end , nflout=1 if file-end reached

!The daily trajectory files contain:
! https://ansperformance.eu/study/data-challenge/data.html

!flight identification: 
!unique ID (flight_id, same as for flight list),
! (obfuscated) ICAO 24-bit address (icao24, same value as flight_id)
!4D position: longitude [DD, decimal degrees in -90/90 range] and latitude [DD, decimal degrees in -180/180 range], altitude [ft], 
!timestamp [timestamp with time zone]
!speed: ground speed (groundspeed [kt]), 
!track angle (track  [decimal degrees]), 
!vertical rate of climb/descent (vertical_rate [ft/min])
!(optionally) meteorological info at 4D position:
!wind (u_component_of_wind and v_component_of_wind [m/s])
!temperature , kelvin]
    real CLR

    istat=-999
    Ilast=0
    if(FLIGHT1line%flight_id.gt.0) then
    FLIGHT(1)=FLIGHT1line
    NW=1
    else
    NW=0
    end if
    doloop: do I=NW+1,NWX
!    No,flight_id
!     character*12 timestamp
!     character*16 timestamp2
!     real latitude,longitude,altitude,groundspeed,track,vertical_rate &
!     ,u_component_of_wind,v_component_of_wind &
!     ,temperature,specific_humidity 
    FLIGHT(I)%temperature=-999.
  1 continue
    read(5,*,iostat=istat) FLIGHT(I)
     readok: if(istat.eq.0) then
     if(FLIGHT(I)%timestamp(1:4).ne.'2022') goto 1
     if(FLIGHT(I)%timestamp2(3:3).ne.':') goto 1
     if(FLIGHT(I)%temperature.lt.0.) goto 1
     CLR= FLIGHT(I)%vertical_rate*0.3048/60.
     if(abs(CLR).gt.50.) goto 1
 !   if(FLIGHT(I)%FLIGHT_id.ne. 255455413) goto 1   
    FLIGHT(I)%groundspeed=  FLIGHT(I)%groundspeed*0.5144  ! Knots to m/s
    FLIGHT(I)%vertical_rate= CLR! feet/min to m/s 
    NW=I
    ifiold: if(FLIGHT1line%flight_id.gt.0) then
     if(FLIGHT(I)%flight_id.ne.FLIGHT1line%flight_id ) then
      FLIGHT1line=FLIGHT(I)
      NW=NW-1
       exit
      end if
    else ifiold
    FLIGHT1line%flight_id =FLIGHT(I)%flight_id
!    Print*,' FLIGHT1line%flight_id first',FLIGHT1line%flight_id 
     end if ifiold
    else readok
!        Print*,'istat', istat
    Ilast=1
!    Print*,' exit2'
    exit
    end IF readok
     end do   doloop
 ! test
 !    nwgt0: if(NW.gt.0) then
 !    do I=1,NW
 !    write(44,*) FLIGHT(I)
 !    end do
 !    write(44,*)
 !    I=1+NW/2
 !    Print*,' output done inREADFP, NW=', NW, 'tiemstamp=',flight(I)%timestamp &
 !    ,' id=',FLIGHT(I)%flight_id 
 !    end if nwgt0
     
   
    end SUBROUTINE  READFP 
    
  
    
 
       subroutine gtchallengesetc(NFLX,FLLISTC,NFC)
! reads and completes the challenge date set
! completes airport coordinates and times
 ! and finally sorts the flight list to monotonically increasing  flight_id
      USE mo_types,only: TFLIGHTLISTc
    IMPLICIT NONE
    INTEGER, INTENT(IN):: NFLX
    INTEGER,INTENT(OUT)::NFC
    TYPE(TFLIGHTLISTc),intent(OUT),dimension(NFLX):: FLLISTC
    INTEGER I
    INTEGER yyyy,mm,dd,hh,minmin,sec,ddold,IDAYY
    INTEGER,PARAMETER:: is=5
    ddold=0
   
 
       read(5,*)
       NFC=0
       do I=1,NFLX
    
       read(5,*,END=999) FLLISTC(I)%flight_id &
      ,FLLISTC(I)%cdatex &
      ,FLLISTC(I)%callsign &
      ,FLLISTC(I)%adep &
      ,FLLISTC(I)%name_adep &
      ,FLLISTC(I)%country_code_adep &
      ,FLLISTC(I)%ades &
      ,FLLISTC(I)%name_ades &
      ,FLLISTC(I)%country_code_ades &
     ,FLLISTC(I)%actual_offblock_time &
     ,FLLISTC(I)%arrival_time &
     ,FLLISTC(I)%aircraft_type &
     ,FLLISTC(I)%wtc &
     ,FLLISTC(I)%airline  &
     ,FLLISTC(I)%flight_duration &
     ,FLLISTC(I)%taxiout_time &
     ,FLLISTC(I)%flown_distance &
      ,FLLISTC(I)%tow    
!      if(FLLISTC(I)%adep.eq.'EDDF'.and.FLLISTC(I)%ades.eq.'KJFK') then
!      print*,' FLLISTC(I)%flight_id ',FLLISTC(I)%flight_id !! 250971266
!      stop
!      end if

    
       NFC=I
       end do
       print*,'gtchallengesetc: too short'
       stop
  999 continue
       Print*,' setc: NFC,NFLX',NFC,NFLX
       do I=1,NFC
!       call findairportdepdes(adep(I),ades(I),xdep(I) &
!      ,ydep(I),xdes(I),ydes(I))
        call findairportdepdes(FLLISTC(I)%adep,FLLISTC(I)%ades,FLLISTC(I)%xdep &
      ,FLLISTC(I)%ydep,FLLISTC(I)%xdes,FLLISTC(I)%ydes)
       FLLISTC(I)%IFLinitialcruise=0
       FLLISTC(I)%IFLmax=0
     read(FLLISTC(I)%actual_offblock_time,'(I4,1x,5(I2,1x))') & 
     yyyy,mm,dd,hh,minmin,sec
     If(dd.ne.ddold) call DAYJ(yyyy,mm,dd,IDAYY)
     ddold=dd
      FLLISTC(I)%itwdep=(IDAYY-1) *24*3600+hh*3600+minmin*60+sec &
      +FLLISTC(I)%taxiout_time*60  !time in minutes
      
     read(FLLISTC(I)%arrival_time,'(I4,1x,5(I2,1x))') & 
     yyyy,mm,dd,hh,minmin,sec
     If(dd.ne.ddold) call DAYJ(yyyy,mm,dd,IDAYY)
     ddold=dd
     
     

      FLLISTC(I)%itwdes=(IDAYY-1) *24*3600+hh*3600+minmin*60+sec 
 !      FLLISTC(I)%itwdes= FLLISTC(I)%itwdep+FLLISTC(I)%flight_duration*60 !!! ???
 !    Print*,'FLLISTC(I)%flight_duration in min or s?',FLLISTC(I)%flight_duration*60
 !    Print*,'FLLISTC(I)%itwdes-FLLISTC(I)%itwdep?',FLLISTC(I)%itwdes-FLLISTC(I)%itwdep
      
 !   or tests:   if(I.gt.100 ) stop
!       print* &
!       ,' FLLISTC(I)%xdep,FLLISTC(I)%ydep,FLLISTC(I)%xdes,FLLISTC(I)%ydes' &
!       ,FLLISTC(I)%xdep,FLLISTC(I)%ydep,FLLISTC(I)%xdes,FLLISTC(I)%ydes
       end do
 !      print*, 'NFC',NFC
!       print*,' findairportdepdes done'
       CALL FLLISTssortC(NFC,FLLISTC)
       end subroutine  gtchallengesetc
       
 
       subroutine gtchallengesets(NFLX,FLLISTS,NFS)
 ! reads and completes the final submission  date set
 ! completes airport coordinates and times
 ! and finally sorts the flight list to monotonically increasing  flight_id
      USE mo_types,only: TFLIGHTLISTs
!      USE mo_directories, ONLY: IUTC0
    IMPLICIT NONE
    INTEGER, INTENT(IN):: NFLX
    INTEGER,INTENT(OUT)::NFS
    TYPE(TFLIGHTLISTs),intent(OUT),dimension(NFLX):: FLLISTS
    INTEGER I
    INTEGER yyyy,mm,dd,hh,minmin,sec,ddold,IDAYY
    INTEGER,PARAMETER:: is=6
    ddold=0
   
   
   
 
       read(5,*)
       NFS=0
       do I=1,NFLX
   
       read(5,*,END=999) FLLISTS(I)%flight_id &
      ,FLLISTS(I)%cdatex &
      ,FLLISTS(I)%callsign &
      ,FLLISTS(I)%adep &
      ,FLLISTS(I)%name_adep &
      ,FLLISTS(I)%country_code_adep &
      ,FLLISTS(I)%ades &
      ,FLLISTS(I)%name_ades &
      ,FLLISTS(I)%country_code_ades &
     ,FLLISTS(I)%actual_offblock_time &
     ,FLLISTS(I)%arrival_time &
     ,FLLISTS(I)%aircraft_type &
     ,FLLISTS(I)%wtc &
     ,FLLISTS(I)%airline  &
     ,FLLISTS(I)%flight_duration &
     ,FLLISTS(I)%taxiout_time &
     ,FLLISTS(I)%flown_distance  ! in nmiles
 !     ,FLLISTS(I)%tow    
     
       NFS=I
       end do
       print*,'gtchallengesets: too short, nfs,NFLX ',nfs,NFLX 
       stop
  999 continue
       Print*,'sets:  NFS,NFLX',NFS,NFLX
        do I=1,NFS
       call findairportdepdes(FLLISTS(I)%adep,FLLISTS(I)%ades,FLLISTS(I)%xdep &
      ,FLLISTS(I)%ydep,FLLISTS(I)%xdes,FLLISTS(I)%ydes)
       end do
!       print*,' findairportdepdes done'
       call FLLISTCsortS(NFS,FLLISTS)
       
             do I=1,NFS
      read(FLLISTS(I)%actual_offblock_time,'(I4,1x,5(I2,1x))') & 
     yyyy,mm,dd,hh,minmin,sec
     
!     print*,' I,yyyy',I,yyyy,FLLISTS(I)%actual_offblock_time
!     print*,' I,FLLISTS(I)',FLLISTS(I)
     if(yyyy.ne. 2022)THEN
     PRINT*,' yyyy',yyyy
      print*,' I,FLLISTS(I)',FLLISTS(I)
     stop
     END IF
     
     
     If(dd.ne.ddold) call DAYJ(yyyy,mm,dd,IDAYY)
     ddold=dd
      FLLISTS(I)%itwdep=(IDAYY-1) *24*3600+hh*3600+minmin*60+sec &
      +FLLISTS(I)%taxiout_time*60  !time in minutes
      
     read(FLLISTS(I)%arrival_time,'(I4,1x,5(I2,1x))') & 
     yyyy,mm,dd,hh,minmin,sec

     If(dd.ne.ddold) call DAYJ(yyyy,mm,dd,IDAYY)
     ddold=dd
    ddold=dd
  
     
     

      FLLISTS(I)%itwdes=(IDAYY-1) *24*3600+hh*3600+minmin*60+sec 
 !      FLLISTS(I)%itwdes= FLLISTS(I)%itwdep+FLLISTS(I)%flight_duration*60 !!! ???
!       print*,' I,yyyy,mm,dd,hh,minmin,sec,itwdep,IUTC0' &
!         , I,yyyy,mm,dd,hh,minmin,sec,FLLISTS(I)%itwdes-IUTC0,IUTC0
                      
      
       end do
      
       end subroutine  gtchallengesets
       
    subroutine FLLISTCsortS(NFS,FLLISTC)
    USE mo_types,only: TFLIGHTLISTs
    IMPLICIT NONE
    INTEGER, INTENT(IN):: NFS
    TYPE(TFLIGHTLISTs),INTENT(INOUT)::FLLISTC(NFS)
    TYPE(TFLIGHTLISTs)::FLLISTCopx(NFS)
     INteger,DIMENSION(NFS)::arr,brr
     INTEGER I,J
 
    FLLISTCopx(1:NFS)=FLLISTC(1:NFS)   
    arr(1:NFS)=FLLISTC(1:NFS)%flight_id
!    do I=1,NFS
!    if( 258074338 .eq. FLLISTC(I)%flight_id) print*,'FLLISTC(I)=',I,FLLISTC(I)%flight_id,' ',FLLISTC(I)%adep,' ',FLLISTC(I)%ades,I
!! FLLISTC(I)=      105959   258074338  EBBR KJFK  
!   end do
!    Print*
!    stop
    
    do I=1,NFS
    Brr(I)=I
    end do
    call sort2(NFS,arr,brr)
    do I=1,NFS
    J=BRR(I)
    FLLISTC(I)=FLLISTCopx(J)
    end do
!    do I=2,NFS
!    print*,'FLLISTC(I)=',I,FLLISTC(I)%flight_id,' ',FLLISTC(I)%adep,' ',FLLISTC(I)%ades
!    if(FLLISTC(I)%flight_id.le.FLLISTC(I-1)%flight_id) THEN
!    print*,' error in sort'
!    stop
!    end if
!    end do
    end subroutine  FLLISTCsortS
    
    subroutine FLLISTssortC(NFC,FLLISTC)
    USE mo_types,only: TFLIGHTLISTC
    IMPLICIT NONE
    INTEGER, INTENT(IN):: NFC
    TYPE(TFLIGHTLISTC),INTENT(INOUT)::FLLISTC(NFC)
    TYPE(TFLIGHTLISTC)::FLLISTCopx(NFC)
     INteger,DIMENSION(NFC)::arr,brr
     INTEGER I,J
 
    FLLISTCopx=FLLISTC   
    arr(1:NFC)=FLLISTC(1:NFC)%flight_id
    do I=1,NFC
    Brr(I)=I
    end do
    call sort2(NFC,arr,brr)
    do I=1,NFC
    J=BRR(I)
    FLLISTC(I)=FLLISTCopx(J)
    end do
!    do I=2,NFC
!    print*,'FLLISTC(I)=',I,FLLISTC(I)%flight_id,' ',FLLISTC(I)%adep,' ',FLLISTC(I)%ades
!    if(FLLISTC(I)%flight_id.le.FLLISTC(I-1)%flight_id) THEN
!    print*,' error in sort'
!    stop
!    end if
!    end do
    end subroutine  FLLISTssortC
    
      
 
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
        ENDIF
        IF(ARR(L).GT.ARR(IR))THEN
          TEMP=ARR(L)
          ARR(L)=ARR(IR)
          ARR(IR)=TEMP
          TEMP=BRR(L)
          BRR(L)=BRR(IR)
          BRR(IR)=TEMP
        ENDIF
        IF(ARR(L+1).GT.ARR(L))THEN
          TEMP=ARR(L+1)
          ARR(L+1)=ARR(L)
          ARR(L)=TEMP
          TEMP=BRR(L+1)
          BRR(L+1)=BRR(L)
          BRR(L)=TEMP
        ENDIF
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
        Print*, 'NSTACK TOO SMALL IN SORT2'
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
        ENDIF
      ENDIF
      GOTO 1
      END
!  (C) Copr. 1986-92 Numerical Recipes Software 2.02


   
    subroutine sFLIGHTadd(NW)
! adds FLIGHTadd(I)%ITw and FLIGHTadd(I)%IORIGINAL
      USE mo_directories, ONLY: FLIGHT,FLIGHTadd!,iline
      IMPLICIT NONE
      INTEGER,INTENT(IN)::NW
      INTEGER I,yyyy,mm,dd,hh,minmin,sec,ddold,IDAYY
      INTEGER, parameter:: IS=15
      ddold=-1
      do I=1,NW
    read(FLIGHT(I)%timestamp,'(I4,x,I2,1x,I2)') yyyy,mm,dd
    read(FLIGHT(I)%timestamp2,'(4(I2,1x))')hh,minmin,sec
    If(dd.ne.ddold) call DAYJ(yyyy,mm,dd,IDAYY)
    FLIGHTadd(I)%ITw=(IDAYY-1) *24*3600+hh*3600+minmin*60+sec
    FLIGHTadd(I)%IORIGINAL=1
!    write(12,'(2I12,2E15.7,G13.1)')iline-1+I,FLIGHTadd(I)%ITw-IUTC0,FLIGHT(I)%latitude,FLIGHT(I)%longitude,FLIGHT(I)%altitude
    end do
!    write(12,*)
    end subroutine sFLIGHTadd
    
 
    




      SUBROUTINE snavigation(NW,airportx,airporty,xw,yw,salpha,calpha)
! https://en.wikipedia.org/wiki/Great-circle_navigation
!  
!  alpha= Final COURSE
      use mo_cocip_parameters, only: PIR180
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
	

      real function distance_between_points(x1,y1, x2,y2)
      use mo_cocip_parameters, only: PIR180
      IMPLICIT NONE
      

      real,intent(IN):: x1,y1, x2,y2
      real lon1,lat1,lon2,lat2,d_lat,d_lon,a,c,dist
      
      
!      https://pypi.org/project/great-circle-calculator/#files
!      
      
!    """ This function computes the distance between two points in the unit given in the unit parameter.  It will
!    calculate the distance using the haversine unless the user specifies haversine to be False.  Then law of cosines
!    will be used
!    :param p1: tuple point of (lon, lat)
!    :param p2: tuple point of (lon, lat)
!    :param unit: unit of measurement. List can be found in constants.eligible_units
!    :param haversine: True (default) uses haversine distance, False uses law of cosines
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
       dist=d_lat**2+(d_lon*cos(lat1))**2 ! in radians
!!       PRINT*,'dist.gt.0.001?',dist
       if(dist.gt. 1.E-03) then     
!!       PRINT*,'dist.gt.0.001?  JA!'       
        a = sin(d_lat *0.5)**2 +cos(lat1)*cos(lat2)*sin(d_lon *0.5)**2
        c = 2 * atan2(sqrt(a), sqrt((1. - a)))
        dist =  c
        else
        dist=sqrt(dist)
        end if
!      else
!!    # Spherical Law Of Cosines
!      dist = acos(sin(lat1)*sin(lat2)+cos(lat1)*cos(lat2) &
!      *cos(lon2 - lon1))
!      end if
      distance_between_points=dist!*RADIUS
      end function  distance_between_points
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      
                 
      subroutine eliminatezerodistc(NW,ic)
! 
! eliminates points that have zero distance to previous one or zero groundspeed
      use mo_cocip_parameters, only: UNITY,PIR180
      USE mo_directories, ONLY: FLIGHT,FLLISTC,FLIGHTADD
      IMPLICIT NONE
      INTEGER,INTENT(IN)::IC
      INTEGER,INTENT(INout)::NW
      REAL x1,y1,x2,y2
      REAL dist,dx,dy,UNITX
      INTEGER I,NWNEW,inew(NW),K,J
      logical leliminate,lcopy
!      return
    
     
      NWNEW=0
      lcopy=.false.
      do I=1,NW-1  
      test1: if(flightadd(I)%ITW.lt.FLLISTC(IC)%ITWDEP-60) then
      leliminate=.true.
      else if(flightadd(I)%ITW.gt.FLLISTC(IC)%ITWDES+60) then
      leliminate=.true.
      else   test1   
      x1=FLIGHT(I)%longitude
      y1=FLIGHT(I)%latitude
      x2=FLIGHT(I+1)%longitude
      y2=FLIGHT(I+1)%latitude
      dist=abs(x2-x1)+abs(y2-y1)  
      UNITX=0.5*(y1+y2)*PIR180
      UNITX=UNITY*cos(UNITX)
      dx=(x2-x1)*UNITX
      dy=(y2-y1)*UNITY
      dist=dx*dx+dy*dy
      
      
      leliminate = dist.lt.10. 
      end if test1
      test2: if(leliminate) then
       lcopy=.true.
       else test2
      NWNEW=NWNEW+1
      inew(NWNEW)=I
      end if test2
      end do
       
      
      test4: if(lcopy) then
      do J=1,NWNEW
      K=INEW(J)
      if(K.ne.J) then
      FLIGHT(J)=FLIGHT(K)
      endif
      end do
      end if test4
      
     
   
      NW=NWNEW
     
      
      end SUBROUTINE  eliminatezerodistc
      
            
      
                    
      subroutine eliminatezerodists(NW,IS)
! 
! eliminates points that have zero distance to previous one or zero groundspeed
      use mo_cocip_parameters, only: UNITY,PIR180
      USE mo_directories, ONLY: FLIGHT,FLLISTS,FLIGHTADD
      IMPLICIT NONE
      INTEGER,INTENT(IN)::IS
      INTEGER,INTENT(INout)::NW
      REAL x1,y1,x2,y2
      REAL dist,dx,dy,UNITX
      INTEGER I,NWNEW,inew(NW),K,J
      logical leliminate,lcopy
!      return
    
     
      NWNEW=0
      lcopy=.false.
      do I=1,NW-1  
      test1: if(flightadd(I)%ITW.lt.FLLISTS(IS)%itwdep-60) then
      leliminate=.true.
      else if(flightadd(I)%ITW.gt.FLLISTS(IS)%itwdes+60) then
      leliminate=.true.
      else   test1   
      x1=FLIGHT(I)%longitude
      y1=FLIGHT(I)%latitude
      x2=FLIGHT(I+1)%longitude
      y2=FLIGHT(I+1)%latitude
      dist=abs(x2-x1)+abs(y2-y1)  
      UNITX=0.5*(y1+y2)*PIR180
      UNITX=UNITY*cos(UNITX)
      dx=(x2-x1)*UNITX
      dy=(y2-y1)*UNITY
      dist=dx*dx+dy*dy
      
      
      leliminate = dist.lt.10. 
      end if test1
      test2: if(leliminate) then
       lcopy=.true.
       else test2
      NWNEW=NWNEW+1
      inew(NWNEW)=I
      end if test2
      end do
 
      test3: if(FLIGHT(nw)%groundspeed.ne.0.) then     
      NWNEW=NWNEW+1
      inew(NWNEW)=nw
      end if test3
      
      
      test4: if(lcopy) then
      do J=1,NWNEW
      K=INEW(J)
      if(K.ne.J) then
      FLIGHT(J)=FLIGHT(K)
      endif
      end do
      end if test4
      
     
   
      NW=NWNEW
     
      
      end SUBROUTINE  eliminatezerodists
      
      
   
         subroutine Sairlinec(IC,Jairline)
  ! set up list of airlines based on the given 32-character airline codes
  ! Jairline is the airline number in FLLISTC 
  
   USE mo_directories, ONLY: Nairline,cairline,Nflairline,nairlinex,FLLISTC
 
  IMPLICIT NONE
  INTEGER,INTENT(IN):: IC
  INTEGER,INTENT(OUT) :: Jairline
  
 
 
  INTEGER I,J
  Print*
  print*,' Sairline start',Nairline
  Print*
  if(Nairline.eq.0 ) then
  Nairline=1
  cairline(1)=FLLISTC(IC)%airline
  Nflairline(1)=1
  Jairline=1
  else
  J=0
  do I=1,Nairline
  if ( cairline(I).eq.FLLISTC(IC)%airline) then
  j=I
  Jairline=J
  exit
  end if
  end do
  if(J.eq.0) then
  Nairline=Nairline+1
  Jairline=nairline
  if(Nairline.gt.nairlinex) then
  print*,'airline: Nairline.gt.nairlinex',Nairline
  stop
  end if
  cairline(Nairline)=FLLISTC(IC)%airline
   Nflairline(Nairline)=1
   else
   Nflairline(J)=Nflairline(J)+1
   end if
   end if
  
   end subroutine Sairlinec
   
   
      subroutine SairlineS(IS,Jairline)
  ! set up list of airlines based on the given 32-character airline codes
  ! Jairline is the airline number 
  ! same as before but for FLLISTS
   
   USE mo_directories, ONLY: Nairline,cairline,Nflairline,nairlinex,FLLISTS
 
  IMPLICIT NONE
  INTEGER,INTENT(IN):: IS
  INTEGER,INTENT(OUT) :: Jairline
 
 
  INTEGER I,J
  Print*
  print*,' Sairline start',Nairline
  Print*
  if(Nairline.eq.0 ) then
  Nairline=1
  cairline(1)=FLLISTS(IS)%airline
  Nflairline(1)=1
  Jairline=1
  else
  J=0
  do I=1,Nairline
  if ( cairline(I).eq.FLLISTS(IS)%airline) then
  j=I
  Jairline=J
  exit
  end if
  end do
  if(J.eq.0) then
  Nairline=Nairline+1
  Jairline=nairline
  if(Nairline.gt.nairlinex) then
  print*,'airline: Nairline.gt.nairlinex',Nairline
  stop
  end if
  cairline(Nairline)=FLLISTS(IS)%airline
   Nflairline(Nairline)=1
   else
   Nflairline(J)=Nflairline(J)+1
   end if
   end if
  
   
   end subroutine SairlineS
   
      
    subroutine Sairlineprint   
 ! prints list of airlines for FLLISTC
   USE mo_directories, ONLY: Nairline,cairline,Nflairline 
  IMPLICIT NONE 
  INTEGER J
   Print*
   do J=1,nairline
   print*,'J,cairline(j),Nflairline(J)',J,cairline(j),Nflairline(J)
   end do
   Print*
   end subroutine Sairlineprint
   
       
       
     
          
          