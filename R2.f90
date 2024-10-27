!
! R2 derives the daily load factors (for 365 days) for given TOW (file LFYEARs3),
! and derives a mass estimate for the NFS submission data for the derived daily load factors (file MassLF3)

! is based on  model coefficients derived for the PS model by Poll and Schumann, 
!    and uses Ian Poll's equations for load factors. 
! coded by Ulrich Schumann, Oct 2024





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
     REAL xdep,ydep,xdes,ydes,zdep,zdes
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
      REAL xdep,ydep,xdes,ydes,zdep,zdes
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
      INTEGER,PARAMETER:: NPS=69  !!!! NPS=NPS in psbeta3.inc
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
  REAL,PARAMETER:: cpe=1.2378*cp
  REAL,PARAMETER:: cpkero=2.*cp
  REAL,PARAMETER:: etacombustion=0.99
  
 
  REAL,PARAMETER:: CPE_CP= 1.2378
  
 
end module mo_cocip_parameters

PROGRAM main

  USE mo_directories, ONLY: FLLISTC,FLLISTS,iline &
  ,IUTC0,NWX,istopall,nwmax,NHIST,NPLOT,speedmax,NPS
  USE mo_cocip_parameters, ONLY: LCVCC
  IMPLICIT NONE

  INCLUDE "CPARA.inc"
  INCLUDE "psbeta3.inc"
  
  INTEGER ,PARAMETER:: NFCX=369014,NFSX=158150
  INTEGER istat,ILAST,NFL,NW,idold,NFs,I,IC,NFC,IS
  INTEGER*8 NFC8,NWTOT  
  real ,parameter::TOWZERO=-999.
  INTEGER NFOUNDC,NFOUNDS,NOTFOUND,JDAYJ0,IDAY!,icold
  INTEGER IDAY1
  real time0,time1,times,timec,timetot,fexp,LF
  real timess,times1,times0,floadfit,m,RT,error,tom,floadfits
  CHARACTER*10 cdate,cdates,cdatesdep
  INTEGER IY,IMONTH,IDAYM,Nout,nwr8,nwr9,Iday31122021,J,NIU &
  ,yyyy,mm,dd,hh,minmin,sec
  INTEGER IPS,NOTFOUNDPS,ichallenge,Ietafactor
  LOGICAL,PARAMETER:: LNEW=.true.
  LOGICAL,PARAMETER:: lwrite18=.false.
 LOGICAL,PARAMETER:: lwrite8=.true.
  REAL TOMS(NFSX)
       real meanLF(NPS),meanLFs(NPS)
       real,dimension(365):: meanLFd,meanerr,meanerrd
       real,dimension(365):: meanLFsd
       real allerr,allf,fexpMin,fuelflow_per_meter
       INTEGER nmeanLF(NPS), nmeanLFs(NPS)
       INTEGER nallerr,nallf
!!!  call testdist
        INTEGER,dimension(365) ::nfoundday,nmeanLFd,nmeanLFsd
        real,parameter:: minfactor=0.90,maxfactor=1.0!2
        REAL fexpn,etafactor
        real       etalow(NPS),etahigh(NPS)
        Integer netalow(NPS),netahigh(NPS)
        real aerr,aerr0
     
        aerr0=1.e10

        NOTFOUNDPS=0
       nfoundday=0
       NIU=0
       meanLF=0.
       nmeanLF=0
       meanLFd=0
       nmeanLFd=0
       meanerr=0
       meanerrd=0
       
      
 ! compute number of reference day (end of 2021) 
       CALL CPU_TIME(time0)
       call    DAYJ(2021,12,31,JDAYJ0,3)
       IUTC0=(JDAYJ0-1)*24*3600 ! preliminary values
 !      print*,' JDAYJ0',JDAYJ0
       istopall=0
       speedmax=0.
       nwmax=0
       NHIST=0
       NPLOT=0
       call AIRPORTINIT
       Print*,' AIRPORTINIT done'
 ! prepare data FLLISTC and FLLISTS data  
       ALLOCATE(FLLISTC(NFCX),stat=istat)
    Print*,' ALLOCATE: FLLISTC(NFCX),stat=',istat 
       OPen(5,file='EU/challenge_setb.csv',action='read')
       print*,'EU/challenge_setb.csv done',NFCX
       CALL gtchallengesetC(NFCX,FLLISTC,NFC)
       print*,'after gtchallengesetC',nfc
   
    
       PRINT*,'NFC,NFCX',NFC,NFCX
       print*,'fllistc(1)',fllistc(1)
      I=1
      print*,'I',I,'fllistc(I)',fllistc(I)
   
      Print*

       I=NFc
      print*,'I',I,'fllistc(I)',fllistc(I)
    
         ALLOCATE(FLLISTS(NFSX),stat=istat)
    Print*,' ALLOCATE: FLLISTS(NFSX),stat=',istat 
       OPen(5,file='EU/final_submission_setb.csv',action='read')
       CALL gtchallengesets(NFSX,FLLISTs,NFS)
    
       PRINT*,'NFS,NFSX',NFS,NFSX
       print*,'fllists(1)',fllists(1)
      I=1
      print*,'I',I,'fllists(I)',fllists(I)
   
      Print*

       I=NFs
      print*,'I',I,'fllists(I)',fllists(I)
    
    

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
      open(27,file='etafactor.txt')
          letaf:   do Ietafactor=0,0
!################# get ps model #####################################################       
       call psbeta3INIT
             
!     picao(IPS),nmeaner(IPS),meanerr(IPS) B772         8322  0.335633010
!  picao(IPS),nmeaner(IPS),meanerr(IPS) B77W         9162  0.196391582
!  picao(IPS),nmeaner(IPS),meanerr(IPS) B788         5960  0.226188838
!  picao(IPS),nmeaner(IPS),meanerr(IPS) B789         5734  0.186104044

!      etafactor=1.+Ietafactor*0.01
!      call ACNUMBPSbeta('B772',IPS) 
!      etaL_Do(IPS)=etaL_Do(IPS)*etafactor
!      call ACNUMBPSbeta('B77W',IPS) 
!      etaL_Do(IPS)=etaL_Do(IPS)*etafactor
!      call ACNUMBPSbeta('B788',IPS) 
!      etaL_Do(IPS)=etaL_Do(IPS)*etafactor
!      call ACNUMBPSbeta('B789',IPS) 
!      etaL_Do(IPS)=etaL_Do(IPS)*etafactor
     
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end of ps completion     
  
      
      
       
            call  DAYJ(2021,12,31,Iday31122021,1)
            
            j=0
         
            
       
!######################################################################   
!   dervive  load factor estimates  
  
  NFOUNDC=0

             CALL CPU_TIME(time0)
   

   allf=0.
   nallf=0
          etalow=0.
        netalow=0
         etahigh=0.
        netahigh=0
     open(20,file='lfmin.txt')
   open(21,file='lfmax.txt')
        
    write(20,*) 'ICAO ','fitting_Loadfactor ' &
    ,'given_TOM/kg ', 'PS_OEM/kg ', 'PS_MTOM/kg '  &
        ,'PS_etaLDo ', 'fitting_etaLD0 ','ground_distance/nml ' &
        ,' fligth_id '
    write(21,*) 'ICAO ','fitting_Loadfactor '  &
    ,'given_TOM/kg ', 'PS_OEM/kg ', 'PS_MTOM/kg ' &
        ,'PS_etaLDo ', 'fitting_etaLD0 ','ground_distance/nml ' &
        ,' fligth_id '
 
        doic: DO IC=1,NFC
         read(FLLISTC(Ic)%cdatex,'(I4,2(1x,i2))') yyyy,mm,dd
         call  DAYJ(yyyy,mm,dd,IDAY1,1)
         IDAY=IDAY1-Iday31122021
        call ACNUMBPSbeta(FLLISTC(Ic)%aircraft_type,IPS) 
       
        lpsvalid: if(IPS.gt.0) then
          m=FLLISTC(IC)%tow
          RT=FLLISTC(Ic)%flown_distance*1820
       fexp=exp(-(0.014+1.015*(g*RT/(etaL_Do(IPS)*LCVCC))))-0.05
      floadfit=  (m*fexp-POEM(IPS))/(PMZFM(IPS)-POEM(IPS))

      
        if(floadfit.lt.0.) then
        fexpn=POEM(IPS)/FLLISTC(IC)%tow
        fexpn=-log(fexpn+0.05)
        fexpn=(fexpn-0.014)/(1.015*(g*RT/LCVCC))
        fexpn=1./fexpn
  
        write(20,*) PICAO(IPS),' ',floadfit,m,POEM(IPS),PMTOM(IPS) &
        ,etaL_Do(IPS),fexpn,FLLISTC(Ic)%flown_distance, FLLISTC(Ic)%flight_id!,fexpn
        if(fexpn.gt.0.) then
        etalow(IPS)=etalow(IPS)+log(fexpn/etaL_Do(IPS))
        netalow(IPS)=netalow(IPS)+1
        end if
        end if
        if(floadfit.gt.1.) then
        fexpn=PMZFM(IPS)/FLLISTC(IC)%tow
        fexpn=-log(fexpn+0.05)
        fexpn=(fexpn-0.014)/(1.015*(g*RT/LCVCC))
        fexpn=1./fexpn
       write(21,*) PICAO(IPS),' ',floadfit,m,POEM(IPS),PMTOM(IPS) &
        ,etaL_Do(IPS),fexpn,FLLISTC(Ic)%flown_distance,FLLISTC(Ic)%flight_id!,fexpn
         if(fexpn.gt.0.) then
        etahigh(IPS)=etahigh(IPS)+log(fexpn/etaL_Do(IPS))
        netahigh(IPS)=netahigh(IPS)+1
        end if
        end if
 
      
      
        floadfit=MIN(minfactor,max(floadfit,maxfactor-1.))
        meanLF(IPS)=meanLF(IPS)+floadfit
        nmeanLF(IPS)=nmeanLF(IPS)+1

        meanLFd(IDAY)=meanLFd(IDAY)+floadfit
        nmeanLFd(IDAY)=nmeanLFd(IDAY)+1
        
        else lpsvalid
!         print*,' IC,FLLISTC(I)%TOw',IC,FLLISTC(Ic)%TOw ,' ',FLLISTC(Ic)%aircraft_type  ,' missing'
        NOTFOUNDPS=NOTFOUNDPS+1
        print*,' IPS=0??',IPS,NOTFOUNDPS
        stop
           end if lpsvalid
            
        end do doic
        
        close(20)
        close(21)
        print*
        do IPS=1,NPS
        if (netahigh(IPS).gt.0) then
        print*, 'high ', PICAO(IPS),' ',netahigh(IPS),exp(etahigh(IPS)/netahigh(IPS))
        if(netahigh(IPS).gt.100) then
        etaL_Do(IPS)=etaL_Do(IPS)*sqrt(exp(etahigh(IPS)/netahigh(IPS)))
        end if
        endif
        end do
        print*
           do IPS=1,NPS
        if (netalow(IPS).gt.0) then
        print*, 'low ',PICAO(IPS),' ',netalow(IPS),exp(etalow(IPS)/netalow(IPS))
        if(netalow(IPS).gt.1000) then
        etaL_Do(IPS)=etaL_Do(IPS)*sqrt(sqrt(exp(etahigh(IPS)/netahigh(IPS))))
        end if
        endif
        end do
 !       stop
!!!!!!!!!!!!!!!!!!!!!!!!!!!

!! again with modifed etaLDo values

   allf=0.
   nallf=0
          etalow=0.
        netalow=0
         etahigh=0.
        netahigh=0
!     open(20,file='lfmin.txt')
!   open(21,file='lfmax.txt')
        doicx: DO IC=1,NFC
         read(FLLISTC(Ic)%cdatex,'(I4,2(1x,i2))') yyyy,mm,dd
         call  DAYJ(yyyy,mm,dd,IDAY1,1)
         IDAY=IDAY1-Iday31122021
        call ACNUMBPSbeta(FLLISTC(Ic)%aircraft_type,IPS) 
       
        lpsvalid2: if(IPS.gt.0) then
          m=FLLISTC(IC)%tow
          RT=FLLISTC(Ic)%flown_distance*1820
       fexp=exp(-(0.014+1.015*(g*RT/(etaL_Do(IPS)*LCVCC))))-0.05
      floadfit=  (m*fexp-POEM(IPS))/(PMZFM(IPS)-POEM(IPS))

      
        if(floadfit.lt.0.) then
!        fexpn=POEM(IPS)/FLLISTC(IC)%tow
        fexpn=-log(fexpn+0.05)
        fexpn=(fexpn-0.014)/(1.015*(g*RT/LCVCC))
        fexpn=1./fexpn
  
 !       write(20,*) PICAO(IPS),' ','LF=',floadfit,'TOM,OEM,MTOM=',TOM,POEM(IPS),PMTOM(IPS) &
 !       ,'h,hnew=',etaL_Do(IPS),fexpn,' dist=',FLLISTC(Ic)%flown_distance,fexpn
        if(fexpn.gt.0.) then
        etalow(IPS)=etalow(IPS)+log(fexpn/etaL_Do(IPS))
        netalow(IPS)=netalow(IPS)+1
        end if
        end if
        if(floadfit.gt.1.) then
        fexpn=PMZFM(IPS)/FLLISTC(IC)%tow
        fexpn=-log(fexpn+0.05)
        fexpn=(fexpn-0.014)/(1.015*(g*RT/LCVCC))
        fexpn=1./fexpn
!        write(21,*) PICAO(IPS),' ','LF=',floadfit,'TOM,OEM,MTOM=',TOM,POEM(IPS),PMTOM(IPS) &
!        ,'h,hnew=',etaL_Do(IPS),fexpn,' dist=',FLLISTC(Ic)%flown_distance,fexpn
         if(fexpn.gt.0.) then
        etahigh(IPS)=etahigh(IPS)+log(fexpn/etaL_Do(IPS))
        netahigh(IPS)=netahigh(IPS)+1
        end if
        end if
 
      
      
   
!        print*,' IC,FLLISTC(I)%TOw',IC,FLLISTC(Ic)%TOw ,' ',FLLISTC(Ic)%aircraft_type  ,PMTOM(IPS)&
!        ,POEM(IPS),'LF=',floadfit
        floadfit=MIN(minfactor,max(floadfit,maxfactor-1.))
        meanLF(IPS)=meanLF(IPS)+floadfit
        nmeanLF(IPS)=nmeanLF(IPS)+1

        meanLFd(IDAY)=meanLFd(IDAY)+floadfit
        nmeanLFd(IDAY)=nmeanLFd(IDAY)+1
        
        else lpsvalid2
        
        NOTFOUNDPS=NOTFOUNDPS+1
         print*,' invalid Aircraft type, 2: IPS,NOTFOUNDPS=',ips,NOTFOUNDPS
        
        stop
           end if lpsvalid2
            
        end do doicx
        
        close(20)
        close(21)
        print*
        do IPS=1,NPS
        if (netahigh(IPS).gt.0) then
        print*, 'high ', PICAO(IPS),' ',netahigh(IPS),exp(etahigh(IPS)/netahigh(IPS))
!        if(netahigh(IPS).gt.100) then
!        etaL_Do(IPS)=etaL_Do(IPS)*sqrt(sqrt(exp(etahigh(IPS)/netahigh(IPS))))
!        end if
        endif
        end do
        print*
           do IPS=1,NPS
        if (netalow(IPS).gt.0) then
        print*, 'low ',PICAO(IPS),' ',netalow(IPS),exp(etalow(IPS)/netalow(IPS))
!        if(netalow(IPS).gt.1000) then
!        etaL_Do(IPS)=etaL_Do(IPS)*sqrt(sqrt(exp(etahigh(IPS)/netahigh(IPS))))
!        end if
        endif
        end do
     
!!!!!!!!!!!!!!!!!!!!!!!!!!! OUTPUT of Load factors in to file LFYEAR2.txt'

        
        print*
        print*,' mean values'
  
! 1. versus day of year    
       Print*
 !       print*,' nallf,allf',nallf,sqrt(allf/nallf)
        print*
       open(7,file='git/LFYEAR2.txt')
       print*
       do I=1,365
!       print*,' nmeanLFd(i), meanLFd(I)',I,nmeanLFd(i),meanLFd(I)/max(nmeanLFd(i),1) 
       meanLFd(I)=meanLFd(I)/max(nmeanLFd(i),1)
       write(7,*) I,nmeanLFd(i),meanLFd(I)       
       end do
       close(7)
       print*
! 2. versus aircraft type  
   allf=0.
   nallf=0       
             Print*
       do IPS=1,NPS
      
       allf=allf+meanLF(IPS)
       nallf=nallf+nmeanLF(IPS)
   
       
       if(nmeanLF(IPS).gt.0) then
       print*,' picao(IPS),nmeanLF(IPS),meanLF(IPS)/max(1,nmeanLF(IPS)) ' &
       ,picao(IPS),' ',nmeanLF(IPS),meanLF(IPS)/max(1,nmeanLF(IPS))
       end if
 !      Print*,'PMZFM(IPS),POEM(IPS)', picao(IPS),' :',PMZFM(IPS),POEM(IPS)
       
       end do
       Print*
       
       PRINT*,' NOTFOUNDPS', NOTFOUNDPS
       
       
       
        
      
!######################################################################     TOM for C and mean errors
        
          nmeanLF=0

        meanerr=0.
        nmeanLFd=0
        meanerrd=0.
        allerr=0.
        
        fexpMin=1.
        open(7,file='git/MassLF3.txt')
   open(20,file='git/oem_errorlf.txt')
   open(21,file='git/mtom_errorlf.txt')
        doic2: DO IC=1,NFC
        TOM=0.
         read(FLLISTC(Ic)%cdatex,'(I4,2(1x,i2))') yyyy,mm,dd
         call  DAYJ(yyyy,mm,dd,IDAY1,1)
         IDAY=IDAY1-Iday31122021
        call ACNUMBPSbeta(FLLISTC(Ic)%aircraft_type,IPS) 
       ifpsvalid3:  if(IPS.gt.0) then
       RT=FLLISTC(Ic)%flown_distance*1820
       fexp=exp(-(0.014+1.015*(g*RT/(etaL_Do(IPS)*LCVCC))))-0.05
       
       if((fexp.lt.0.1) .or. (fexp.gt.1.)) then
       print*,' fexp out of range', fexp
       stop
       end if
       
       fexpMin=min(fexp,fexpmin)

       if((IDAY.le.0) .or. (IDAY.gt.365)) then
       print*,'IDAY out of range', IDAY
       stop
       end if

      TOM=(meanLFd(IDAY)*PMZFM(IPS)+(1.-meanLFd(IDAY))*POEM(IPS))/fexp

!      if(isnan(TOM)) then
!      print*,'tom nan', TOM
!      stop
!      end if
!        print*,' IC,FLLISTC(I)%TOw',IC,FLLISTC(Ic)%TOw ,' ',FLLISTC(Ic)%aircraft_type  ,PMTOM(IPS)&
!        ,POEM(IPS),'TOM=',TOM,'meanLFd',IDAY,meanLFd(IDAY),' fexp', fexp

        if(TOM.lt.POEM(IPS)) then
        write(20,*) PICAO(IPS),' ','TOM,OEM,MTOM=',TOM,POEM(IPS),PMTOM(IPS) &
        ,'h,dist=',etaL_Do(IPS),FLLISTC(Ic)%flown_distance &
        ,FLLISTC(Ic)%flight_id
        end if
        if(TOM.gt.PMTOM(IPS)) then
        write(21,*) PICAO(IPS),' ','TOM,OEM,MTOM=',TOM,POEM(IPS),PMTOM(IPS) &
        ,'h,dist=',etaL_Do(IPS),FLLISTC(Ic)%flown_distance &
        ,FLLISTC(Ic)%flight_id
        end if
        TOM=max(TOM ,maxfactor*POEM(IPS))
        TOM=min(TOM ,minfactor*PMTOM(IPS))
        
        
        error=(TOM-FLLISTC(Ic)%TOw)/FLLISTC(Ic)%TOw
        meanerr(IPS)=meanerr(IPS)+error**2
        nmeanLF(IPS)=nmeanLF(IPS)+1

        meanerrd(IDAY)=meanerrd(IDAY)+error**2
        nmeanLFd(IDAY)=nmeanLFd(IDAY)+1
        
   
        end if  ifpsvalid3
        write(7,*)FLLISTC(Ic)%flight_id,TOM
        end do doic2
        close(7)
        close(20)
        close(21)
  
        
           print*
 
       
       open(7,file='git/LFYEARerror2.txt')
       print*
       do I=1,365
       meanerrd(I)=sqrt(meanerrd(I)/max(nmeanLFd(i),1) )
!       print*,' nmeanLFd(i), meanLFd(I)',I,nmeanLFd(i),meanerrd(I) 
       write(7,*) I,nmeanLFd(i),meanerrd(I)
       
       end do
       close(7)
       print*
       
       
       print*,' mean error values'
        Print*
         nallerr=0
         allerr=0.
       do IPS=1,NPS
    
     
       allerr=allerr+meanerr(IPS)
       nallerr=nallerr+nmeanLF(IPS)
         if(nmeanLF(IPS).gt.0) then
       meanerr(IPS) = sqrt(meanerr(IPS)/max(1,nmeanLF(IPS)))
       print*,' picao(IPS),nmeaner(IPS),meanerr(IPS) ' &
       ,picao(IPS),' ',nmeanLF(IPS),meanerr(IPS)
       end if
       end do
       Print*
       
  print*,' frequent flying aircraft types (more than 1000 flights)'     
       do IPS=1,NPS
       if(nmeanLF(IPS).gt.1000) then
             print*,' picao(IPS),nmeaner(IPS),meanerr(IPS) ' &
       ,picao(IPS),' ',nmeanLF(IPS),meanerr(IPS)
       end if
       end do
       Print*
       
       
       print*,' nallerr,allerr',nallerr,sqrt(allerr/nallerr)
       
       Print*,' fexpMin', fexpMin
 
        
!###################################################################### 
! end of processing given TOM values
   
   

   
             CALL CPU_TIME(time1)
      timetot=(time1-time0)
   
     print*,' all DAY LOOPS timetot',timetot
     Print*,' before submission part'
 !    Print*,'stop before submission'
!???     stop
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! submission     
     
 
      
!######################################################################   
!  TOM for Submission using estimated load factors
        
          nmeanLF=0

        meanerr=0.
        nmeanLFd=0
        meanerrd=0.
      
        
        fexpMin=1.
        
        
        meanLFs=0.
        nmeanLFs=0

        meanLFsd=0.
        nmeanLFsd=0
        
        fexpMin=1.
        doics: DO IS=1,NFs
         read(FLLISTS(IS)%cdatex,'(I4,2(1x,i2))') yyyy,mm,dd
         call  DAYJ(yyyy,mm,dd,IDAY1,1)
         IDAY=IDAY1-Iday31122021
        call ACNUMBPSbeta(FLLISTS(IS)%aircraft_type,IPS) 
!        print*,'IS,IPS,IDAY',IS,IPS,IDAY
        if(IPS.gt.0) then
       RT=FLLISTS(IS)%flown_distance*1820
       fexp=exp(-(0.014+1.015*(g*RT/(etaL_Do(IPS)*LCVCC))))-0.05
       if((fexp.lt.0.1) .or. (fexp.gt.1.)) then
       print*,' fexp out of rangefexp,RT,etaL_Do(IPS)', fexp,RT,etaL_Do(IPS)
       stop
       end if
       fexpMin=min(fexp,fexpmin)

       if((IDAY.le.0) .or. (IDAY.gt.365)) then
       print*,'IDAY out of range', IDAY
       stop
       end if
       LF=meanLFd(IDAY)
       LF=min(LF,minfactor)
       LF=Max(LF,maxfactor-1.)
      TOM=(LF*PMZFM(IPS)+(1.-LF)*POEM(IPS))/fexp
      if(isnan(TOM)) then
      print*,'invalid  TOM',TOM
      print*,'invalid  PMTOM(IPS)',PMTOM(IPS)
      print*,'invalid  meanLFd(IDAY)',meanLFd(IDAY)
      print*,'invalid  PMZFM(IPS)',PMZFM(IPS)
      print*,'invalid  POEM(IPS)',POEM(IPS)
      print*,'invalid  fexp,LF',fexp,LF
      stop
      end if
      TOM=min(TOM,PMTOM(IPS))
  
      
      TOMS(Is)=TOM
 !     print*,' IDAY,meanLFd(IDAY),LF',IDAY,meanLFd(IDAY),LF
    
      m=TOM      
      floadfit=(m*fexp-POEM(IPS))/(PMZFM(IPS)-POEM(IPS))
      floadfits=(m-POEM(IPS))/(PMTOM(IPS) -POEM(IPS))
   
   
     
        meanLFs(IPS)=meanLFs(IPS)+floadfit
        nmeanLFs(IPS)=nmeanLFs(IPS)+1

        meanLFsd(IDAY)=meanLFsd(IDAY)+floadfit
        nmeanLFsd(IDAY)=nmeanLFsd(IDAY)+1
 !!!??           print*,'IPS,tom,fexp,etaL_Do(IPS),floadfit,floadfits,LF ',PICAO(IPS),' ',IPS,TOM,fexp,etaL_Do(IPS),floadfit,floadfits,LF
         ELSE
         print*,'IPS',IPS
         stop
       
        end if  
!        if(FLLISTS(IS)%aircraft_type.eq.'AT76') stop
      
        end do doics
        
        Print*,' fexpMin', fexpMin
       
       allf=0
       nallf=0
            print*
        print*,' submission mean values'
    
        print*
       open(7,file='git/LFYEARs3.txt')
       print*
       do I=1,365
           meanLFsd(I)=meanLFsd(I)/max(nmeanLFsd(i),1)
!       print*,' nmeanLFsd(i), meanLFsd(I)',I,nmeanLFsd(i),meanLFsd(I)
        write(7,*) I,nmeanLFsd(i),meanLFsd(I)       
       end do
       close(7)
       
       
       Print*
       do IPS=1,NPS
       allf=allf+meanLFs(IPS)
       nallf=nallf+nmeanLFs(IPS)
       if(nmeanLFs(IPS).gt.0) then
       print*,' subm: picao(IPS),nmeanLFs(IPS),meanLFs(IPS)/max(1,nmeanLFs(IPS)) ' &
       ,picao(IPS),' ',nmeanLFs(IPS),meanLFs(IPS)/max(1,nmeanLFs(IPS))
       write(27,*) picao(IPS),' ',nmeanLFs(IPS),meanLFs(IPS)/max(1,nmeanLFs(IPS))
       end if
       end do
       Print*
       
       PRINT*,' subm: NOTFOUNDPS', NOTFOUNDPS
       Print*
        print*,' subm: nallf,allf',nallf,sqrt(allf/max(1,nallf))
       print*
       
       Print*,'submission data prepared'
       
       open(7, &
       file='git/final_submisson.csv')
       write(7,*)'flight_id, tow'
        DO IS=1,NFs
        write(7,*) FLLISTS(IS)%flight_id,', ',TOMS(IS)
        end do
        close(7)
       
! reminder       
           print*,' competion resutl: nallerr,allerr',nallerr,sqrt(allerr/nallerr)
           write(27,*) etafactor,sqrt(allerr/nallerr)
           aerr=sqrt(allerr/nallerr)
           if(aerr.gt.aerr0) exit
           aerr0=aerr
           end do letaf
           
      open(28,file='PS_B772_plot.txt')
      write(28,*)'I flight_id ICAO Jairline TOM_MTOM TOM_OEM'
      call ACNUMBPSbeta('B772',IPS)
      J=0
      do I=1,NFC
      if(FLLISTC(I)%aircraft_type.eq.'B772') then
      write(28,*) I,FLLISTC(I)%flight_id,FLLISTC(I)%aircraft_type,J &
      ,FLLISTC(I)%TOW/PMTOM(IPS),FLLISTC(I)%TOW/POEM(IPS)
      end if
      end do
      close(28)
      open(28,file='PS_B788_plot.txt')
      write(28,*)'I flight_id ICAO Jairline TOM_MTOM TOM_OEM'
      call ACNUMBPSbeta('B788',IPS)
      J=0
      do I=1,NFC
      if(FLLISTC(I)%aircraft_type.eq.'B788') then
      write(28,*) I,FLLISTC(I)%flight_id,FLLISTC(I)%aircraft_type,J &
      ,FLLISTC(I)%TOW/PMTOM(IPS),FLLISTC(I)%TOW/POEM(IPS)
      end if
      end do
      close(28)
           
     
  
   Print*,'################################################# END of Pogram Main'
          
  deallocate(FLLISTS)
  
   
   deallocate(FLLISTC)
  
     close(27)
   end PROGRAM  main
  
    
 
       subroutine gtchallengesetc(NFLX,FLLISTC,NFC)
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
        call findairportdepdes(FLLISTC(I)%adep &
        ,FLLISTC(I)%ades,FLLISTC(I)%xdep &
      ,FLLISTC(I)%ydep,FLLISTC(I)%zdep,FLLISTC(I)%xdes &
      ,FLLISTC(I)%ydes,FLLISTC(I)%zdes)
      
!      findairportdepdes(dep,des,xdep,ydep,zdep,xdes,ydes,zdes)
      
       FLLISTC(I)%IFLinitialcruise=0
       FLLISTC(I)%IFLmax=0
     read(FLLISTC(I)%actual_offblock_time,'(I4,1x,5(I2,1x))') & 
     yyyy,mm,dd,hh,minmin,sec
     If(dd.ne.ddold) call DAYJ(yyyy,mm,dd,IDAYY,is)
     ddold=dd
      FLLISTC(I)%itwdep=(IDAYY-1) *24*3600+hh*3600+minmin*60+sec &
      +FLLISTC(I)%taxiout_time*60  !time in minutes
      
     read(FLLISTC(I)%arrival_time,'(I4,1x,5(I2,1x))') & 
     yyyy,mm,dd,hh,minmin,sec
     If(dd.ne.ddold) call DAYJ(yyyy,mm,dd,IDAYY,is)
     ddold=dd
     
     
!      FLLISTC(I)%itwdes= FLLISTC(I)%itwdep+FLLISTC(I)%flight_duration*60 !!! ???
      FLLISTC(I)%itwdes=(IDAYY-1) *24*3600+hh*3600+minmin*60+sec 
 !    Print*,'FLLISTC(I)%flight_duration in min or s?',FLLISTC(I)%flight_duration*60
 !    Print*,'FLLISTC(I)%itwdes-FLLISTC(I)%itwdep?',FLLISTC(I)%itwdes-FLLISTC(I)%itwdep
      
 !     if(I.gt.100 ) stop
!       print* &
!       ,' FLLISTC(I)%xdep,FLLISTC(I)%ydep,FLLISTC(I)%xdes,FLLISTC(I)%ydes' &
!       ,FLLISTC(I)%xdep,FLLISTC(I)%ydep,FLLISTC(I)%xdes,FLLISTC(I)%ydes
       end do
 !      print*, 'NFC',NFC
!       print*,' findairportdepdes done'
       CALL FLLISTssortC(NFC,FLLISTC)
       end subroutine  gtchallengesetc
       
 
       subroutine gtchallengesets(NFLX,FLLISTS,NFS)
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
       print*,'gtchallengesets: too short'
       stop
  999 continue
!       Print*,'sets:  NFS,NFLX',NFS,NFLX
       do I=1,NFS
       call findairportdepdes(FLLISTS(I)%adep &
       ,FLLISTS(I)%ades,FLLISTS(I)%xdep &
      ,FLLISTS(I)%ydep,FLLISTS(I)%zdep,FLLISTS(I)%xdes &
      ,FLLISTS(I)%ydes,FLLISTS(I)%zdes)
       end do
!       print*,' findairportdepdes done'
       call FLLISTCsortS(NFS,FLLISTS)
       
             do I=1,NFS
       
     read(FLLISTS(I)%actual_offblock_time,'(I4,1x,5(I2,1x))') & 
     yyyy,mm,dd,hh,minmin,sec
     If(dd.ne.ddold) call DAYJ(yyyy,mm,dd,IDAYY,is)
     ddold=dd
      FLLISTS(I)%itwdep=(IDAYY-1) *24*3600+hh*3600+minmin*60+sec &
      +FLLISTS(I)%taxiout_time*60  !time in minutes
      
     read(FLLISTS(I)%arrival_time,'(I4,1x,5(I2,1x))') & 
     yyyy,mm,dd,hh,minmin,sec
     If(dd.ne.ddold) call DAYJ(yyyy,mm,dd,IDAYY,is)
     ddold=dd
    ddold=dd
  
     
     
!      FLLISTS(I)%itwdes= FLLISTS(I)%itwdep+FLLISTS(I)%flight_duration*60 !!! ???
      FLLISTS(I)%itwdes=(IDAYY-1) *24*3600+hh*3600+minmin*60+sec 
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
!    if( 258074338 .eq. FLLISTC(I)%flight_id) print*,'FLLISTC(I)=',I,FLLISTC(I)%flight_id,' ',FLLISTC(I)%adep,' ' ,FLLISTC(I)%ades,I
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
!    print*,'FLLISTC(I)=',I,FLLISTC(I)%flight_id,' ',FLLISTC(I)%adep,' ' ,FLLISTC(I)%ades
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
!    print*,'FLLISTC(I)=',I,FLLISTC(I)%flight_id,' ',FLLISTC(I)%adep,' ' ,FLLISTC(I)%ades
!    if(FLLISTC(I)%flight_id.le.FLLISTC(I-1)%flight_id) THEN
!    print*,' error in sort'
!    stop
!    end if
!    end do
    end subroutine  FLLISTssortC
    
 