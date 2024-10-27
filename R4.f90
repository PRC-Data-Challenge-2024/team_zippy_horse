! 
! R4 computes the linear least square equation coefficients X1 and X2 
!              to derive the mass from the initial climb rate  (climbdataca)
! computes mean correction factors mean errors (nmeanerrorac)
! and finds the best fitting method (1 out of 5)  (ibestm)

!  uses of the PS model for load factor analysis from Poll and Schumann
! coded by Ulrich Schumann, Oct 2024
! uses the PS model coefficients for mass and etaLDo
! and the flight lists FLISTC and FLISTS  (no trajectory information)
!
! coded by Ulrich Schumann, Oct 2024
! 

MODULE mo_types
 
   
     type Tclimbdata      
     real  clrm,a12,a22,b1,b2,x1,x2 
     INTEGER nclr
     end type Tclimbdata
  
  
END MODULE mo_types

MODULE mo_directories ! many code details
  USE mo_types,only: Tclimbdata
  IMPLICIT NONE
    
     type (Tclimbdata),DIMENSION(:,:),ALLOCATABLE  :: climbdataca
     type (Tclimbdata),DIMENSION(:,:),ALLOCATABLE  :: climbdata
     INTEGER nclimbdata
     INTEGER IPS
     INTEGER,PARAMETER::NMethod=6
     INTEGER,PARAMETER::M=NMETHOD-1
     INTEGER,PARAMETER:: NPS=69  !!!! NPS=NPS in psbeta3.inc
     integer,parameter:: nairlinex=50
       INTEGER,PARAMETER::NCMAX=369013
   
     real,DIMENSION(Nmethod,NPS,nairlinex):: meanerrora
     real,DIMENSION(Nmethod,NPS,nairlinex):: quadraticerrorac,meanerrorac
     INTEGER,DIMENSION(Nmethod,NPS,nairlinex):: nmeanerra,nmeanerrorac
     INTEGER,DIMENSION(NPS,nairlinex):: nmeanone
     
     
   
     INTEGER JDAYJ0
  
        real,dimension(NCMAX):: massxfinal
          integer imassvalid(Nmethod-1,NCMAX),imassvalidc(Nmethod-1,NCMAX)
          real massresult5(Nmethod-1,NCMAX),clrout(NCMAX) &
         ,TOW(NCMAX),flmax(NCMAX),altmax(NCMAX) &
         ,err5c(Nmethod-1,NCMAX),massresult5c(Nmethod-1,NCMAX)
         INTEGER ITEST,ICC(NCMAX),nclrout(NCMAX),flight_id(NCMAX)
 
         INTEGER Jairline(NCMAX)
     
           character*4 ATYP(NCMAX)
           INTEGER IPSC(NCMAX)

END MODULE mo_directories 

PROGRAM main

  USE mo_directories,ONLY: IPS  &
 ,quadraticerrorac,nmeanerra,meanerrora &
  , nmethod  &
  ,NCMAX,imassvalid,ITEST  &
  ,Jairline,NPS,ICC,JDAYJ0,flight_id &
  ,climbdataca,climbdata,nclimbdata &
  ,nairlinex,massresult5c ,err5c &
  ,massresult5,clrout,nclrout,TOW,flmax,altmax,atyp,massxfinal,ipsc &
  ,meanerrorac,nmeanerrorac,nmeanone,M
 
  IMPLICIT NONE
  INCLUDE "CPARA.inc"
  INCLUDE "psbeta3.inc"
  INTEGER,PARAMETER:: NFCX=1,NFSX=1
  
  INTEGER istat,NW,I,J,K   
  INTEGER IDAY
  real time0,time1,timec
  real*8 det,BTESTmax,btest1,btest2,da11
  INTEGER,DIMENSION(nps,nairlinex)::  ibestm
        
  CHARACTER*10 cdate    
  INTEGER Imethod
  INTEGER  iairline,nbest(0:Nmethod)
  real*8 errsum,er,errrms,erminl,ermaxl
 
 
  INTEGER Ner
   INTEGER ibestmall(NCMAX)
 
  LOGICAL,PARAMETER:: LWRITE7=.false.
  
  
   integer nsumps(NPS),nsumac(nairlinex)
  
  integer ibestsequence(5)
  
  INTEGER,PARAMETER:: nipsexclude=4
       INTEGER IPSexclude(nipsexclude)
  nsumps=0
  nsumac=0
  nbest=0
  ner=0

  
  
  do I=1,5
     ibestsequence(I)=I
  end do
  
 
      open(50,file='git/R4_result.txt')
      
! for compution of X1 and X2
    
  
       CALL CPU_TIME(time0)
       call    DAYJ(2021,12,31,JDAYJ0,3)
  
 
      CALL CPU_TIME(time1)
      timec=(time1-time0)
      print*,' timec',timec
 Print*,'#################################################'
 
      
 allocate(climbdataca(nps,nairlinex),stat=istat) 
 Print*,' ALLOCATE: climbdataca(nps,nairlinex),stat,NPS=',istat,nps,nairlinex  
 allocate(climbdata(nps,nairlinex),stat=istat) 
 Print*,' ALLOCATE: climbdata(nps,nairlinex),stat,NPS=',istat,nps,nairlinex  

 
       call psbeta3INIT
      
       
 !correct       PMMO(IPS)= 0.82
 !correct       PMalthft(IPS)=410.
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
 
       
!         call  ACNUMBPSbeta('B772',IPSexclude(1))
!         call  ACNUMBPSbeta('B77W',IPSexclude(2))
!         call  ACNUMBPSbeta('B788',IPSexclude(3))
!         call  ACNUMBPSbeta('B789',IPSexclude(4))
 
   IPSexclude=0
  
             CALL CPU_TIME(time0)
         
 ltest: do ITEST=1,5
 ltest1: if(itest.eq.1) then
! compute annual mean climbdata 

    
             
    do Iairline=1,Nairlinex
    do IPs=1,NPS
    
      climbdata(IPS,Iairline)%clrm=0.
      climbdata(IPS,Iairline)%a12=0.
      climbdata(IPS,Iairline)%a22=0.
      climbdata(IPS,Iairline)%b1=0.
      climbdata(IPS,Iairline)%b2=0.
      climbdata(IPS,Iairline)%x1=0.
      climbdata(IPS,Iairline)%x2=0.      
      climbdata(IPS,Iairline)%nclr=0
     
      end do
      end do
       nclimbdata=0

 ldoday1: do iday=1,365!365!260,262!262!262!262!1,2!5!1,1!1,1!2  
   call scdate(iday,cdate)
   
!   open(5,file='Results2/climbdatanew'//cdate,form='unformatted')
   open(5,file='climbdatanew'//cdate,form='unformatted')
   read(5)climbdataca
   close(5)
  
    
    do Iairline=1,Nairlinex
    do IPs=1,NPS
      climbdata(IPS,Iairline)%clrm=climbdata(IPS,Iairline)%clrm+ climbdataca(IPS,Iairline)%clrm
      climbdata(IPS,Iairline)%a12=climbdata(IPS,Iairline)%a12+ climbdataca(IPS,Iairline)%a12
      climbdata(IPS,Iairline)%a22=climbdata(IPS,Iairline)%a22+ climbdataca(IPS,Iairline)%a22
      climbdata(IPS,Iairline)%b1=climbdata(IPS,Iairline)%b1+ climbdataca(IPS,Iairline)%b1
      climbdata(IPS,Iairline)%b2=climbdata(IPS,Iairline)%b2+ climbdataca(IPS,Iairline)%b2
!      climbdata(IPS,Iairline)%x1=climbdata(IPS,Iairline)%x1+ climbdataca(IPS,Iairline)%x1
!      climbdata(IPS,Iairline)%x2=climbdata(IPS,Iairline)%x2+ climbdataca(IPS,Iairline)%x2     
      climbdata(IPS,Iairline)%nclr=climbdata(IPS,Iairline)%nclr+ climbdataca(IPS,Iairline)%nclr
     
   nclimbdata=nclimbdata+1
   end do
   end do
   print*,' climbdataca read for iday=',iday,nclimbdata,nclimbdata/(NPS*Nairlinex) ! = 
! climbdataca read for iday=         365     1259250         365
  end do ldoday1
 
 
    ips=0
    if(IPS.gt.0) then
    print*,' climbdataca read for itest=',itest
    do IPs=1,NPS
    print*,'IPS= ',picao(IPS),' nclr(IPS,Iairline)',(climbdata(IPS,Iairline)%nclr, Iairline=1,8)
     print*,'IPS= ',picao(IPS),' clrm(IPS,Iairline)',(climbdata(IPS,Iairline)%clrm, Iairline=1,8)
      print*,'IPS= ',picao(IPS),' a12(IPS,Iairline)',(climbdata(IPS,Iairline)%a12, Iairline=1,8)
     print*,'IPS= ',picao(IPS),' b1(IPS,Iairline)',(climbdata(IPS,Iairline)%b1, Iairline=1,8)
    end do
    stop
    end if
    
    

    
     do I=1,NPS
  do J=1, Nairlinex
  if (climbdata(I,J)%nclr.gt.0) then
  print*,' picao(i),j nclr,clrm', PICAO(I),J,climbdata(I,J)%nclr,climbdata(I,J)%clrm
  end if
  end do
  end do
        print*,'end for itest=', ITEST
 end if ltest1
   
   
   
   
 ltest2: if(itest.eq.2) then
! compute and save annual mean climbdata%x1 and x2 values
  
! test   BTESTmax=0.
 
     do Iairline=1,nairlinex
         do IPS=1,NPS
      da11=climbdata(IPS,Iairline)%nclr
      det= da11*climbdata(IPS,Iairline)%a22  &
      -climbdata(IPS,Iairline)%a12**2
      if(det.ne.0.) then 
      print*,'IPS,Iairline ',IPS,Iairline, ' det=',det
      if(det.lt.0.) stop
      else
      climbdata(IPS,Iairline)%nclr=0
      end if
 !    
   ldetget0: if(det.gt.0 ) then !.and. det.gt.0.01*climbdata(IPS,Iairline)%nclr*climbdata(IPS,Iairline)%a22) then
     climbdata(IPS,Iairline)%x1=   &
  (climbdata(IPS,Iairline)%b1*climbdata(IPS,Iairline)%a22  &
  -climbdata(IPS,Iairline)%b2*climbdata(IPS,Iairline)%a12)/det
   
    climbdata(IPS,Iairline)%x2=  &
  (climbdata(IPS,Iairline)%b2-climbdata(IPS,Iairline)%a12  &
  *climbdata(IPS,Iairline)%x1)/climbdata(IPS,Iairline)%a22
  
! test

!     btest1=     climbdata(IPS,Iairline)%nclr*climbdata(IPS,Iairline)%x1 &
!     + climbdata(IPS,Iairline)%a12*climbdata(IPS,Iairline)%x2 &
!     -climbdata(IPS,Iairline)%b1
!     btest1=btest1/det
     
!     btest2=     climbdata(IPS,Iairline)%a12*climbdata(IPS,Iairline)%x1 &
!     + climbdata(IPS,Iairline)%a22*climbdata(IPS,Iairline)%x2 &
!     -climbdata(IPS,Iairline)%b2
!     btest2=btest2/det
!     BTESTmax=max(btestmax,abs(Btest1),abs(btest2))
!     if(BTESTmax.gt. 1.e-4) then
!     PRINT*,'da11',da11
!     print*,' BTESTmax',BTESTmax
!     print*,' x1,x2',climbdata(IPS,Iairline)%x1,climbdata(IPS,Iairline)%x2
!     print*,' btest1',btest1
!     print*,' btest2',btest2
!     end if
     
   else ldetget0
      climbdata(IPS,Iairline)%x1=0.
      climbdata(IPS,Iairline)%x2=0.
    print*,'clr IPS',IPS,det,' ',picao(IPS),' Iairline=',Iairline
    end if ldetget0
!          print*,'IPS,climbdata(IPS,Iairline)%x1,climbdata(IPS,Iairline)%x2'  &
!,IPS,climbdata(IPS,Iairline)%x1,climbdataca(IPS,Iairline)%x2
      if(climbdata(IPS,Iairline)%nclr.lt.100) then
      climbdata(IPS,Iairline)%x1=0.
      climbdata(IPS,Iairline)%x2=0.
      print*,' climbdata set to zero because of too small nclr'
      end if
      
      
      end do
      end do
      
      
         print*
    do IPs=1,NPS
        print*,'IPS= ',picao(IPS),' nclr(IPS,Iairline)',(climbdata(IPS,Iairline)%nclr, Iairline=1,8)
     print*,'IPS= ',picao(IPS),' clrm(IPS,Iairline)',(climbdata(IPS,Iairline)%clrm, Iairline=1,8)
      print*,'IPS= ',picao(IPS),' a12(IPS,Iairline)',(climbdata(IPS,Iairline)%a12, Iairline=1,8)
      print*,'IPS= ',picao(IPS),' a22(IPS,Iairline)',(climbdata(IPS,Iairline)%a22, Iairline=1,8)
     print*,'IPS= ',picao(IPS),' b1(IPS,Iairline)',(climbdata(IPS,Iairline)%b1, Iairline=1,8)
    print*,'IPS= ',picao(IPS),' MTOM1(IPS,Iairline)',(PMTOM(Ips), Iairline=1,8)
    print*,'IPS= ',picao(IPS),' x1(IPS,Iairline)',(climbdata(IPS,Iairline)%x1, Iairline=1,8)
    print*,'IPS= ',picao(IPS),' x2(IPS,Iairline)',(climbdata(IPS,Iairline)%x2, Iairline=1,8)
       print*,'IPS= ',picao(IPS),' OEM(IPS,Iairline)',(POEM(IPS), Iairline=1,8)
    print*
    end do
  
 
    open(7,file='F:\FLLISTCout2/climbdata2',form='unformatted')
   write(7)climbdata
   close(7)
      
       
     do I=1,NPS
  do J=1, Nairlinex
  if (climbdata(I,J)%nclr.gt.0) then
  print*,' picao(i),j nclr,clrm',  &
  PICAO(I),J,climbdata(I,J)%x1,climbdata(I,J)%x2 &
  ,'nclr=',climbdata(I,J)%nclr
  end if
  end do
  end do
 ! Print*,'BTESTmax',BTESTmax
         print*,'end for itest=', ITEST
     end if ltest2
     
      
    ltest3: if(itest.eq.3) then
!  compute mean errors
!  if(lwrite7) then
!    open(5,file='F:\FLLISTCout2/climbdata2',form='unformatted',action='read')
!   read(5)climbdata
!   close(5)
!   end if
   
   meanerrora=0.
   nmeanerra=0
   
  
     call readdata(NW)
    
 
    
         loopinm3: do I=1,NW
         if(ICC(I).gt.0) then 
         nsumps(IPSC(I))=nsumps(IPSC(I))+1
         nsumac(jairline(I))=nsumac(jairline(I))+1
         
         do Imethod=1,M
         er=(massresult5(imethod,I)-TOW(I))/TOW(I)
        
     
          meanerrora(imethod,IPSC(I),jairline(I))= meanerrora(imethod,IPSC(I),jairline(I))+er
            nmeanerra(imethod,IPSC(I),jairline(I))=nmeanerra(imethod,IPSC(I),jairline(I))+1
         end do
         end if
         end do loopinm3
          
                    
      do I=1,NPS
      if(nsumps(I).gt.0) print*,' picao(IPS),nsumps ', picao(I),' ',nsumps(I)
      end do
      do I=1,nairlinex
      if(nsumac(I).gt.0) print*,' jairline,nsumac ', i,nsumac(I)
      end do
   
 
 do IAirline=1,Nairlinex
 do IPS=1,NPS
 do imethod=1,5
  if(nmeanerra(imethod,IPS,Iairline).gt.0) then
    meanerrora(imethod,IPS,Iairline)=meanerrora(imethod,IPS,Iairline)/nmeanerra(imethod,IPS,Iairline)
    end if
    end do
    end do
    end do
    
!    if(lwrite7) then
           open(7,file='F:\FLLISTCout2/ermeana.txt',form='unformatted')
           write(7)meanerrora,nmeanerra
           close(7)
!     end if
           
       
 

     print*,'end for itest=', ITEST
  end if ltest3
  
        

     
  
      ltest4: if(itest.eq.4) then
!  correct for mean errors, compute mean correction, and select ibestm
!   if(lwrite7) then
    open(5,file='F:\FLLISTCout2/climbdata2',form='unformatted',action='read') 
   read(5)climbdata
   close(5)
!   end if
   
!     if(lwrite7) then
!    open(5,file='F:\FLLISTCout2/ermeana.txt',form='unformatted')
!           read(5)meanerrora,nmeanerra
!           close(5)
!     end if
           
         
             
       err5c=0.
        massresult5c=0.
    
        quadraticerrorac=0.
        nmeanerrorac=0
        nmeanone=0
        
        
        meanerrorac=0.
    
       call readdata(NW)
       do J=1,M
       print*,'J,sum(imassvalid(J,1:NW)),NCMAX' &
       ,J,sum(imassvalid(J,1:NW)),NCMAX
       
       end do
    
    
    
        print*,'doiloop done, NW=', NW 
    
          loopinm4: do I=1,NW
          if(ICC(I).gt.0) then 
 !         print*,'I,icc(I).gt.0',ICC(I)
         
      
        
         do K=1,M
            massresult5c(K,I)= massresult5(K,I)/(1.-meanerrora(K,IPSc(I),Jairline(I)))
         massresult5c(K,I)=max(POEM(IPSc(I)),massresult5c(K,I))
         massresult5c(K,I)=min(PMTOM(IPSc(I)),massresult5c(K,I))
         err5c(K,I)=(massresult5c(K,I)-tow(I))/TOW(I)
         if(imassvalid(K,I).eq.0) then
         err5c(K,I)=0.
         else
        
       meanerrorac(K,ipsc(I),jairline(I))=meanerrorac(K,ipsc(I),jairline(I))+err5c(K,I)
       quadraticerrorac(K,ipsc(I),jairline(I))=quadraticerrorac(K,ipsc(I),jairline(I))+err5c(K,I)**2
       nmeanerrorac(K,ipsc(I),jairline(I))=nmeanerrorac(K,ipsc(I),jairline(I))+1
         nmeanone(ipsc(I),jairline(I))=nmeanone(ipsc(I),jairline(I))+1
!         print*,' test',ITEST
!         stop
        end if
        end do
     
        end if
           end do loopinm4        
     
      

  do IAirline=1,Nairlinex
   do IPS=1,NPS
   ermaxl=1.e30
   IBESTM(IPS,Iairline)=0
   Immm: do IMethod=1,M !???
   if(nmeanerrorac(Imethod,IPS,IAirline).gt.0) then
   meanerrorac(Imethod,ips,iairline)=meanerrorac(Imethod,ips,iairline)/nmeanerrorac(Imethod,IPS,IAirline)
   quadraticerrorac(Imethod,ips,iairline)=quadraticerrorac(Imethod,ips,iairline)/nmeanerrorac(Imethod,IPS,IAirline)
   quadraticerrorac(Imethod,ips,iairline)=sqrt(quadraticerrorac(Imethod,ips,iairline))
   er= quadraticerrorac(Imethod,IPS,IAirline)
   if(er.lt.ermaxl) then
   IBESTM(IPS,Iairline)=imethod
   ermaxl=er
   end if
   end if
!   print*,' ips,iairline,IBESTM(IPS,Iairline)',ips,iairline,IBESTM(IPS,Iairline)
!    if(IBESTM(IPS,Iairline).ne.1) stop
   end do Immm
   nbest(IBESTM(IPS,Iairline))=nbest(IBESTM(IPS,Iairline))+1
  
   
  
!!!!   IBESTM(IPS,Iairline)=1
   
   
   !!!!!!!!!??????????????????????????????????????????????????
   end do
   end do
 print*,'nbest(0:6) in list of airline/ips', nbest
     do IAirline=1,Nairlinex
   do IPS=1,NPS
   if(IBESTM(IPS,Iairline).gt.1) print*,' ibest for ',picao(IPS),Iairline
   end do
   end do
   print*,'after IBESTM(IPS,Iairline).gt.1'
!   stop
   
    do IAirline=1,Nairlinex
   do IPS=1,NPS
   if(nmeanone(IPs,IAIRLINE).gt.0) then
   if(IBESTM(IPS,Iairline).eq.0) then
       print*,'IBESTM(IPS,Iairline).eq.0',IBESTM(IPS,Iairline) &
       ,'iairline,ips=',ips,iairline
       print*,'nmeanerrorac(1:5,IPS,IAirline)',nmeanerrorac(1:M,IPS,IAirline)
       stop
       end if
   end if
   end do
   end do
   
       
!        IBESTM(IPS,Iairline)=1

         
  
 !        if(lwrite7) then
       open(7,file='Results2/ibestm.txt')
       do I=1,nairlinex
       write(7,'(100I2)') (ibestm(J,I),J=1,NPS)
       end do
       close(7)
 !      endif
    
       open(7,file='F:\FLLISTCout2/meanerrorac')
       do I=1,nairlinex
       do K=1,Nmethod
       write(7,'(100E15.7)') (meanerrorac(K,J,I),J=1,NPS)
       end do
       end do
       close(7)
       
     print*,'end for itest=', ITEST
  end if ltest4
  
     
     
           
         ltest5: if(itest.eq.5) then  
 ! test5:    use best results    
 
      if(lwrite7) then
       open(5,file='Results2/ibestm.txt',action='read')
       do I=1,nairlinex
       READ(5,'(100I2)') (ibestm(J,I),J=1,NPS)
       end do
       close(5)
       end if
       
    
  
   
!    do J=1,NAIrlinex
!    if(sum(ibestm(:,J)).gt.0) then
!    print*,'Iairline=',J,' ibestm',(ibestm(I,J),I=1,NPS)
!    end if
!    end do
    print*,' bestm done'
 
        errsum=0.
        errrms=0.
        ner=0
        erminl=1.e20
        ermaxl=-1
    ibestmall=0
           massxfinal=-99999.
          print*,'check Ibestm'
       
      
      
       call readdata(NW)
       nbest=0

        loopinm5: do I=1,NW    
        if(ICC(I).gt.0) then 
        
        


        
      if(IBESTM(IPSc(I),Jairline(I)).gt.0) then

!      lexclude: if( &
      lexclude: if(imassvalid(1,I).gt.0 .and. &
      IPSc(I).ne.IPSexclude(1).and. &
      IPSc(I).ne.IPSexclude(2).and. &
      IPSc(I).ne.IPSexclude(3).and. &
      IPSc(I).ne.IPSexclude(4)) then
      ibestmall(I)=IBESTM(IPSc(I),Jairline(I))
      nbest(IBESTM(IPSc(I),Jairline(I)))=nbest(IBESTM(IPSc(I),Jairline(I)))+1
      massxfinal(I)=massresult5c(IBESTM(IPSc(I),Jairline(I)),I)
!test      massxfinal(I)=massresult5c(1,I)
      massxfinal(I)=max(min(massxfinal(I),0.98*PMTOM(IPSC(I))),POEM(IPSC(I)))
      er=(massxfinal(I)-TOW(I))/TOW(I)
      errsum=errsum+er
      errrms=errrms+er**2
       ner=ner+1
      ermaxl=max(abs(er),ermaxl)
      erminl=min(abs(er),erminl)
!      else
!      print*,' IBESTM(IPSc(I),Jairline(I))=0',I,IPSc(I),Jairline(I),IBESTM(IPSc(I),Jairline(I))
    end if lexclude
   end if
        
 !    print*,' end of loopinm5',NER
     end if
            end do loopinm5
      
       I=sum(nbest(0:Nmethod))
       print*,'nbest(0:6) in output', nbest,' sum(nbest)=',sum(nbest(0:Nmethod))
       print*,'nbest(0:6) fractions', nbest/float(I),' sum(nbest)=',sum(nbest(0:Nmethod))/float(I)
       print*,' ner', ner
       if(ner.eq.0) stop
   
       
       errsum=errsum/ner
       errrms=errrms/ner
       errrms=sqrt(errrms)
       print*,'NCMAX,ner, errsum,errrms',NCMAX,ner, errsum,errrms
       print*,'erminl,ermaxl',erminl,ermaxl
       print*,'Ner,nw',ner,nw
           stop !!???
       OPen(7,file='Results2/R6_bestof.txt')
       write(7,'(a10,2a12,a10,a3,a8)') 'Flight_id ','mass/kg','TOM/kg','error','ib','IC'
       do I=1, NCMAX
       if(massxfinal(I).le.1.) then
 !      print*,'I,I,massxfinal(I)',I,I,massxfinal(I)
       else
       er=(massxfinal(I)-TOW(I))/TOW(I)
       end if
       write(7,'(I10,2F12.0,F10.4,I3,I8)') flight_id(I), massxfinal(I),TOW(I),er,ibestmall(I),I
            if(er**2.gt.0.3) then
      print*,'er**2>0.3: I, massxfinal,TOW,er,bestm',I, massxfinal(I),TOW(I),er &
      ,ibestmall(I),' ',PIcao(IPSC(I)),' ',Jairline(I),'er**2= ',er**2   &
      ,'Max=',PMTOM(IPSC(I)),POEM(IPSC(I))
      end if
       
       end do
       print*,' R6_bestof.txt done'
       close(7)
       
   
    print*,' final result of first 5 methods: er,ner',er,ner
  
    
    print*,'final end for itest=', ITEST
    end if ltest5
    
    print*,' Itest=',ITEST
    
    end do  ltest
   
   
          
 deallocate(climbdataca,stat=istat) 
 Print*,' DEALLOCATE: climbdataca=',istat  
 deallocate(climbdata,stat=istat) 
 Print*,' DEALLOCATE: climbdata=',istat  
 print*,' check climbdata'
 print*,'check Ibestm'
 
   end PROGRAM  main
  
  
      
                          
  subroutine scdate(iday,cdate)
       USE mo_directories,ONLY: JDAYJ0
  IMPLICIT NONE
  character*10,intent(out):: cdate
  INTEGER,INTENT(in)::iday
  INTEGER IY,IMONTH,IDAYM
       call monday(JDAYJ0+iday,IY,IMONTH,IDAYM)
       write(cdate,'(I4,A1,I2.2,A1,I2.2)')IY,'-',IMONTH,'-',IDAYM
 !      print*,'scdate: cdate',cdate
  end subroutine scdate
       
  subroutine readdata(NW)
! read data and computes mass fromClimb rate
  USE mo_directories,ONLY: imassvalid,atyp,ICC,IPSC,climbdata &
  ,massresult5,jairline,clrout,nclrout & 
  ,err5c,flmax,altmax,TOW,NCMAX,M,nmethod,flight_id
 
  IMPLICIT NONE
!  INCLUDE "CPARA.inc"
!  INCLUDE "psbeta3.inc"
  character*10:: cdate
  INTEGER,INTENT(OUT)::NW
  INTEGER I,J,IDAY
  character*4 ATYPI
  INTEGER flight_idI,ICCI,IPSCI,jairlineI ,imassvalidi(5),nclroutI
  real massresult5i(5),clroutI,TOWI,err5ci(5),flmaxI,altmaxI
  real errormax
     Print*,' M,nmethod',M,nmethod
  
 NW=0
 jairline=0
 ICC=0
 errormax=0
 flight_id=0
 
  ATYP='none'
  ICC=1
  IPSC=1
 
  jairline=1
  massresult5=1.e5
  clrout=0
  nclrout=0
  TOW=1.01e5
  err5c=1.e-2
  flmax=30000.
  altmax=35000.
  imassvalid=0
 ldoday3: do iday=1,365!!260,262!262!262!262!1,2!5!1,1!1,1!2
     call scdate(iday,cdate)
!  print*,' before' ,'F:\FLLISTCout2/outmass_'//cdate//'.txt'
 
  OPen(5,file='F:\FLLISTCoutnew/outmass_'//cdate//'.txt',action='read') 
   read(5,*)
!  print*,' opened','F:\FLLISTCout2/outmass_'//cdate//'.txt'
  massresult5(4,:)=0.
        doiloop: do J=1,NCMAX
  
    read(5, &
    '(I10,1x,a4,1x,I10,2i3,1x,5I2,5f10.0,f10.1,I10,F10.0,5f10.5,2F10.0)' &
    ,end=999) &
     flight_idI,ATYPI,ICCI,IPSCI,jairlineI &
     ,imassvalidi(:),massresult5i(1:5),clroutI,nclroutI,TOWI &
      ,err5ci(1:5),flmaxI,altmaxI
           
!!! test !!! ???jairlineI=1
!!!    jairlineI=1  
 !!!! if(iday.lt.260 .or. iday.gt.262) imassvalidi=0 ! ??? for test
      errormax=max(errormax, maxval(abs(err5ci(1:5))))
      if(maxval(abs(err5ci(1:5))).gt.1.1) then
      print*,' maxval(abs(err5ci(1:5))).gt.1.1',err5ci
!      stop
      end if
  I=ICCI
   if(I.eq.0) then 
  print*,'ICCI=0',ICCI
  stop
  end if
  flight_id(I)=flight_idI
  ATYP(I)=ATYPI
  ICC(I)=ICCI
  IPSC(I)=IPSCI
  if(ipsci.eq.0) then 
  print*,'ipsci=0',ipsci
  stop
  end if
  jairline(I)=jairlineI
  imassvalid(:,I)=imassvalidi(1:5)
!  print*,' J,I,imassvalidi',J,I,imassvalidi,'massvalid=',imassvalid(:,I)
  
  massresult5(1:M,I)=massresult5i(1:5)
  clrout(I)=clroutI/nclroutI
!!! ist diese division durch nclr ok?
  nclrout(I)=nclroutI
  TOW(I)=TOWI
  err5c(1:5,I)=err5ci(1:5)
  flmax(I)=flmaxI
  altmax(I)=altmaxI
 
     
       NW=NW+1
       end do doiloop
   999 continue   
       close(5)
       end do ldoday3
       
       err5c=0
   do I=1,NW
   if(ICC(I).gt.0) then 
    if(jairline(I).gt.0) then
   massresult5(4,I)=climbdata(IPSC(I),jairline(I))%x1+clrout(I)*climbdata(IPSC(I),jairline(I))%x2
   if( climbdata(IPSC(I),jairline(I))%x2.lt.0. ) then
   imassvalid(4,I)=1
   err5c(4,I)=(massresult5(4,I)-TOW(I))/TOW(I)
   else
   imassvalid(4,I)=0
   err5c(4,I)=0.
   end if
  
   end if
   end if
!!!!     imassvalid(4,I)=0 !!???? temporary
     !!!???? temporary
   end do
   
    
        print*,'readdata: doiloop done, NW,errormax=', NW,errormax
   
        end subroutine readdata
        