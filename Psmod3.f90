  
       SUBROUTINE psbeta3INIT
       USE mo_directories,ONLY: NPS
       use mo_cocip_parameters, only: LCVCC
       implicit none
       INTEGER I,J
         INCLUDE "psbeta3.inc"
       INCLUDE "CPARA.inc"
       real fuelflow_per_meter
       real p1,p2  
       real MZFM_MTOM,OEM_MTOM,MPM_MTOM,etafactor
       LOGICAL,PARAMETER:: lcorrection=.true.
      open(5,file='INPUT/PSoct2024.txt',action='read')
       read(5,*)
    
    
!   MFM = minimum flight mass This is the lowest weight at which the aircraft can be safely controlled. It is a bit lower than the OEM (the operational empty mass)

!Aircraft for which I have found MFM in the type certificate data sheet

!B788 (Boeing 787-800) 104100 (kg) (MFM/MLM=0.604)
!B789 (Boeing 787-900) 110677 (kg) (MFM/MLM=0.574)
!B78X (Boeing 787-1000) 110677 (kg) (MFM/MLM=0.548)

!E170 (Embraer EMB170LR) 21800 (kg) (MFM/MLM=0.665)
!E290 (Embraer E190-E2) 32700 (kg) (MFM/MLM=0.667)
!E295 (Embraer E195-E2) 34700 (kg) (MFM/MLM=0.643)

!BCS1 (Airbus A220-100) 34927 (MFM/MLM=0.661)
!BCS3 (Airbus A220-300) 36287 (MFM/MLM=0.595)

!The mean relationship is MFM/MLM=0.58

!I double checked the MTOM for the E195 and E290 

!MTOM E195 is 56400 kgs
!MTOM E290 is now 62500 kgs ( changed this year)

 PMOM=0.


        do I=1,NPS
          read(5,*) J,PICAO(I), PMTOM(I),PMLM(I),POEM(I),PMFM(I),PMalthft(I) & 
         ,PMPM(I),PMZFM(I),PSwingarea(I) & 
         ,Pspan(I),bf_m(I),Pdelta2(I),Pcossweep(I),PAR(I),PPSI0(I),PXref(I) & 
         ,Pwingconstant(I),PJ2(I),PJ1(I),CLo(I),etaL_Do(I),POPR(I),PBPR(I),FPR(I) & 
         ,PF00(I),mfmaxTO_kg_s(I),PFFIdle(I),PMdes(I),PCT(I) & 
         ,Peta1(I),Peta2(I),Mec(I),Tec(I),TETmaxclimb(I),TETmaxt_o(I) & 
         ,etaPC(I),PMMO(I),Vcasasl(I),pimaxPa(I),pinfcoPa(I) & 
         ,Pwinglets(I),PWV(I),PYear_FF(I),Pnoengines(I),ipslast &
         ,PManufacturer(I),PType(I)
          PMOM(I)=PMFM(I)
         print*,'read for ' ,PIcao(I)
         
         if(PICAO(I).eq.'B788') then
                   PMOM (I)=104100.
                    PMLM(I)=172365.
         end if
        if(PICAO(I).eq.'B789') then
                   PMOM (I)=110677.
         end if
        if(PICAO(I).eq.'B78X') then
                   PMOM (I)=110677.
         end if
        if(PICAO(I).eq.'E170') then
                   PMOM (I)=21800.
         end if
        if(PICAO(I).eq.'E290') then
                   PMOM (I)=32700.
         end if
        if(PICAO(I).eq.'E295') then
                   PMOM (I)=34700.
         end if
         
!BCS1 (Airbus A220-100) 34927 (MFM/MLM=0.661)
!BCS3 (Airbus A220-300) 36287 (MFM/MLM=0.595)

        if(PICAO(I).eq.'BCS1') then
                   PMOM (I)=34927.
         end if
        if(PICAO(I).eq.'BCS3') then
                   PMOM (I)=36287.
         end if
       
         
              if(PICAO(I).eq.'E195') then
    
                 PMTOM(I)=56400.
                 endif
                 if(PICAO(I).eq.'E290') then
                 PMTOM(I)=62500.
                 endif
         
!MTOM E195 is 56400 kgs
!MTOM E290 is now 62500 kgs ( changed this year)



         if(PICAO(I).eq.'B788') then
         etaL_Do(I)=etaL_Do(I)*1.05
         Peta1(I)=Peta1(I)*1.05
          end if
         if(PICAO(I).eq.'B789') then
         etaL_Do(I)=etaL_Do(I)*1.1
         Peta1(I)=Peta1(I)*1.1
         end if
         if(PICAO(I).eq.'B763') then
         PMTOM(I)=187333.
         
!         PMZFM(I) =129955±3%
!         PMLM(I) =140614±3%,  
!         POEM= 87856±3%, 
!         PMPM=41122±6.5%, 
!         PMTOM(I)min =156490 and (MTOM)max=186880. 
          
         PMZFM(I) =129955.
         PMLM(I) =140614
         POEM(I)= 87856 
         PMPM(I)=41122. 
         PMTOM(I)=186880.
         
         
         end if
         
            
         if(PICAO(I).eq.'C56X') then
         PMTOM(I)=9222.
         PMLM(I)=8482.
         POEM(I)=5625
         PMZFM(I)=6804
         Pspan(I)=16.97
         PSwingarea(I)=34.4
         etaL_Do(I)=5.5
          end if
          
                if(picao(I).eq.'B772' &
       .or. picao(I).eq.'B77W' &
       .or. picao(I).eq.'B788'  &
       .or. picao(I).eq.'B789' ) then
           etafactor=1.1
      etaL_Do(I)=etaL_Do(I)*etafactor
      Peta1(I)=Peta1(I)*etafactor
      end if
      

!    if(PICAO(I).eq.'B772') then ! does not improve the load factor estimate
!      PMTOM(I)= 297556. !kg, 
!      PMLM(I)= 213188. !kg, 
!      PMZFM(I)= 200487. !kg
!     end if

          if(lcorrection) then
!    
!    
!    3  A310            2           0  minac>1,maxac<1:   0.838952959      0.580284715
     if(PICAO(I).eq.'A310')POEM(I)=POEM(I)*0.838952959
     if(PICAO(I).eq.'A310')PMTOM(I)=PMTOM(I)*0.84
!    6  A319        17578        7496  minac>1,maxac<1:    1.12706149      0.886261463
!    7  A320        79761       34210  minac>1,maxac<1:    1.08635426      0.985179484
!    8  A321        29423       12396  minac>1,maxac<1:    1.10202956      0.948470592
!    9  A332         2241         942  minac>1,maxac<1:    1.15828156      0.899184525
!   10  A333        17011        7305  minac>1,maxac<1:    1.12010169       1.03642488
     if(PICAO(I).eq.'A333')PMTOM(I)=PMTOM(I)*1.03642488
!   12  A343          480         218  minac>1,maxac<1:    1.16212606      0.945620239
!   15  A359         1725         779  minac>1,maxac<1:    1.06191182      0.988439262
!   23  B737         3627        1530  minac>1,maxac<1:    1.05684233       1.05265415
    if(PICAO(I).eq.'B737')PMTOM(I)=PMTOM(I)* 1.05265415
!   24  B738        37495       16318  minac>1,maxac<1:    1.05580378      0.997468889
!   25  B739         1793         798  minac>1,maxac<1:    1.15822458      0.961921096
!   30  B752            6           4  minac>1,maxac<1:    1.48435020      0.823553801
!   33  B763         1190         496  minac>1,maxac<1:    1.09886634      0.998560548
!   36  B772         8322        3543  minac>1,maxac<1:    1.10784638       1.00212264
    if(PICAO(I).eq.'B772')PMTOM(I)=PMTOM(I)*1.00212264
!   37  B77W         9162        3954  minac>1,maxac<1:    1.07872295      0.999408305
!   38  B773            1           2  minac>1,maxac<1:    1.37549901      0.731927037
!   39  B788         5960        2551  minac>1,maxac<1:   0.988900006      0.996757805
    if(PICAO(I).eq.'B788')POEM(I)=POEM(I)*0.988900006 
!   40  B789         5734        2522  minac>1,maxac<1:   0.989395320      0.991911829
    if(PICAO(I).eq.'B789')POEM(I)=POEM(I)*0.989395320 
!   44  E195        24914       10456  minac>1,maxac<1:    1.10857141       1.32725966
    if(PICAO(I).eq.'E195')PMTOM(I)=PMTOM(I)* 1.32725966
!   48  CRJ9        22926        9920  minac>1,maxac<1:    1.04765391      0.989146590
!   52  A20N        37944       16301  minac>1,maxac<1:    1.07139587      0.973670900
!   53  A21N        23257        9901  minac>1,maxac<1:    1.17169261      0.987948477
!   55  B38M        12330        5066  minac>1,maxac<1:    1.04569721      0.989971459
!   56  B39M          542         240  minac>1,maxac<1:    1.29066074      0.925730944
!   57  BCS1         4649        2090  minac>1,maxac<1:    1.08825839      0.892829120
!   58  BCS3        11936        5211  minac>1,maxac<1:   0.987881124      0.900911212
    if(PICAO(I).eq.'BCS3')POEM(I)=POEM(I)*0.987881124
!   63  E190         2105         921  minac>1,maxac<1:    1.06996417       1.00309694
    if(PICAO(I).eq.'E190')PMTOM(I)=PMTOM(I)*  1.00309694
!   66  E290            1           2  minac>1,maxac<1:    1.94963861       1.14765954
    if(PICAO(I).eq.'E290')PMTOM(I)=PMTOM(I)*  1.14765954
!   68  AT76         6897        2977  minac>1,maxac<1:    1.14865494      0.997565210
!   69  C56X            1           0  minac>1,maxac<1:    9.23164463       5.63088274
    if(PICAO(I).eq.'C56X')PMTOM(I)=PMTOM(I)* 5.63088274
  
         end if

         end do
         
         
         print*,'read nps=',nps
         do I=1,NPS
         print*,I,' PICAO ',PICAO(I)
         end do
         print*,'NPS',NPS
       
           close(5)
    
! 
       end SUBROUTINE  psbeta3INIT
       
   
       real FUNCTION HICAOPA(PP)      
      IMPLICIT NONE
      real,INTeNT(IN):: PP
!   Altitude in ICAO Standard Atmosphere 
!  INPUT: PP Pressure in Pa
!  output: HFT Altitude in m
!   U. Schumann 4.4.09,changed 4 Oct 2021
      IF(PP .GT. 22631.7009) THEN
      HICAOPA=(1.-(PP/101325.)**0.190261200307954)*44330.7692307692
      ELSE
      HICAOPA=(11000.-LOG(PP/22631.7009)*6341.55216103358)
      END IF 
      END FUNCTION HICAOPA

      REAL FUNCTION PICAOPA(Z)
      IMPLICIT NONE
!   Pressure in ICAO Standard Atmosphere
!   valid for Troposphere and Stratosphere
!  INPUT: Z Altitude in m
!  output: PICAOPA Pressure in Pa
!   U. Schumann 6.12.2008,changed 4 Oct 2021
      real,INTeNT(IN):: z
      IF(Z .LT. 11000.) THEN
      PICAOPA=101325.*(1.-(0.0000225576956446295*Z))**5.25593236235981
      ELSE
      PICAOPA=22631.7009*EXP((-0.000157690100878554)*(Z-11000.))
      END IF
      END FUNCTION PICAOPA
       

       
      subroutine psbeta3(IPS,mass, mach,FL, dhdt,dvdt,DTISA,ff,eta & 
       ,etaLD,CL,CD,massxCL,massxCT)
!  program for psbeta3
! Model from Ian Poll, March 2023
! coded by Ulrich Schumann, version 1,  12 April 2023

! INPUT: IPS,mass, mach,FL, climbrate,acc,DTISA
!
! IPS = Index of aircraft type in Input table
! mass  = aircraft mass in kg
! Mach  = Mach number, nondimensional ! mass  = aircraft mass in kg
! FL  = flight level in hft 
! climbrate  = dh/dt in m/s
! acc  = dv/dt in m/s2
! DTISA = temperature difference between ambient air and ISA standard atmosphere in K

! OUTPUT: ff,eta,cl,cd,etaLD
!
! ff = fuel flow rate in kg/s
! eta = overall propulsion efficiency 
! etaLD = eta *  L/D
! CL= lift coefficient
! CD = drag coefficient
! massxCL ,massxCT = masses from CL or CT
     USE mo_directories,ONLY: NPS
     use mo_cocip_parameters, only: R0,g,LCVCC,cpkero,etacombustion &
     ,PI,GAMMA,cp,cpe_cp
     USE mo_directories,ONLY: NPS

       IMPLICIT NONE
       INCLUDE "psbeta3.inc"
        INTEGER,INTENT(IN):: IPS
       real,INTENT(IN):: mass,mach,FL,dhdt,dvdt,DTISA
       real,INTENT(out):: ff,eta,etaLD,CL,CD,massxCL,massxCT
       real temp,h
        real PICAOPA
 
       real amue,tmg,TAS
       real Ca17,Cb17,Cc17,Cd17,Ce17,Cf17,Cg17,Ch17,Ci17,Cj17,Ck17,Cl17 & 
      ,Cm17,Cn17,Co17,Cp17,Cq17,Cr17,Cs17,Ct17,Cu17,Cv17,Cw17,Cx17,Cy17 & 
      ,Cz17
      real Ba17,Bb17,Bc17,Bd17,Be17,Bf17,Bg17,Bh17,Bi17,Bj17,Bk17,Bl17 & 
      ,Bm17,Bn17,Bo17,Bp17,Bq17,Br17,Bs17,Bt17,Bu17,Bv17,Bw17,Bx17,By17 & 
      ,Bz17
      real aa17,Ab17,Ac17,Ad17,Ae17,Af17,Ag17,Ah17,Ai17,Aj17,Ak17,Al17 & 
      ,Am17,An17,Ao17,Ap17,Aq17,Ar17,As17,At17,Au17,Av17,Aw17,Ax17,Ay17 & 
      ,Az17
      real e17,f17,g17,p,asound,amue,n17 & 
       ,o17,p17,r17,s17,t17,u17,v17,w17,x17,y17,z17
       real z, TISA
       INTEGER ibe171
         TISA(z)=max( 216.65,288.15-0.0065*z)
          if(IPS.le.0.or. IPS.gt.NPS) then
      print*,'psbeta3: ips.eq.0, IPS,NPS', IPS,NPS
      stop
      end if
      h=FL*30.48
  
       temp=TISA(h)+dtisa
 !      Print*,' mass,mach,FL,dhdt,dvdt,dtisa' & 
 !     ,mass,mach,FL,dhdt,dvdt,dtisa
    

 
       p=PICAOPA(h) ! p
 !      Print*,'p',p
 
       asound=sqrt(GAMMA*R0*Temp) ! a
 !      Print*,'asound',asound
   
       amue=0.000001458*(TEMP**1.5)/(110.4+TEMP) ! amue
 !      Print*,'amue',amue
       TAS=mach*asound ! TAS
    
 !      Print*,'TAS',TAS
       R17=dvdt/g ! dvdt/g
 !      Print*,'r17',r17
       S17=dhdt/TAS ! sin(theta)
 !      Print*,'s17',s17
       T17= 1.-S17**2 ! cos(theta)
 !      Print*,'t17',t17
       U17=S17/T17 !tan(theta)
 !      Print*,'u17',u17
       V17=ASIN(S17)*180./PI ! theta
 !      Print*,'v17',v17
!      sin(𝜃)	cos(𝜃)	tan(𝜃)	𝜃 (degs)
   !  $P$5=PSwingarea(IPS)

      Z17=(mass*g*T17)/(0.5*GAMMA*p*mach*mach*PSwingarea(IPS)) ! CL
!      Print*,'Z17',Z17
      AA17= (sqrt(PSwingarea(IPS))*mach)*(p/amue)*sqrt(GAMMA/(R0*Temp)) ! R Reynoldszahl
!      Print*,'AA17',AA17
      AB17=0.0269/(AA17**0.14)! Cf
!      Print*,'Ab17',Ab17
 ! $W$5 = PPSI0
      AC17 =PPSI0(IPS)*AB17 ! Cd0
!      Print*,'Ac17',Ac17
  ! $T$5= Pcossweep(IPS)
      AD17=0.8*(1.-0.53*Pcossweep(IPS))*AC17 ! = k1
!      Print*,'Ad17',Ad17
 !    $S$5= Pdelta2(IPS)
 ! *$U$5=PAR(IPS)
 ! *$U$5 = winglets
      if(Pwinglets(IPS).eq.0) then
      AE17=(1./((1.+0.03+Pdelta2(IPS))+AD17*PI*PAR(IPS))) ! eLS
      else
      AE17=(1.075/((1.+0.03+Pdelta2(IPS))+AD17*PI*PAR(IPS)))  ! eLS
      end if
!      Print*,'AE17',AE17
      AF17= 1./(PI*PAR(IPS)*AE17) ! K
 !      Print*,'AF17',AF17
!      $Y$5 = Pwingconstant(IPS)
      AG17 =Pwingconstant(IPS)-0.1*(Z17/Pcossweep(IPS)**2)! = Mcc
 !      Print*,'AG17',AG17
      AH17=mach*Pcossweep(IPS)/AG17 ! = X
!      Print*,'AH17',AH17
 ! $AA$5= PJ1(IPS)
 ! $Z$5 = PJ2(IPS)
 !=WENN(AH17.LT.$Z$5;0;WENN(AH17.LT.$X$5;($T$5**3)*$AA$5*((AH17-$Z$5)**2);($T$5**3)*$AA$5*((AH17-$Z$5)**2)+40*((AH17-$X$5)**4)))
 ! $X$5 = PXref(I)
      if(AH17.LT.PJ2(IPS)) then
       AI17=0. ! Cdw
       else 
       if(AH17.lt. PXref(IPS)) then
      AI17=(Pcossweep(IPS)**3)*PJ1(IPS)*((AH17-PJ2(IPS))**2) ! Cdw
      else
      AI17= (Pcossweep(IPS)**3)*PJ1(IPs)*((AH17-PJ2(IPs))**2) & 
       +40.*((AH17-PXref(IPS))**4) ! cdw
       end if
      end if
!      Print*,'AI17',AI17
      AK17=AC17+AF17*Z17*Z17+AI17 ! Cd, total drag
 !      Print*,'AK17',AK17
      AL17 = Z17/AK17 ! = L/D
 !      Print*,'AL17',AL17
      
      AN17 =(T17/AL17)+S17+R17 != Fn/mg
 !      Print*,'AN17',AN17
      AO17=AN17*(Mass*g)/(0.5*GAMMA*p*mach*mach*PSwingarea(IPS))
 !      Print*,'AO17',AO17
!       $AK$5    = PMdes(IPS) Mach design??
       AP17=mach/PMdes(IPS)  
 !      Print*,'AP17',AP17
       if(mach.LT.0.4) then
       AQ17=1.3*(0.4-mach) ! SIGMA
       else
       AQ17=0 ! SIGMA
       end if
 !      Print*,'AQ17',AQ17
       AR17=AQ17-0.43! SIGMA-THETA
  !      Print*,'AR17',AR17
       AS17= AQ17*0.43! SIGMA*THETA
   !      Print*,'AS17',AS17
       ! $AN$5= Peta2(IpS)
       AT17=AP17**Peta2(IPS)! h1
 !      Print*,'AT17',AT17
 ! $AM$5 = Peta1(IPS)
       AU17=AT17*Peta1(IPS)*PMdes(IPS)** Peta2(IpS)  ! $AN$5 eta_OB
  !      Print*,'AU17',AU17
  ! $AK$5=  PMdes(IPS
       AV17=((1.+0.55*mach)/(1.+0.55* PMdes(IPS))/(AP17**2))! h2
  !      Print*,'AV17',AV17
 !    $AL$5 = PCT(IPS)  
       AW17=AV17* PCT(IPS)  ! (CT)𝜂B
  !      Print*,'AW17',AW17
       AX17 =AO17/AW17!= CT/(CT)𝜂B
  !      Print*,'AX17',AX17
!       AY17 ! =WENN(AX17.LT.0.3
! ;10*(1.+0.8*AR17-0.6027*AS17)*AX17+33.3333*(-1.-0.97*AR17+0.8281*AS17)*(AX17**2)+37.037*(1.+AR17-0.9163*AS17)*(AX17**3)
! ;(1.+AR17-AS17)+(-2*AR17+4*AS17)*AX17+(AR17-6*AS17)*(AX17**2)+4*AS17*(AX17**3)-AS17*(AX17**4))
 ! =WENN(AX17.LT.0.3
 !;10*(1+0.8*AR17-0.6027*AS17)*AX17+33.3333*(-1-0.97*AR17+0.8281*AS17)*(AX17^2)+37.037*(1+AR17-0.9163*AS17)*(AX17^3)
 !;(1+AR17-AS17)+(-2*AR17+4*AS17)*AX17+(AR17-6*AS17)*(AX17^2)+4*AS17*(AX17^3)-AS17*(AX17^4))
      if(AX17.LT.0.3) then
  ! AY17 = 𝜂o/(𝜂o)𝜂B
       AY17=10.*(1.+0.8*AR17-0.6027*AS17)*AX17+33.3333 & 
       *(-1.-0.97*AR17+0.8281*AS17)*(AX17**2)+37.037 & 
       *(1.+AR17-0.9163*AS17)*(AX17**3)
       else
        AY17=(1.+AR17-AS17)+(-2.*AR17+4.*AS17)*AX17+(AR17-6.*AS17) & 
       *(AX17**2)+4.*AS17*(AX17**3)-AS17*(AX17**4)
       end if
  !      Print*,'AY17',AY17
  ! AZ17
       if(AX17.LT.0.3) THEN
       AZ17=6.56*(1.+0.8244*AQ17)*AX17-19.43*(1.+1.053*AQ17)*(AX17**2)+21.11*(1.+1.063*AQ17)*(AX17**3)
       else
       AZ17= (1.-0.43*((AX17-1.)**2))*((1.+AQ17*((AX17-1.)**2)))
      end if
 !      Print*,'AZ17',AZ17
      BA17=AY17*AU17 ! eta_o
   !      Print*,'BA17 eta_o =0.305, with AY',BA17
      BA17=AY17*AU17 ! eta_o
 !      Print*,'BA17;  eta_o =0.305, with AZ',BA17
      ! $bb$14 =0.975
      BB17=0.975*BA17! in service corrected 𝜂o
 !      Print*,'BB17',BB17
      BD17=BB17*AL17! (𝜂oL/D)
      !      Print*,'BD17, (𝜂oL/D)',BD17
 ! $AJ$5=PFFIdle(IPS) !nominal((mf)FI)SLS (kg/s) =0.22
!      BE17=WENN(0.5*GAMMA*(AO17*(mach**3)/BB17)*(p*asound*PSwingarea(IPS)/LCVCC).LT.$AJ$5*(1.-0.178*(FL/100)+0.0085*((FL/100)**2));$AJ$5*(1.-0.178*(FL/100)+0.0085*((FL/100)**2));0.7*(AO17*(mach**3)/BB17)*(p*asound*PSwingarea(IPS)/LCVCC)) ! mf/((kg/s)
      ibe171=0
      if(0.5*GAMMA*(AO17*(mach**3)/BB17)*(p*asound*PSwingarea(IPS)/LCVCC).LT. & 
       PFFIdle(IPS)*(1.-0.178*(FL*0.01) & 
       +0.0085*((FL*0.01)**2)))THEN
      BE17=PFFIdle(IPS)*(1.-0.178*(FL*0.01) & 
       +0.0085*((FL*0.01)**2))
        ibe171=2. ! idle
      else
      BE17=0.5*GAMMA*(AO17*(mach**3)/BB17)*(p*asound*PSwingarea(IPS)/LCVCC)
      ibe171=1
      end if
      !      Print*,'BE17,  mf (kg/s)=0.608 kg/s?',BE17
      BF17=BE17/TAS! mf in kg per m
      !      Print*,'BF17',BF17
      
 ! engine state
      
      BH17= 0.6*(1.+(2./3.)*AX17) ! TR
      !      Print*,'BH17',BH17
 ! $AP$5= TEC(IPS)
 ! $AO$5= MEC(IPS)
 !  $AF$5*=PBPR(IPS)
 ! $PAR$5
 !$PAR$5= TETmaxt_o(IPS) =((TET)maxt/o)/(Tsl)isa

      BI17 = 0.6*TEC(IPS)*(1.-0.53*((mach-MEC(IPS))**2)) & 
         *(1.+((2./3.)*AX17))!      (T0)4/(T0)2
      !      Print*,'BI17',BI17
      BK17=  PBPR(IPS)*(TETmaxt_o(IPS)**1.13)/(BI17**1.13) !   PBPR
      !      Print*,'BK17',BK17
      
  ! $AE$5= nominla POPR = POPR(IPS)
  ! =$AG$5 =nominal FPR  = FPR(IPS)

      BL17= (POPR(IPS)/(TETmaxt_o(IPS)**2.5))*(BI17**2.5)
      !      Print*,'BL17',BL17
      BM17=(FPR(IPS)/TETmaxt_o(IPS))*BI17
      !      Print*,'BM17',BM17
 !     $AS$5= (𝜂)PC = etaPC(IPS)

      BN17=BL17**(0.4/(GAMMA*etaPC(IPS)))
      !      Print*,'BN17',BN17
      
      
      BO17=BM17**(0.4/(GAMMA*etaPC(IPS))) ! (T0)13/(T0)2
      !      Print*,'BO17',BO17
      
      BP17 =BI17-(cp/1244.)*(BK17*(BO17-1.)+(BN17-1.)) ! (T0)5/(T0)2
      !      Print*,'BP17',BP17

      BQ17 =(1.+0.2*mach*mach)*Temp! (T0)2 (K)
      !      Print*,'BQ17',BQ17
      
      BR17 = BN17*BQ17
      !      Print*,'BR17',BR17
      
      BS17=BI17*BQ17 ! T0 4
      !      Print*,'BS17',BS17
      
      BT17=BP17*BQ17 ! T05
      !      Print*,'BT17',BT17
      
      BV17 =((1.+0.2*mach*mach)**3.5)*p ! (p0)2 (Pa)
      !      Print*,'BV17',BV17
      
     
      BW17 = BL17*BV17 ! 751231 = (p0)3 (Pa)
      !      Print*,'BW17',BW17
      
      ! $BY$10 = cpe/cp = 1.2378
      ! LCVCC= LCVCC
      ! $BY$9 = cp
      ! $BY$10=cpe/cp
      ! $BY$12=cpkero/cp
      ! $BY$14= etacombustion=0.99

      
      BY17 =((cpe_cp*(BI17-(298./BQ17))-(BN17-(298./BQ17)))/((LCVCC/ & 
       (cp*BQ17))-cpe_cp*(BI17-(298./BQ17))+cpkero/cp & 
       *(1.-(298./BQ17)))) & 
       /etacombustion ! = FTAR =0.0203
     
      !      Print*,'BY17',BY17
     
      ! = TRmax= (CT)max/(CT)𝜂B
      ! $CC$12=1.2
      
      
 !     CA17=WENN(AO17.gt.(Z17*((1./AL17)+(1.524/TAS)));AO17;(Z17*((1./AL17)+(1.524/TAS))))
       if(AO17.gt.(Z17*((1./AL17)+(1.524/TAS)))) then
       CA17=AO17
       else
       CA17=Z17*((1./AL17)+(1.524/TAS))
       end if
 !      Print*,' CA17',  CA17 
      
 !     $AQ$5= TET max climb = 1529 = TETmaxclimb(IPS)
      
      CC17 =1.2*( TETmaxclimb(IPS)/Temp)/(TEC(IPS)* & 
       (1.-0.53*((mach-MEC(IPS))**2))*(1.+0.2*mach*mach)) ! = 1.337

 !      Print*,' CC17',  CC17 
      
! CD17 = (CT)max/(CT)𝜂B
      CD17= 1.+2.5*(CC17-1.)  ! = 1.844
  !      Print*,' CD17',  CD17 
      
       CE17 = CD17*AW17 ! (CT)avail = 0.0639
  !      Print*,' CE17',  CE17 
      
      !CF17 = (CT)avail/(CT)req
      CF17=CE17/CA17
 !      Print*,' CF17',  CF17 
      
      
  !    clmax= (Z17*((1./AL17)+(1.524/TAS))
  
 !      CI17=(Z17*((1./AL17)+(1.524/TAS))   ! 1.024
       
       CI17 =sqrt(((CE17-AC17-AI17)/AF17)+(((S17+R17)/(2.*AF17))**2)) & 
       -((S17+R17)/(2.*AF17))
!      Print*,' CI17',  CI17 
     
      CJ17=CI17*(0.5*GAMMA*p*mach*mach)*PSwingarea(IPS)/g
  !      Print*,' CJ17',  CJ17 
      massxCT=CJ17
       
       
       CL17=mach/PMdes(IPS) ! = 1 = Mach/PMdes(IPS)
  !      Print*,' CL17',  CL17 
       
       ! $AB$5   = (CL)do = $AB$5  =clo(IPS)  ?? 
       
       
 !      cm17= =WENN(CL17.LT.0.5*GAMMA;1.8+0.16*CL17-1.085*(CL17**2);13.272-42.262*CL17+49.883*(CL17**2)-19.683*(CL17**3))
       if(CL17.LT.0.5*GAMMA) then
       cm17=1.8+0.16*CL17-1.085*(CL17**2)
       else
       cm17=13.272-42.262*CL17+49.883*(CL17**2)-19.683*(CL17**3)
       end if
  !      Print*,' cm17',  cm17 

       cn17=CM17*clo(IPS)
  !      Print*,' cn17',  cn17 
       co17=Z17/CN17
  !      Print*,' co17', co17
!        if(CO17.gt.1.) print*,' CL not achievable'
       cr17=CN17*(0.5*GAMMA*p*mach*mach)*PSwingarea(IPS)/g
       massxcl=CR17
  !      Print*,' cr17',  cr17 
        
 ! $AV$5 = PMalthft(IPS)
! $AY$5 = pinfcoPa(IPS)
! $AX$5=  pimaxPa(IPS)
        ct17=FL/PMalthft(IPS)
        if(ct17.gt.1.)    print*,' FL not achievable'
        
!        CW17=WENN(FL.LT.100;sqrt(2)*sqrt(sqrt(1+(2/GAMMA)*(10510/p))-1);WENN(p.GT.$AY$5;sqrt(2)*sqrt(sqrt(1+(2/GAMMA)*($AX$5/p))-1);$AV$5))
      if(FL.LT.100.) THEN
      CW17=sqrt(2.*sqrt(1.+(2./GAMMA)*(10510./p))-1.)
      else
      if(p.GT.pinfcoPa(IPS)) then
      CW17=sqrt(2.*sqrt(1.+(2./GAMMA)*(pimaxPa(IPS)/p))-1.)
      else
      CW17= PMalthft(IPS)
      end if
      end if 
      if(cw17.lt.1.)    print*,' Mach not achievable'
 
    
       FF=BE17
       Tmg=AN17
      eta = max(0.,Tmg*mass*g*TAS/(ff*LCVCC))
      etaLD=mass*g*Tas/(FF*LCVCC)
!      if(etald.gt.15..and. ibe171.eq.1.and. dhdt.ge.0.) then
!      Print*,'etald',etald
!      print*,' mass,g,tas,lcvcc,ff,ibe171', mass,g,tas,lcvcc,ff,ibe171
!      print*,' IPS,mass, mach,FL, dhdt,dvdt,DTISA',IPS,mass, mach,FL, dhdt,dvdt,DTISA
!      stop
   
!      end if
      cl=Z17
      cd=AK17
      
      
      
 !     Cy17=mach/$AK$5
 !    CY17=mach/PMdes(IPS)
 !     Cz17==1-43.363*((CY17-1)^3)-4.9728*((CY17-1)^2)-1.2027*(CY17-1)
 !     DA17=CZ17*CLo(IPS)
 !     DC17=DA17*0.5*gamma*PICAOPA(h) *mach**2*PSwingarea(IPS)/g
      
      
 !      print*,' end of psbeta3, eta,etald,ff,cl,cd,eta*cl/cd' & 
 !      ,eta,etald,ff,cl,cd,eta*cl/cd
       end subroutine  psbeta3
       
      
      SUBROUTINE ACNUMBPSbeta(atyp,IPS)
      USE mo_directories,ONLY: NPS
! finds Index IPS of model coefficients in Common CPSbeta for given aircraft atyp 
      IMPLICIT NONE     
      character*4,INTENT(IN):: atyp ! ICAO type
      INTEGER,INTENT(OUT):: IPS
      INTEGER I
      INCLUDE "psbeta3.inc"
      IPS=0
      do I=1,NPS
      if(atyp.eq.PICAO(I)) THEN
      IPS=I
      exit
      end if
      end do
      end SUBROUTINE ACNUMBPSbeta
       

 
      
       