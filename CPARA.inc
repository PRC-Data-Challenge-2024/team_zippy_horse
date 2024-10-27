! constant  parameters  
      INTEGER ISEMLRATIO
      PARAMETER (ISEMLRATIO=0)   !  unimportant?  
      REAL R0,R1,CP,G,P0,PI,PIR180,RADIUS,RHOICE,CPICE,HLATENTI,FEXTSW & 
      ,GAMMA,LCV,betaT ,TISA,ZZZS
      real sigmam
      PARAMETER(sigmam=2000.)
      PARAMETER(CP=1004.,G=9.80665,P0=101325.,GAMMA=1.4)
      PARAMETER(PI=3.141592653589793,PIR180=1.745329251994330E-02 & 
      ,RADIUS=6371229.)
      Parameter (RHOICE=917.,CPICE=2050.,HLATENTI=2800.E3 & 
      ,FEXTSW=7.08286238E6,LCV=43.e6,betaT=0.0065)  
!      ! beta T = -(mean tropospheri! lapse rate)
!      AK=1.31 ! real refractive index
!      AL=0.55E-6 ! wave length / m
!      FEXTSW/LW=4.*pi*(AK-1.)/AL mit AL=ALSW/LW
      real EIH2O,QFUEL,RVOLZUREFF
      common/cpara/EIH2O,QFUEL,RVOLZUREFF
      PARAMETER(R0=287.05) !  from WMO 
      PARAMETER(R1=461.51) ! from WMO 23 
! World Meteorological Organization, 2011 
! General Meteorological Standards and Recommended Practices. (Updated in 2012), 
! Technical Regulations, Volume I, Appendix A (WMO-No. 49), Geneva
! http://library.wmo.int/pmb_ged/wmo_49-v1-2012_en.pdf
      REAL TKSL,HTP,TKTP,PTP
      PARAMETER(TKSL=288.15,HTP=11000.,TKTP=TKSL-HTP*betaT)
      PARAMETER(PTP= 22631.7009)  
      LOGICAL LHSPEED
      parameter(LHSPEED=.true.)
!       Saturation pressure over liquid water (Sonntag, 1994)
      REAL PSAT,PICE,TTS
      PSAT(TTS)=100.*EXP(-(6096.9385/TTS)+16.635794-2.711193E-2*TTS &
      +1.673952E-5*TTS*TTS+2.433502*ALOG(TTS))
!       Saturation pressure over ice water (Sonntag, 1994)
      PICE(TTS)=100.*EXP(-(6024.5282/TTS)+24.7219+1.0613868E-2*TTS &            
     -1.3198825E-5*TTS*TTS-0.49382577*ALOG(TTS)) 
      real QMIE,RFACTSS
      QMIE(RFACTSS)=2.-(4./RFACTSS)*(sin(RFACTSS)-(1.-COS(RFACTSS))/RFACTSS)
      TISA(ZZZS)=max(TKSL-betaT*ZZZS,TKTP)