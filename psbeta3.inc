       INTEGER,Parameter:: NColumns=48 ! NPS=69
       character*16 ptitle(NColumns)
       character*4 PICAO(NPS)
       character*12 PManufacturer(NPS)
       character*12 PType(NPS)
       character*4 LASTNAME
       INTEGER ipslast
       INTEGER,dimension(NPS)::Pwinglets,PWV,PYear_FF,Pnoengines
       real,dimension(NPS)::PMTOM,PMLM,POEM,PMFM,PMalthft& 
      ,PMPM,PMZFM,PSwingarea & 
      ,Pspan,bf_m,Pdelta2,Pcossweep,PAR,PPSI0,PXref & 
      ,Pwingconstant,PJ2,PJ1,CLo,etaL_Do,POPR,PBPR,FPR & 
      ,PF00,mfmaxTO_kg_s,PFFIdle,PMdes,PCT & 
      ,Peta1,Peta2,Mec,Tec,TETmaxclimb,TETmaxt_o & 
      ,etaPC,PMMO,Vcasasl,pimaxPa,pinfcoPa,PMOM
!
       common/PSCo/PMTOM,PMLM,POEM,PMFM,PMalthft& 
      ,PMPM,PMZFM,PSwingarea & 
      ,Pspan,bf_m,Pdelta2,Pcossweep,PAR,PPSI0,PXref & 
      ,Pwingconstant,PJ2,PJ1,CLo,etaL_Do,POPR,PBPR,FPR & 
      ,PF00,mfmaxTO_kg_s,PFFIdle,PMdes,PCT & 
      ,Peta1,Peta2,Mec,Tec,TETmaxclimb,TETmaxt_o & 
      ,etaPC,PMMO,Vcasasl,pimaxPa,pinfcoPa & 
      ,Pwinglets,PWV,PYear_FF,Pnoengines,PMOM,ipslast
       common/PSCC/ptitle,PManufacturer,PType,PICAO ,LASTNAME