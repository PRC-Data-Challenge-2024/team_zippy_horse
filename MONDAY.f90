      SUBROUTINE DAYJ(IYEAR,IMONTH,IDAYM,IDAYY)
      IMPLICIT NONE
! INPUT IYEAR: year
! INPUT: IMONTH,IDAYM
! INPUT: IDAYM
! note IDAYY = 1 -> 1 Jan 2000
! output IDAYY: day of year since 2000                    
      INTEGER,INTENT(IN):: IYEAR,IMONTH,IDAYM
      INTEGER,INTENT(OUT):: IDAYY
      INTEGER iadd(2000:2032)/0,366,731,1096,1461,1827,2192,2557,2922 &
      ,3288,3653,4018,4383,4749,5114,5479,5844,6210,6575,6940,7305 &
      ,7671,8036,8401,8766,9132,9497,9862,10227,10593,10958,11323,11688/
      INTEGER NDAY(12)/0,31,59,90,120,151,181,212,243,273,304,334/
      INTEGER NDAYj(12)/0,31,60,91,121,152,182,213,244,274,305,335/
      if(mod(IYEAR,4).eq.0) THEN
      IDAYY=ndayj(IMONTH)+IDAYM+iadd(IYEAR)
      else
      IDAYY=nday(IMONTH)+IDAYM+iadd(IYEAR)
      end if
      end 
      
      SUBROUTINE MONDAY(JDAYJ,IY,IMONTH,IDAYM)
! input JDAYJ: day of year since 2000
! OUTPUT: IY, year
! output: IY,IMONTH,IDAYM
      IMPLICIT NONE
      INTEGER,intent(IN):: JDAYJ
      INTEGER,INTENT(OUT):: IY,IMONTH,IDAYM
      INTEGER I,JFEB,JDAY,IFOUND,J
      INTEGER iadd(2000:2032)/0,366,731,1096,1461,1827,2192,2557,2922 &
     ,3288,3653,4018,4383,4749,5114,5479,5844,6210,6575,6940,7305 &
     ,7671,8036,8401,8766,9132,9497,9862,10227,10593,10958,11323,11688/
      IFOUND=0
      IY=0
      IMONTH=0
      IDAYM=0
      JDAY=0
      DO I=2000,2031                                                               
      if(JDAYJ.gt.iadd(I).and.JDAYJ.le.iadd(I+1)) THEN ! corrected Oct 2020
      IFOUND=1
      IY=I
      JDAY=JDAYJ-IADD(I)
      exit
      end if
      end do
      if(IFOUND.eq.0) THEN
      print*,'MONDAY: JDAYJ out of range', iadd(2000),iadd(2020)
      print*,' stop in monday'
      stop
      end if
      JFEB=28
      if(MOD(IY,4).eq.0) JFEB=29
! alle ganzzahlig durch vier teilbaren Jahre sind Schaltjahre, 
! mit Ausnahme der Jahrhunderte, die nicht durch 400 teilbar sind.
!      IF(IY.EQ.2004)JFEB=29
!      IF(IY.EQ.2008)JFEB=29
!      IF(IY.EQ.2012)JFEB=29
!      IF(IY.EQ.2016)JFEB=29
!      IF(IY.EQ.2020)JFEB=29
!      IF(IY.EQ.2024)JFEB=29
!      IF(IY.EQ.2028)JFEB=29
!      IF(IY.EQ.2032)JFEB=29
      J=JDAY
      IF(J.le.31) THEN
      I=1
      IDAYM=J
 !      GOTO 999
      ELSE IF(J.le.31+JFEB) THEN
      I=2
      IDAYM=J-31
 !      GOTO 999
      ELSE IF(J.le.62+JFEB) THEN
      I=3
      IDAYM=J-(31+JFEB)
 !      GOTO 999
      ELSE IF(J.le.92+JFEB) THEN
      I=4
      IDAYM=J-(62+JFEB)
 !      GOTO 999
      ELSE IF(J.le.123+JFEB) THEN
      I=5
      IDAYM=J-(92+JFEB)
 !      GOTO 999
      ELSE IF(J.le.153+JFEB) THEN
      I=6
      IDAYM=J-(123+JFEB)
 !      GOTO 999
      ELSE IF(J.le.184+JFEB) THEN
      IDAYM=J-(153+JFEB)
      I=7
 !      GOTO 999
      ELSE IF(J.le.215+JFEB) THEN
      IDAYM=J-(184+JFEB)
      I=8
 !      GOTO 999
      ELSE IF(J.le.245+JFEB) THEN
      IDAYM=J-(215+JFEB)
      I=9
 !      GOTO 999
      ELSE IF(J.le.276+JFEB) THEN
      IDAYM=J-(245+JFEB)
      I=10
 !      GOTO 999
      ELSE IF(J.le.306+JFEB) THEN
      IDAYM=J-(276+JFEB)
      I=11
 !      GOTO 999
      ELSE IF(J.le.337+JFEB) THEN
      IDAYM=J-(306+JFEB)
      I=12
 !      GOTO 999
      ELSE 
      PRINT*,' JDAY.gt. 335/336,JDAY=', JDAY
      print*,' stop in monday'
      STOP
      END IF
  999 CONTINUE
      IMONTH=I
      END
      
      
      subroutine syyyymmddhhmmss(itim,fout)
      IMPLICIT NONE
      character*(*) Fout
      INTEGER ITIM,N,I
      INTEGER ITIML,iday,year,month,daym,hours,minutes,seconds
      N=len(FOUT)
      ITIML=ITIM
      iday=ITIML/(24*3600)
      ITIML=(ITIML-(24*3600)*iday)
      hours=ITIML/3600.
      minutes=(ITIML-hours*3600)/60
      seconds=ITIML-hours*3600-minutes*60
      iday=iday+1
      call MONDAY(iday,year,month,daym)
      if(N.ge.14) THEN
      write(FOUT,'(I4,5i2)') year,month,daym,hours,minutes,seconds
      else if(N.ge.10) THEN
      write(FOUT,'(I4,3i2)') year,month,daym,hours
      else if(N.ge.8) THEN
      write(FOUT,'(I4,2i2)') year,month,daym
      else  if(N.ge.6) THEN
      write(FOUT,'(I4,i2)') year,month
      else  if(N.ge.4) THEN
      write(FOUT,'(I4)') year
      else
      print*, 'syyyymmddhhmmss: character string too short', N
      end if
      do I=5,N-1,2
      if(FOUT(I:I).eq.' ')FOUT(I:I)='0'
      end do       
      end       
