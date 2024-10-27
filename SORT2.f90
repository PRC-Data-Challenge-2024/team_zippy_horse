
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
      END SUBROUTINE  SORT2
!  (C) Copr. 1986-92 Numerical Recipes Software 2.02
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       
       