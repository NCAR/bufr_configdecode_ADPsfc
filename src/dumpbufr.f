      program test_BUFR
      parameter (mxmn = 10)
      parameter (mxlv = 255)
c     real*8 r8arr(mxmn,mxlv)
      real*8 r8bfms
      parameter (mxbf=32000)
      character cbfmsg*(mxbf), csubset*8, inf*32
      integer   ibfmsg(mxbf/4)
      logical msgok
      PARAMETER ( MXR8PM = 10 )
      PARAMETER ( MXR8LV = 255 )
      PARAMETER ( MXR8VN = 10 )
      PARAMETER ( MXR8VT = 6 )
      PARAMETER ( MXSTRL = 80 )
      REAL*8 hdr(MXR8PM)
c     REAL*8 evns(MXR8PM,MXR8LV,MXR8VN,MXR8VT)
      equivalence (cbfmsg(1:4), ibfmsg(1))
c
c     COMMON  / PREPBC /      hdr, evns, nlev
      r8bfms=10.0E10
c
      write(*,*) 'enter input BUFR file?'
      read(*,'(a)') inf
      open(unit=11,file=inf,form='unformatted')
      open(unit=21,file='dumpbufr.out')
      call openbf(11,'IN',11)
      call datelen(10)           ! for date: YYYYMMDDHH
      do while (.true.)
        call readns(11,csubset,idate,ierr)
        write(*,*)' idate: ',idate,'  ',csubset
        call ufdump(11,21)
        if(ierr.eq.-1) then
          write(*,*) '....All records read, exit.'
          call closbf(11)
          go to 9998
        endif
        msgok=.true.
      enddo
 9998 continue
      stop
      end
