!**********************************************************************
!
!	Usage $extract_basin [dir_file_name] [ip0] [jp0]
!	dir_file_name : file name for dir data set
!	ip0 : "i index" for the downstream edge point
!	jp0 : "j index" for the downstream edge point
!
!**********************************************************************
	program extract_basin
	use m_common
	implicit none
	integer :: i, j
	character(len=125)::buf, buf0
	integer :: isw
	
	integer :: ip0, jp0
	integer :: iarg
	character(len=1024) :: fname0	!for input
	character(len=1024) :: fname1	!for output
	integer :: ni1, nj1
	integer :: imin, imax, jmin, jmax
	real*8  :: xl1, yl1

!==================================================
	!default value set
	ip0 = 2780; jp0 = 1304
	fname0 = "dir.asc"

!--------------------------------------------------
!get args
!--------------------------------------------------
	iarg = iargc()
	if(iarg==3)then					!for gfortran
		call getarg(1, fname0)			!for gfortran
		call getarg(2, buf)			!for gfortran
		read (buf,*) ip0
		call getarg(3, buf)			!for gfortran
		read (buf,*) jp0
	else
		write(*,*) "Usage of extract_basin is as follow"
		write(*,*) "$extract_basin [dir_file_name] [ip0] [jp0]"
		stop
	endif

!--------------------------------------------------
! NODATA_valueの指定有無チェック
!--------------------------------------------------
	open(1, file=trim(fname0))
	read(1,*) buf, buf
	read(1,*) buf, buf
	read(1,*) buf, buf
	read(1,*) buf, buf
	read(1,*) buf, buf
	read(1,*) buf0, buf
	close(1)
	
	if(buf0(1:12) == "NODATA_value")then
		!NODATA_valueの指定あり
		isw = 1
	else
		!NODATA_valueの指定なし
		isw = -1
	end if
	
!--------------------------------------------------
! ファイル読込開始
!--------------------------------------------------
	write(*,*) ""
	write(*,*) "1.start reading DIR data."
	write(*,*) ""
	open(1, file=trim(fname0))
	!open(1, file="data.asc")
	read(1,*) buf, nj
	read(1,*) buf, ni
	read(1,*) buf, xl
	read(1,*) buf, yl
	read(1,*) buf, csize
	if(isw == 1) read(1,*) buf, buf		!NODATA_valueの指定ありのみ
	
	allocate(idir(1:ni, 1:nj))

	do i=1, ni
		read(1,*) (idir(i,j), j=1, nj)
		!write(*,*) i
 	end do
	close(1)
	write(*,*) ""
	write(*,*) "... end."
	write(*,*) ""
	
	
	fname1 = trim(fname0)//"_basin.txt"
!--------------------------------------------------
!DIRデータを利用した流域抽出
!--------------------------------------------------
	write(*,*) ""
	write(*,*) "2.start extracting BASIN from DIR data."
	write(*,*) ""
	allocate(ibasin(1:ni, 1:nj))
	ibasin = -9999
	!write(*,*) ip0, jp0
	
	if( (idir(ip0,jp0) == 1)       &
		.or. (idir(ip0,jp0) == 2)	 &
		.or. (idir(ip0,jp0) == 4)  &
		.or. (idir(ip0,jp0) == 8)  &
		.or. (idir(ip0,jp0) == 16) &
		.or. (idir(ip0,jp0) == 32) &
		.or. (idir(ip0,jp0) == 64) &
		.or. (idir(ip0,jp0) == 128) &
		.or. (idir(ip0,jp0) == 0) &
		.or. (idir(ip0,jp0) == 255) &
		.or. (idir(ip0,jp0) == -1) &
	)then
		!write(*,*) ip0, jp0
		ibasin(ip0,jp0) = 1
		call find_basin(ip0, jp0)
		
	else
		write(*,*) "That point does not have any data."
		stop
	end if
	write(*,*) ""
	write(*,*) "... end."
	write(*,*) ""
	
!--------------------------------------------------
!データ整形　→　必要箇所のみ抽出
!--------------------------------------------------
	write(*,*) ""
	write(*,*) "3.start fixing DATA shape."
	write(*,*) ""
	imin = ni; imax = 1
	jmin = nj; jmax = 1
	do i=1,ni
		do j=1, nj
			if(ibasin(i,j) == 1 )then
				if(i < imin) imin = i
				if(i > imax) imax = i
				if(j < jmin) jmin = j
				if(j > jmax) jmax = j
			end if
		end do
	end do

	ni1 = imax - imin + 1
	nj1 = jmax - jmin + 1
	xl1 = xl + csize*(jmin-1)
	yl1 = yl + csize*(ni - imax)
	write(*,*) ""
	write(*,*) "... end."
	write(*,*) ""
	
!--------------------------------------------------
!結果出力
!--------------------------------------------------
	write(*,*) ""
	write(*,*) "4.start outputting DATA."
	write(*,*) ""
	open(1, file=trim(fname1))
	write(1,"(a5, i10)") "ncols", nj1
	write(1,"(a5, i10)") "nrows", ni1
	write(1,"(a9, f20.10)") "xllcorner", xl1
	write(1,"(a9, f20.10)") "yllcorner", yl1
	write(1,"(a8, f20.10)") "cellsize", csize
	write(1,"(a12, i10)") "NODATA_value", -9999
	do i=imin, imax
		write(1,'(10000(i6))') (ibasin(i,j), j=jmin, jmax)
	end do
	close(1)
	
!--------------------------------------------------
!Debug　出力	
!--------------------------------------------------
	!open(2, file="dir_chk.txt")
	!write(2,"(a5, i10)") "ncols", nj
	!write(2,"(a5, i10)") "nrows", ni
	!write(2,"(a9, f20.10)") "xllcorner", xl
	!write(2,"(a9, f20.10)") "yllcorner", yl
	!write(2,"(a8, f20.10)") "cellsize", csize
	!write(2,"(a12, i10)") "NODATA_value", -9999
	!do i=1, ni
	!	!write(2,'(10000(i6))') (idir(i,j), j=1, nj)
	!end do
	!close(2)
	
	write(*,*) ""
	write(*,*) "... end."
	write(*,*) ""
	write(*,*) "normal ends."
	
	end program


	
	
	