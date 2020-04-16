!**********************************************************************
!
!	!DIRデータを利用した流域抽出
!
!**********************************************************************
	recursive subroutine find_basin(ip0, jp0)
	use m_common
	implicit none
	integer, intent(in) :: ip0, jp0
	integer :: ip, jp
!==================================================	  
! --------> j
! |              6 4	  
! |         32----|----128
! |     16--------|--------1	  
! |          8----|----2
! |               4
! ↓
! i
	
	!id = 1
	ip = ip0; jp = jp0+1
	if((ip > 1 .and. ip < ni) .and. (jp > 1 .and. jp < nj)) then
		if(idir(ip,jp) == 16) then	
			!ibasin(ip,jp) = 16
			ibasin(ip,jp) = 1
			call find_basin(ip, jp)
		end if
	end if
		
	!id = 2
	ip = ip0+1; jp = jp0+1
	if((ip > 1 .and. ip < ni) .and. (jp > 1 .and. jp < nj)) then
		if(idir(ip,jp) == 32) then
			!ibasin(ip,jp) = 32
			ibasin(ip,jp) = 1
			call find_basin(ip, jp)
		end if
	end if
	  
	!id = 4
	ip = ip0+1; jp = jp0
	if((ip > 1 .and. ip < ni) .and. (jp > 1 .and. jp < nj)) then
		if(idir(ip,jp) == 64) then
			!ibasin(ip,jp) = 64
			ibasin(ip,jp) = 1
			call find_basin(ip, jp)
		end if
	end if
		
	!id = 8
	ip = ip0+1; jp = jp0-1
	if((ip > 1 .and. ip < ni) .and. (jp > 1 .and. jp < nj)) then
		if(idir(ip,jp) == 128) then
			!ibasin(ip,jp) = 128
			ibasin(ip,jp) = 1
			call find_basin(ip, jp)
		end if
	end if
	!id = 16
	ip = ip0; jp = jp0-1
	if((ip > 1 .and. ip < ni) .and. (jp > 1 .and. jp < nj)) then
		if(idir(ip,jp) == 1) then
			ibasin(ip,jp) = 1
			call find_basin(ip, jp)
		end if
	end if
	!id = 32
	ip = ip0-1; jp = jp0-1
	if((ip > 1 .and. ip < ni) .and. (jp > 1 .and. jp < nj)) then
		if(idir(ip,jp) == 2) then
			!ibasin(ip,jp) = 2
			ibasin(ip,jp) = 1
			call find_basin(ip, jp)
		end if
	end if
	!id = 64
	ip = ip0-1; jp = jp0
	if((ip > 1 .and. ip < ni) .and. (jp > 1 .and. jp < nj)) then
		if(idir(ip,jp) == 4) then
			!ibasin(ip,jp) = 4
			ibasin(ip,jp) = 1
			call find_basin(ip, jp)
		end if
	end if
		
	!id = 128
	ip = ip0-1; jp = jp0+1
	if((ip > 1 .and. ip < ni) .and. (jp > 1 .and. jp < nj)) then
		if(idir(ip,jp) == 8) then
			!ibasin(ip,jp) = 8
			ibasin(ip,jp) = 1
			call find_basin(ip, jp)
		end if
	end if
	return
	end subroutine