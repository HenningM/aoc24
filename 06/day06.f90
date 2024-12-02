program day06
  implicit none
  character(len=256) :: line
  integer :: ios

  do
     read(*,'(A)', iostat=ios) line

     if (ios == -1) then
       exit
     end if

     print *, trim(adjustl(line))
  end do
end program day06
