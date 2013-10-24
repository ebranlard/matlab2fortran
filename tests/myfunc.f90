!> 
subroutine myfunc(x, z)
implicit none 
! Use somemodule 
real*8 :: temp1      !>  
real*8, intent(in) :: x      !>  
real*8, dimension(:), intent(out) :: z      !>  
! 
! Doc example. Chapter 4. 
! $Revision: 1.1 $ 
temp1 = x .* 10 .* sin(x)
z = anint(temp1)
end subroutine  myfunc 
