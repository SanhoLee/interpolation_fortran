PROGRAM INTERPOLATION
	implicit none
 
  ! parameter declaration
  integer, parameter :: SIZE = 100
  integer :: cnt,i
  double precision, dimension(SIZE,SIZE) :: x
  double precision :: r2_lstsq,r2_2nd,r2_log
  character(100) :: read_file="read_file.txt", write_file="write_file.txt"
  
  !initialize variables
  cnt = 0
  i=0
  r2_lstsq = 0.0
  r2_2nd = 0.0
  r2_log = 0.0
  
  call check_lines_getarr(read_file,x,cnt)
  call lst_sq(x,cnt,SIZE,r2_lstsq)
  !call lst_sq_2nd(x,cnt,SIZE,r2_2nd) 
  !call lst_sq_log(x,cnt,SIZE,r2_log)


  ! find maximum r2 value among each calculations.


END PROGRAM INTERPOLATION

subroutine check_lines_getarr(filename,tarray,cnt)
!read how many lines it has
!it returns number of lines,
!           get array, 
!           final line number == > cnt  
  implicit none
  character(100) :: filename
  integer :: cnt,i
  integer,parameter::SIZE=100
  double precision, dimension(SIZE,SIZE) :: tarray
  
  !initialize variables
  cnt = 0
  i = 0
  !file read
  open(10, file=filename)
  ! read line one by one, and count numbers
  do
    read(10,*,END=990) tarray(cnt+1,1),tarray(cnt+1,2)
    cnt = cnt + 1
  enddo
  990 continue
  ! check read data and print out
  write(*,'(a,a20)') 'read file : ',filename
  write(*,'(a,i2)') 'number of line : ',cnt
  close(10)
end subroutine check_lines_getarr

subroutine fy_val(a,b,xt,fy)
  ! calculating fy = a*xt + b
  implicit none
  double precision :: a,b
  double precision :: fy
  double precision :: xt
  ! if variables initialized, all variable will be 0 although variables is considered.
  fy=a*xt+b
end subroutine fy_val

subroutine lst_sq(tarray,cnt,SIZE,r2)
  ! calculating method of least squre, base on y= ax + b
  ! error will be ==>  sigma(yi-f(x))^2
  implicit none
  double precision :: r2, a, b
  double precision :: sig_x,sig_y,sig_xy,sig_x2
  double precision :: SSR, SST,fy,y_mean
  integer :: SIZE
  integer :: i,cnt
  double precision, dimension(SIZE,SIZE):: tarray
  !initialize variables
  r2 = 0.0
  a = 0.0
  b = 0.0
  sig_x = 0.0
  sig_y = 0.0     
  sig_xy = 0.0
  sig_x2 = 0.0
  SSR = 0.0
  SST = 0.0
  fy = 0.0
  y_mean = 0.0
  !calculating sigma variables
  do i=1, cnt
    sig_x = sig_x + tarray(i,1)
    sig_y = sig_y + tarray(i,2)
    sig_xy = sig_xy + tarray(i,1)*tarray(i,2)
    sig_x2 = sig_x2 + tarray(i,1)**2
  enddo
  ! calculating parameter a and b
  a = (cnt*sig_xy-sig_x*sig_y)/(cnt*sig_x2-sig_x**2)
  b = (sig_x2*sig_y-sig_x*sig_xy)/(cnt*sig_x2-sig_x**2)
  y_mean = sig_y / cnt

  print *, 'This is a method for 1st degree linear equation.'
  ! check a and b value
  write(*,90) "a","b"
  write(*,100) a, b
  90 format(2(2x,a7))
  100 format(2(2x,f7.2))
  ! check y_mean value
  write(*,"(a12,1x,f5.2)") "y_mean is : ",y_mean
  ! calculating r2=1-(SSR/SST)
  do i=1, cnt
    call fy_val(a,b,tarray(i,1),fy)
    SSR = SSR + ( tarray(i,2) - fy )**2
    SST = SST + ( tarray(i,2) - y_mean )**2
  enddo
  r2 = 1 - ( SSR / SST )
  ! check SSR, SST, r^2 values
  write(*,"(a10,f10.4)") "SSR : ", SSR
  write(*,"(a10,f10.4)") "SST : ", SST
  write(*,"(a10,f10.3)") "r^2 : ", r2
  print *, 'end calculation 1st degree linear equation!'
end subroutine lst_sq

subroutine lst_sq_2nd(tarray,cnt,SIZE,r2)
  ! 2nd degree polynomial equation
  ! y = ax**2+bx+c
end subroutine lst_sq_2nd

subroutine lst_sq_log(tarray,cnt,SIZE,r2)
  ! log equation for least squared error
  ! y = alogx + b
end subroutine lst_sq_log
