PROGRAM INTERPOLATION
	implicit none
 
  ! parameter declaration
  integer, parameter :: SIZE = 100
  integer :: cnt
  double precision, dimension(SIZE) :: x,y
  double precision :: r2_lstsq
  character(100) :: read_file="read_file.txt", write_file="write_file.txt"
  
  !initialize variables
  cnt = 0
  r2_lstsq = 0.0
  

  !file open
  open(10, file=read_file)
  open(20, file=write_file)
  
  call check_num_lines(read_file, cnt)
  call lst_sq(read_file,cnt,r2_lstsq)
END PROGRAM INTERPOLATION

subroutine check_num_lines(filename,cnt)
!read how many lines it has
!it returns number of lines, final line number == > cnt
  implicit none
  character(100) :: filename
  real :: temp
  integer :: cnt
  
  !initialize variables
  cnt = 0
  temp = 0.0

  !file read
  open(10, file=filename)

  ! read line one by one, and count numbers
  do
    read(10,*,END=990) temp
    cnt = cnt + 1
  enddo

  990 continue
  close(10)
end subroutine check_num_lines

subroutine fy_val(a,b,xt,fy)
  implicit none
  double precision :: a,b, xt
  double precision :: fy
  ! if variables initialized, all variable will be 0 although variables is considered.
  fy=a*xt+b
end subroutine fy_val

subroutine lst_sq(filename,num_lines,r2)
  implicit none
  ! calculating method of least squre, base on y= ax + b
  ! error will be ==>  sigma(yi-f(x))^2
  character(100) :: filename
  double precision :: r2, a, b
  double precision :: sig_x,sig_y,sig_xy,sig_x2
  double precision :: SSR, SST,fy,y_mean
  integer :: num_lines
  integer :: i
  double precision, dimension(num_lines)::x,y

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

  !read data from the file
  open(10, file=filename)
  do i=1,num_lines
    read(10,*) x(i),y(i)
  enddo

  !calculating sigma variables
  do i=1, num_lines
    sig_x = sig_x + x(i)
    sig_y = sig_y + y(i)
    sig_xy = sig_xy + x(i)*y(i)
    sig_x2 = sig_x2 + x(i)**2
  enddo

  ! calculating parameter a and b
  a = (num_lines*sig_xy-sig_x*sig_y)/(num_lines*sig_x2-sig_x**2)
  b = (sig_x2*sig_y-sig_x*sig_xy)/(num_lines*sig_x2-sig_x**2)
  y_mean = sum(y) / num_lines

  ! check a and b value
  write(*,90) "a","b"
  write(*,100) a, b
  90 format(2(2x,a7))
  100 format(2(2x,f7.2))
  ! check y_mean value
  write(*,"(a12,1x,f5.2)") "y_mean is : ",y_mean

  ! calculating r2=1-(SSR/SST)
  do i=1, num_lines
    call fy_val(a,b,x(i),fy)
    SSR = SSR + ( y(i) - fy )**2
    SST = SST + ( y(i) - y_mean )**2
  enddo
  r2 = 1 - ( SSR / SST )
  
  ! check SSR, SST, r^2 values
  write(*,"(a10,f10.4)") "SSR : ", SSR
  write(*,"(a10,f10.4)") "SST : ", SST
  write(*,"(a10,f10.3)") "r^2 : ", r2
end subroutine lst_sq

