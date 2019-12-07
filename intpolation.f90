PROGRAM INTERPOLATION
	implicit none
 
  ! parameter declaration
  integer, parameter :: SIZE = 100
  integer :: cnt
  real, dimension(SIZE) :: x,y
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

subroutine lst_sq(filename,num_lines,r2)
  implicit none
  ! calculating method of least squre, base on y= ax + b
  ! error will be ==>  sigma(yi-f(x))^2
  character(100) :: filename
  double precision :: r2, a, b
  double precision :: sig_x,sig_y,sig_xy,sig_x2
  integer :: num_lines
  integer :: i
  real, dimension(num_lines)::x,y

  !initialize variables
  r2 = 0.0
  a = 0.0
  b = 0.0
  sig_x = 0.0
  sig_y = 0.0     
  sig_xy = 0.0
  sig_x2 = 0.0

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

  ! calculating r2 

  ! check a and b value
  write(*,90) "a","b","r^2"
  write(*,100) a, b,r2
  90 format(3(2x,a7))
  100 format(2(2x,f7.2),2x,f7.4)
end subroutine lst_sq

