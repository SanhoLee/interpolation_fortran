PROGRAM INTERPOLATION
	implicit none
 
  ! parameter declaration
  integer, parameter :: SIZE = 100
  real, dimension(SIZE) :: x,y
  character(100) :: read_file="read_file.txt", write_file=""
  
  
  
  open(10, file=read_file)
  read(10,*) x(1), y(1)

  write(*,100) x(1), y(1)
100 format(2(1x, f5.2))

END PROGRAM INTERPOLATION
