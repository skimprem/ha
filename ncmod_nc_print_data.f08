subroutine nc_print_data(nc_file, un)

  use hamodule

  implicit none

  type(ncfile), intent(in) :: nc_file
  integer(4), intent(in) :: un
  integer(4) :: i, j, k
  integer(2) :: progress_step, display_unit = 6
  real :: progress, progress_coef

  progress = 0.0

  write(display_unit, '(1x, a)', advance = 'no') 'Writing... ['
  progress_step = 2
  progress_coef = 100.0 / nc_file%dimension(1)%len

  k = 1
  do i = 1, nc_file%nvariables
    write(un, '(a)', advance = 'no') trim(nc_file%variable(i)%name)//'; '
  end do
  write(un, *)
  do i = 1, nc_file%dimension(1)%len 
    do j = 1,  nc_file%dimension(2)%len
      write(un, '(a)')&
        nc_value_print(value = nc_file%variable(1)%value,&
                       xtype = nc_file%variable(1)%xtype,&
                       ndims = nc_file%variable(1)%ndims,&
                       i = i)//'; '//&
        nc_value_print(value = nc_file%variable(2)%value,&
                       xtype = nc_file%variable(2)%xtype,&
                       ndims = nc_file%variable(2)%ndims,&
                       i = j)//'; '//&
        nc_value_print(value = nc_file%variable(3)%value,&
                       xtype = nc_file%variable(3)%xtype,&
                       ndims = nc_file%variable(3)%ndims,&
                       i = i, j = j)
    end do

    progress = progress + progress_coef
    if(progress / progress_step > 1) then
      write(display_unit, '(a)', advance = 'no') '='
      progress_step = progress_step + 2
    end if

  end do

  write(display_unit, '(a)') '] 100%'

  return

end subroutine nc_print_data
