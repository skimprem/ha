subroutine nc_print_data(nc_file, un)

  use hamodule

  implicit none

  type(ncfile), intent(in) :: nc_file
  integer(4), intent(in) :: un
  integer(4) :: i, j, k

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
  end do

  return

end subroutine nc_print_data
