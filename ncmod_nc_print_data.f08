subroutine nc_print_data(nc_file, output_file_name)

  use hamodule

  implicit none

  type(ncfile), intent(in) :: nc_file
  character(*), intent(in) :: output_file_name
  character(:), allocatable :: string
  integer(4) :: i, j, k, un, stdout = 6, string_len
  real(8) :: progress_coef, tarray(2), current

  open(newunit = un, file =  output_file_name)

  string = 'Writing grid to file "'//output_file_name//'"...: '
  string_len = len_trim(string)
  progress_coef = 100.0 / float(nc_file%dimension(1)%len)
  write(stdout, '(a)')

  k = 1

  write(un, '(a)')&
  trim(adjustl(nc_file%variable(1)%name))//';'//&
  trim(adjustl(nc_file%variable(2)%name))//';'//&
  trim(adjustl(nc_file%variable(3)%name))

  do i = 1, nc_file%dimension(1)%len 
    do j = 1,  nc_file%dimension(2)%len
      write(un, '(a)')&
      nc_value_print(value = nc_file%variable(1)%value,&
                     xtype = nc_file%variable(1)%xtype,&
                     ndims = nc_file%variable(1)%ndims,&
                     i = i)//';'//&
      nc_value_print(value = nc_file%variable(2)%value,&
                     xtype = nc_file%variable(2)%xtype,&
                     ndims = nc_file%variable(2)%ndims,&
                     i = j)//';'//&
      nc_value_print(value = nc_file%variable(3)%value,&
                     xtype = nc_file%variable(3)%xtype,&
                     ndims = nc_file%variable(3)%ndims,&
                     i = i, j = j)
    end do

    do j =  1, string_len + 17 
      write(stdout, '(a)', advance = 'no') '\b'
    end do

    write(stdout, '(a)', advance = 'no') string
    write(stdout, '(f5.1)', advance = 'no') float(i) * progress_coef
    write(stdout, '(a)', advance =  'no') '% completed'

  end do

  write(stdout, '(a)')

  return

end subroutine nc_print_data
