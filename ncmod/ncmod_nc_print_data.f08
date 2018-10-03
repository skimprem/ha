subroutine nc_print_data_two_dim(nc_file, output_filename, verbose_phrase)
  use hamodule
  implicit none
  type(ncfile), intent(in) :: nc_file
  character(*), intent(in) :: output_filename
  character(len=:), allocatable :: string, frmt
  integer(kind=4) :: i, j, un, stdout = 6, string_len 
  integer(kind=4), dimension(3) :: variable_number
  real(kind=8) :: progress_coef
  character(*), intent(in), optional :: verbose_phrase
  logical :: verbose_mode
  verbose_mode = .false.
  if((present(verbose_phrase) .eqv. .true.) .and. (trim(adjustl(verbose_phrase)) /= '')) then
    verbose_mode = .true.
  end if
  if(verbose_mode .eqv. .true.) then
    write(stdout, '(a)') verbose//'begin nc_print_data():'
  end if
  open(newunit = un, file = output_filename)
  string = verbose//'   '//'Writing grid to file "'//output_filename//'"...: '
  string_len = len_trim(string)
  progress_coef = 100.0 / real(nc_file%variable(k)%dimension(1)%len, 4)
  write(un, '(a)')&
  trim(adjustl(nc_file%variable(1)%name))//' '//&
  trim(adjustl(nc_file%variable(2)%name))//' '//&
  trim(adjustl(nc_file%variable(3)%name))
  do i = 1, nc_file%dimension(1)%len 
    do j = 1,  nc_file%dimension(2)%len
      write(un, '(a)')&
      trim_null(nc_value_print(value = nc_file%variable(1)%value,&
                     xtype = nc_file%variable(1)%xtype,&
                     ndims = nc_file%variable(1)%ndims,&
                     i = i))//' '//&
      trim_null(nc_value_print(value = nc_file%variable(2)%value,&
                     xtype = nc_file%variable(2)%xtype,&
                     ndims = nc_file%variable(2)%ndims,&
                     i = j))//' '//&
      trim_null(nc_value_print(value = nc_file%variable(3)%value,&
                     xtype = nc_file%variable(3)%xtype,&
                     ndims = nc_file%variable(3)%ndims,&
                     i = i, j = j))
    end do
    do j =  1, string_len + 17 
      write(stdout, '(a)', advance = 'no') '\b'
    end do
    write(stdout, '(a)', advance = 'no') string
    write(stdout, '(f5.1)', advance = 'no') real(i, 4) * progress_coef
    write(stdout, '(a)', advance =  'no') '% completed'
  end do
  write(stdout, '(a)')
  return
end subroutine nc_print_data_two_dim
