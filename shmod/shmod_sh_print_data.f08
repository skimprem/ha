subroutine sh_print_data(sh_file, output_filename, verbose)
  use hamodule
  implicit none
  type(shfile), intent(in) :: sh_file
  character(*), intent(in) :: output_filename
  character(:), allocatable :: string
  integer(4) :: i, j, un, stdout = 6, string_len
  real(8) :: progress_coef
  character(*), intent(in), optional :: verbose
  logical :: verbose_mode
  verbose_mode = .false.
  if(present(verbose)) verbose_mode = .true.
  if(verbose_mode) write(stdout, '(a)') verbose//' begin sh_print_data(): '//output_filename
  open(newunit = un, file = output_filename)
  string = verbose//'   '//'Writing coef to fiel"'//output_filename//'"...: '
  string_len = len_trim(string)
  progress_coef = 100.0 / real(sh_file%lmax, 4)
  do i = 1, sh_file%lmax
    do j = 1, i
      write(un, *) i, j, sh_file%cilm(1, i, j), sh_file%cilm(2, i, j)
    end do
    do j = 1, string_len + 17
      !write(stdout, '(a)', advance = 'no') '\b'
    end do
    write(stdout, '(a)', advance = 'no') string
    write(stdout, '(f5.1)', advance = 'no') real(i, 4) * progress_coef
    write(stdout, '(a)', advance = 'no') '% completed'
  end do
  write(stdout, '(a)')
  if(verbose_mode) write(stdout, '(a)') verbose//' end sh_print_data()'
  return
end subroutine sh_print_data
