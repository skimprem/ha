subroutine sh_print_info(sh_file)

  use hamodule

  implicit none

  type(shfile), intent(in) :: sh_file
  integer :: i, j, un = 6

  write(un, '(2x, a)', advance = 'no') 'Expand method: '

  select case(sh_file%method)
  case('dh')
    write(un, '(a)') 'Driscoll and Healy sampling theorem'
  case('ls')
    write(un, '(a)') 'Least squares inversion'
  end select

  write(un, '(2x, a)') 'n: '//&
  number_to_string(sh_file%n)

  write(un, '(2x, a)') 'norm: '//&
  number_to_string(sh_file%norm)

  write(un, '(2x, a)') 'sampling: '//&
  number_to_string(sh_file%sampling)

  write(un, '(2x, a)') 'csphase: '//&
  number_to_string(sh_file%csphase)

  write(un, '(2x, a)') 'lmax_calc: '//&
  number_to_string(sh_file%lmax_calc)

  write(un, '(2x, a)') 'exitstatus: '//&
  number_to_string(sh_file%exitstatus)

  do i = 1, sh_file%lmax
    do j = 1, i
      write(un, *) i, j, sh_file%cilm(1, i, j), sh_file%cilm(2, i, j)
    end do
  end do

end subroutine sh_print_info
