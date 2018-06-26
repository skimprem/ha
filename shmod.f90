module shmodule

  type shfile
    real(8), dimension(:,:), allocatable :: griddh
    real(8), dimension(:,:,:), allocatable :: cilm
    integer :: n,&
               lmax,&
               norm,&
               sampling,&
               lmax_calc,&
               csphase,&
               exitstatus
    character(2) :: method
  end type shfile

  contains

  subroutine print_sh_info(sh_file)

    use hamodule

    implicit none
    type(shfile), intent(in) :: sh_file
    integer :: i, j, un = 6

    select case(sh_file%method)
    case('dh')
      write(un, '(a)') 'Driscoll and Healy sampling theorem'
    case('ls')
      write(un, '(a)') 'Least squares inversion'
    end select

    write(un, '(2x, a)') 'n: '//&
    number_to_string(iv = int(sh_file%n, 4), len = num_len(iv = int(sh_file%n, 4)))

    write(un, '(2x, a)') 'norm: '//&
    number_to_string(iv = int(sh_file%norm, 4), len = num_len(iv = int(sh_file%norm, 4)))

    write(un, '(2x, a)') 'sampling: '//&
    number_to_string(iv = int(sh_file%sampling, 4), len = num_len(iv = int(sh_file%sampling, 4)))

    write(un, '(2x, a)') 'csphase: '//&
    number_to_string(iv = int(sh_file%csphase, 4), len = num_len(iv = int(sh_file%csphase, 4)))

    write(un, '(2x, a)') 'lmax_calc: '//&
    number_to_string(iv = int(sh_file%lmax_calc, 4), len = num_len(iv = int(sh_file%lmax_calc, 4)))

    write(un, '(2x, a)') 'exitstatus: '//&
    number_to_string(iv = int(sh_file%exitstatus, 4), len = num_len(iv = int(sh_file%exitstatus, 4)))

    do i = 1, sh_file%lmax
      do j = 1, i
        write(un, *) i, j, sh_file%cilm(1, i, j), sh_file%cilm(2, i, j)
      end do
    end do



  end subroutine print_sh_info


end module shmodule
