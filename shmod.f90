module shmodule

  type shfile
    real(8), dimension(:,:), allocatable :: griddh
    real(8), dimension(:,:,:), allocatable :: cilm
    integer :: n,&
               lmax,&
               norm,&
               sampling,&
               csphase,&
               exitstatus
  end type shfile

  contains

  subroutine print_sh_info(sh_file)
    implicit none
    type(shfile), intent(in) :: sh_file
  end subroutine print_sh_info


end module shmodule
