module shmodule

  type shinit
    real(8), dimension(:,:), allocatable :: griddh
    real(8), dimension(:,:,:), allocatable :: cilm
    integer :: n,&
               lmax,&
               norm,&
               sampling,&
               csphase,&
               exitstatus
  end type shinit

  contains

  subroutine print_sh_info()
    implicit none
  end subroutine print_sh_info


end module shmodule
