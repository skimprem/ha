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
