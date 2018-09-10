!###############################################################
! ONE DIM
!###############################################################
subroutine nc_variable_conv_byte_one_dim(variable, output_value)
  implicit none
  type(ncvariables) :: variable 
  integer(kind=1), dimension(:), allocatable :: output_value
  integer(kind=4) :: i
  real(8), dimension(:), allocatable :: scale_factor
  call nc_value_conv(variable%value, output_value)
  do i = 1, variable%natts
    select case(trim(adjustl(variable%attribute(i)%name)))
    case('scale_factor')
      call nc_value_conv(variable%attribute(i)%value, scale_factor)
      output_value = output_value * scale_factor(1)
    end select
  end do
  return
end subroutine nc_variable_conv_byte_one_dim
subroutine nc_variable_conv_short_one_dim(variable, output_value)
  implicit none
  type(ncvariables) :: variable 
  integer(kind=2), dimension(:), allocatable :: output_value
  integer(kind=4) :: i
  real(8), dimension(:), allocatable :: scale_factor
  call nc_value_conv(variable%value, output_value)
  do i = 1, variable%natts
    select case(trim(adjustl(variable%attribute(i)%name)))
    case('scale_factor')
      call nc_value_conv(variable%attribute(i)%value, scale_factor)
      output_value = output_value * scale_factor(1)
    end select
  end do
  return
end subroutine nc_variable_conv_short_one_dim
subroutine nc_variable_conv_int_one_dim(variable, output_value)
  implicit none
  type(ncvariables) :: variable 
  integer(kind=4), dimension(:), allocatable :: output_value
  integer(kind=4) :: i
  real(8), dimension(:), allocatable :: scale_factor
  call nc_value_conv(variable%value, output_value)
  do i = 1, variable%natts
    select case(trim(adjustl(variable%attribute(i)%name)))
    case('scale_factor')
      call nc_value_conv(variable%attribute(i)%value, scale_factor)
      output_value = output_value * scale_factor(1)
    end select
  end do
  return
end subroutine nc_variable_conv_int_one_dim
subroutine nc_variable_conv_int64_one_dim(variable, output_value)
  implicit none
  type(ncvariables) :: variable 
  integer(kind=8), dimension(:), allocatable :: output_value
  integer(kind=4) :: i
  real(8), dimension(:), allocatable :: scale_factor
  call nc_value_conv(variable%value, output_value)
  do i = 1, variable%natts
    select case(trim(adjustl(variable%attribute(i)%name)))
    case('scale_factor')
      call nc_value_conv(variable%attribute(i)%value, scale_factor)
      output_value = output_value * scale_factor(1)
    end select
  end do
  return
end subroutine nc_variable_conv_int64_one_dim
subroutine nc_variable_conv_float_one_dim(variable, output_value)
  implicit none
  type(ncvariables) :: variable 
  real(kind=4), dimension(:), allocatable :: output_value
  integer(kind=4) :: i
  real(8), dimension(:), allocatable :: scale_factor
  call nc_value_conv(variable%value, output_value)
  do i = 1, variable%natts
    select case(trim(adjustl(variable%attribute(i)%name)))
    case('scale_factor')
      call nc_value_conv(variable%attribute(i)%value, scale_factor)
      output_value = output_value * scale_factor(1)
    end select
  end do
  return
end subroutine nc_variable_conv_float_one_dim
subroutine nc_variable_conv_double_one_dim(variable, output_value)
  implicit none
  type(ncvariables) :: variable 
  real(kind=8), dimension(:), allocatable :: output_value
  integer(kind=4) :: i
  real(8), dimension(:), allocatable :: scale_factor
  call nc_value_conv(variable%value, output_value)
  do i = 1, variable%natts
    select case(trim(adjustl(variable%attribute(i)%name)))
    case('scale_factor')
      call nc_value_conv(variable%attribute(i)%value, scale_factor)
      output_value = output_value * scale_factor(1)
    end select
  end do
  return
end subroutine nc_variable_conv_double_one_dim
!###############################################################
! TWO DIM
!###############################################################
subroutine nc_variable_conv_byte_two_dim(variable, output_value)
  implicit none
  type(ncvariables) :: variable 
  integer(kind=1), dimension(:,:), allocatable :: output_value
  integer(kind=4) :: i
  real(8), dimension(:), allocatable :: scale_factor
  call nc_value_conv(variable%value, output_value)
  do i = 1, variable%natts
    select case(trim(adjustl(variable%attribute(i)%name)))
    case('scale_factor')
      call nc_value_conv(variable%attribute(i)%value, scale_factor)
      output_value = output_value * scale_factor(1)
    end select
  end do
  return
end subroutine nc_variable_conv_byte_two_dim
subroutine nc_variable_conv_short_two_dim(variable, output_value)
  implicit none
  type(ncvariables) :: variable 
  integer(kind=2), dimension(:,:), allocatable :: output_value
  integer(kind=4) :: i
  real(8), dimension(:), allocatable :: scale_factor
  call nc_value_conv(variable%value, output_value)
  do i = 1, variable%natts
    select case(trim(adjustl(variable%attribute(i)%name)))
    case('scale_factor')
      call nc_value_conv(variable%attribute(i)%value, scale_factor)
      output_value = output_value * scale_factor(1)
    end select
  end do
  return
end subroutine nc_variable_conv_short_two_dim
subroutine nc_variable_conv_int_two_dim(variable, output_value)
  implicit none
  type(ncvariables) :: variable 
  integer(kind=4), dimension(:,:), allocatable :: output_value
  integer(kind=4) :: i
  real(8), dimension(:), allocatable :: scale_factor
  call nc_value_conv(variable%value, output_value)
  do i = 1, variable%natts
    select case(trim(adjustl(variable%attribute(i)%name)))
    case('scale_factor')
      call nc_value_conv(variable%attribute(i)%value, scale_factor)
      output_value = output_value * scale_factor(1)
    end select
  end do
  return
end subroutine nc_variable_conv_int_two_dim
subroutine nc_variable_conv_int64_two_dim(variable, output_value)
  implicit none
  type(ncvariables) :: variable 
  integer(kind=8), dimension(:,:), allocatable :: output_value
  integer(kind=4) :: i
  real(8), dimension(:), allocatable :: scale_factor
  call nc_value_conv(variable%value, output_value)
  do i = 1, variable%natts
    select case(trim(adjustl(variable%attribute(i)%name)))
    case('scale_factor')
      call nc_value_conv(variable%attribute(i)%value, scale_factor)
      output_value = output_value * scale_factor(1)
    end select
  end do
  return
end subroutine nc_variable_conv_int64_two_dim
subroutine nc_variable_conv_float_two_dim(variable, output_value)
  implicit none
  type(ncvariables) :: variable 
  real(kind=4), dimension(:,:), allocatable :: output_value
  integer(kind=4) :: i
  real(8), dimension(:), allocatable :: scale_factor
  call nc_value_conv(variable%value, output_value)
  do i = 1, variable%natts
    select case(trim(adjustl(variable%attribute(i)%name)))
    case('scale_factor')
      call nc_value_conv(variable%attribute(i)%value, scale_factor)
      output_value = output_value * scale_factor(1)
    end select
  end do
  return
end subroutine nc_variable_conv_float_two_dim
subroutine nc_variable_conv_double_two_dim(variable, output_value)
  implicit none
  type(ncvariables) :: variable 
  real(kind=8), dimension(:,:), allocatable :: output_value
  integer(kind=4) :: i
  real(8), dimension(:), allocatable :: scale_factor
  call nc_value_conv(variable%value, output_value)
  do i = 1, variable%natts
    select case(trim(adjustl(variable%attribute(i)%name)))
    case('scale_factor')
      call nc_value_conv(variable%attribute(i)%value, scale_factor)
      output_value = output_value * scale_factor(1)
    end select
  end do
  return
end subroutine nc_variable_conv_double_two_dim
!###############################################################
! THREE DIM
!###############################################################
subroutine nc_variable_conv_byte_three_dim(variable, output_value)
  implicit none
  type(ncvariables) :: variable 
  integer(kind=1), dimension(:,:,:), allocatable :: output_value
  integer(kind=4) :: i
  real(8), dimension(:), allocatable :: scale_factor
  call nc_value_conv(variable%value, output_value)
  do i = 1, variable%natts
    select case(trim(adjustl(variable%attribute(i)%name)))
    case('scale_factor')
      call nc_value_conv(variable%attribute(i)%value, scale_factor)
      output_value = output_value * scale_factor(1)
    end select
  end do
  return
end subroutine nc_variable_conv_byte_three_dim
subroutine nc_variable_conv_short_three_dim(variable, output_value)
  implicit none
  type(ncvariables) :: variable 
  integer(kind=2), dimension(:,:,:), allocatable :: output_value
  integer(kind=4) :: i
  real(8), dimension(:), allocatable :: scale_factor
  call nc_value_conv(variable%value, output_value)
  do i = 1, variable%natts
    select case(trim(adjustl(variable%attribute(i)%name)))
    case('scale_factor')
      call nc_value_conv(variable%attribute(i)%value, scale_factor)
      output_value = output_value * scale_factor(1)
    end select
  end do
  return
end subroutine nc_variable_conv_short_three_dim
subroutine nc_variable_conv_int_three_dim(variable, output_value)
  implicit none
  type(ncvariables) :: variable 
  integer(kind=4), dimension(:,:,:), allocatable :: output_value
  integer(kind=4) :: i
  real(8), dimension(:), allocatable :: scale_factor
  call nc_value_conv(variable%value, output_value)
  do i = 1, variable%natts
    select case(trim(adjustl(variable%attribute(i)%name)))
    case('scale_factor')
      call nc_value_conv(variable%attribute(i)%value, scale_factor)
      output_value = output_value * scale_factor(1)
    end select
  end do
  return
end subroutine nc_variable_conv_int_three_dim
subroutine nc_variable_conv_int64_three_dim(variable, output_value)
  implicit none
  type(ncvariables) :: variable 
  integer(kind=8), dimension(:,:,:), allocatable :: output_value
  integer(kind=4) :: i
  real(8), dimension(:), allocatable :: scale_factor
  call nc_value_conv(variable%value, output_value)
  do i = 1, variable%natts
    select case(trim(adjustl(variable%attribute(i)%name)))
    case('scale_factor')
      call nc_value_conv(variable%attribute(i)%value, scale_factor)
      output_value = output_value * scale_factor(1)
    end select
  end do
  return
end subroutine nc_variable_conv_int64_three_dim
subroutine nc_variable_conv_float_three_dim(variable, output_value)
  implicit none
  type(ncvariables) :: variable 
  real(kind=4), dimension(:,:,:), allocatable :: output_value
  integer(kind=4) :: i
  real(8), dimension(:), allocatable :: scale_factor
  call nc_value_conv(variable%value, output_value)
  do i = 1, variable%natts
    select case(trim(adjustl(variable%attribute(i)%name)))
    case('scale_factor')
      call nc_value_conv(variable%attribute(i)%value, scale_factor)
      output_value = output_value * scale_factor(1)
    end select
  end do
  return
end subroutine nc_variable_conv_float_three_dim
subroutine nc_variable_conv_double_three_dim(variable, output_value)
  implicit none
  type(ncvariables) :: variable 
  real(kind=8), dimension(:,:,:), allocatable :: output_value
  integer(kind=4) :: i
  real(8), dimension(:), allocatable :: scale_factor
  call nc_value_conv(variable%value, output_value)
  do i = 1, variable%natts
    select case(trim(adjustl(variable%attribute(i)%name)))
    case('scale_factor')
      call nc_value_conv(variable%attribute(i)%value, scale_factor)
      output_value = output_value * scale_factor(1)
    end select
  end do
  return
end subroutine nc_variable_conv_double_three_dim
