function ha_mem_info_int(value, units_mode)
  implicit none
  integer(kind=4), intent(in) :: value
  character(len=:), allocatable :: ha_mem_info_int
  character(*), intent(in) :: units_mode
  character(len=:), allocatable :: units
  integer(kind=4) :: i
  real(kind=8) :: value_double 
  i = 1
  select case(units_mode)
  case('b')
    value_double = real(value, 8)
    units = 'b'
  case('B')
    value_double = real(value, 8) / 8._8
    units = 'B'
  end select
  do
    if(value_double < 1000) then
      select case(i)
      case(2)
        units = ' k'//units
      case(3)
        units = ' M'//units
      case(4)
        units = ' G'//units
      case(5)
        units = ' T'//units
      case(6)
        units = ' P'//units
      case(7)
        units = ' E'//units
      case(8)
        units = ' Z'//units
      case(9)
        units = ' Y'//units
      end select
      ha_mem_info_int = number_to_string(value_double, '(f100.1)')//units
      return
    end if
    value_double = value_double / 1000._8
    i = i + 1
  end do
end function ha_mem_info_int
function ha_mem_info_int64(value, units_mode)
  implicit none
  integer(kind=8), intent(in) :: value
  character(len=:), allocatable :: ha_mem_info_int64
  character(*), intent(in) :: units_mode
  character(len=:), allocatable :: units
  integer(kind=4) :: i
  real(kind=8) :: value_double 
  i = 1
  select case(units_mode)
  case('b')
    value_double = real(value, 8)
    units = 'b'
  case('B')
    value_double = real(value, 8) / 8._8
    units = 'B'
  end select
  do
    if(value_double < 1000) then
      select case(i)
      case(2)
        units = ' k'//units
      case(3)
        units = ' M'//units
      case(4)
        units = ' G'//units
      case(5)
        units = ' T'//units
      case(6)
        units = ' P'//units
      case(7)
        units = ' E'//units
      case(8)
        units = ' Z'//units
      case(9)
        units = ' Y'//units
      end select
      ha_mem_info_int64 = number_to_string(value_double, '(f100.1)')//units
      return
    end if
    value_double = value_double / 1000._8
    i = i + 1
  end do
end function ha_mem_info_int64
function ha_mem_info_short(value, units_mode)
  implicit none
  integer(kind=2), intent(in) :: value
  character(len=:), allocatable :: ha_mem_info_short
  character(*), intent(in) :: units_mode
  character(len=:), allocatable :: units
  integer(kind=4) :: i
  real(kind=8) :: value_double 
  i = 1
  select case(units_mode)
  case('b')
    value_double = real(value, 8)
    units = 'b'
  case('B')
    value_double = real(value, 8) / 8._8
    units = 'B'
  end select
  do
    if(value_double < 1000) then
      select case(i)
      case(2)
        units = ' k'//units
      case(3)
        units = ' M'//units
      case(4)
        units = ' G'//units
      case(5)
        units = ' T'//units
      case(6)
        units = ' P'//units
      case(7)
        units = ' E'//units
      case(8)
        units = ' Z'//units
      case(9)
        units = ' Y'//units
      end select
      ha_mem_info_short = number_to_string(value_double, '(f100.1)')//units
      return
    end if
    value_double = value_double / 1000._8
    i = i + 1
  end do
end function ha_mem_info_short
function ha_mem_info_byte(value, units_mode)
  implicit none
  integer(kind=1), intent(in) :: value
  character(len=:), allocatable :: ha_mem_info_byte
  character(*), intent(in) :: units_mode
  character(len=:), allocatable :: units
  integer(kind=4) :: i
  real(kind=8) :: value_double 
  i = 1
  select case(units_mode)
  case('b')
    value_double = real(value, 8)
    units = 'b'
  case('B')
    value_double = real(value, 8) / 8._8
    units = 'B'
  end select
  do
    if(value_double < 1000) then
      select case(i)
      case(2)
        units = ' k'//units
      case(3)
        units = ' M'//units
      case(4)
        units = ' G'//units
      case(5)
        units = ' T'//units
      case(6)
        units = ' P'//units
      case(7)
        units = ' E'//units
      case(8)
        units = ' Z'//units
      case(9)
        units = ' Y'//units
      end select
      ha_mem_info_byte = number_to_string(value_double, '(f100.1)')//units
      return
    end if
    value_double = value_double / 1000._8
    i = i + 1
  end do
end function ha_mem_info_byte
function ha_mem_info_float(value, units_mode)
  implicit none
  real(kind=4), intent(in) :: value
  character(len=:), allocatable :: ha_mem_info_float
  character(*), intent(in) :: units_mode
  character(len=:), allocatable :: units
  integer(kind=4) :: i
  real(kind=8) :: value_double 
  i = 1
  select case(units_mode)
  case('b')
    value_double = real(value, 8)
    units = 'b'
  case('B')
    value_double = real(value, 8) / 8._8
    units = 'B'
  end select
  do
    if(value_double < 1000) then
      select case(i)
      case(2)
        units = ' k'//units
      case(3)
        units = ' M'//units
      case(4)
        units = ' G'//units
      case(5)
        units = ' T'//units
      case(6)
        units = ' P'//units
      case(7)
        units = ' E'//units
      case(8)
        units = ' Z'//units
      case(9)
        units = ' Y'//units
      end select
      ha_mem_info_float = number_to_string(value_double, '(f100.1)')//units
      return
    end if
    value_double = value_double / 1000._8
    i = i + 1
  end do
end function ha_mem_info_float
function ha_mem_info_double(value, units_mode)
  implicit none
  real(kind=8), intent(in) :: value
  character(len=:), allocatable :: ha_mem_info_double
  character(*), intent(in) :: units_mode
  character(len=:), allocatable :: units
  integer(kind=4) :: i
  real(kind=8) :: value_double 
  i = 1
  select case(units_mode)
  case('b')
    value_double = value
    units = 'b'
  case('B')
    value_double = value / 8._8
    units = 'B'
  end select
  do
    if(value_double < 1000) then
      select case(i)
      case(2)
        units = ' k'//units
      case(3)
        units = ' M'//units
      case(4)
        units = ' G'//units
      case(5)
        units = ' T'//units
      case(6)
        units = ' P'//units
      case(7)
        units = ' E'//units
      case(8)
        units = ' Z'//units
      case(9)
        units = ' Y'//units
      end select
      ha_mem_info_double = number_to_string(value_double, '(f100.1)')//units
      return
    end if
    value_double = value_double / 1000._8
    i = i + 1
  end do
end function ha_mem_info_double
