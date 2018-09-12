function nc_value_print(value, xtype, ndims, i, j, k)

  use hamodule

  implicit none

  type(ncvalues), intent(in) :: value
  integer(4), intent(in) :: xtype, ndims, i
  integer(4), intent(in), optional :: j, k
  character(:), allocatable :: nc_value_print
  integer(4) :: un = 6

  !if(present(j) .eqv. .false. .and. present(k) .eqv. .false.) then
    !if(ndims /= 1) then
      !write(un, '(a)') 'error in nc_value_print() function: the value of ndims&
      !must be equal to 1'
      !stop
    !end if
  !else if(present(j) .eqv. .true. .and. present(k) .eqv. .false.) then
    !if(ndims /= 2) then
      !write(un, '(a)') 'error in nc_value_print() function: the value of ndims&
      !must be equal to 2'
      !stop
    !end if
  !else if(present(j) .eqv. .true. .and. present(k) .eqv. .true.) then
    !if(ndims /= 3) then
      !write(un, '(a)') 'error in nc_value_print() function: the value of ndims&
      !must be equal to 3'
      !stop
    !end if
  !else
    !stop 'error in nc_value_print() function:'
  !end if

  select case(xtype)
  case(nf90_byte)
    select case(ndims)
    case(1)
      nc_value_print = number_to_string(value%byte_1(i))
    case(2)
      nc_value_print = number_to_string(value%byte_2(i, j))
    case(3)
      nc_value_print = number_to_string(value%byte_3(i, j, k))
    end select
  case(nf90_ubyte)
  case(nf90_char)
    select case(ndims)
    case(1)
      nc_value_print = value%char_1(i)
    case(2)
      nc_value_print = value%char_2(i, j)
    case(3)
      nc_value_print = value%char_3(i, j, k)
    end select
  case(nf90_short)
    select case(ndims)
    case(1)
      nc_value_print = number_to_string(value%short_1(i))
    case(2)
      nc_value_print = number_to_string(value%short_2(i, j))
    case(3)
      nc_value_print = number_to_string(value%short_3(i, j, k))
    end select
  case(nf90_ushort)
  case(nf90_int)
    select case(ndims)
    case(1)
      nc_value_print = number_to_string(value%int_1(i))
    case(2)
      nc_value_print = number_to_string(value%int_2(i, j))
    case(3)
      nc_value_print = number_to_string(value%int_3(i, j, k))
    end select
  case(nf90_uint)
  case(nf90_int64)
    select case(ndims)
    case(1)
      nc_value_print = number_to_string(value%int64_1(i))
    case(2)
      nc_value_print = number_to_string(value%int64_2(i, j))
    case(3)
      nc_value_print = number_to_string(value%int64_3(i, j, k))
    end select
  case(nf90_uint64)
  case(nf90_float)
    select case(ndims)
    case(1)
      nc_value_print = number_to_string(value%float_1(i))
    case(2)
      nc_value_print = number_to_string(value%float_2(i, j))
    case(3)
      nc_value_print = number_to_string(value%float_3(i, j, k))
    end select
  case(nf90_double)
    select case(ndims)
    case(1)
      nc_value_print = number_to_string(value%double_1(i))
    case(2)
      nc_value_print = number_to_string(value%double_2(i, j))
    case(3)
      nc_value_print = number_to_string(value%double_3(i, j, k))
    end select
  case(nf90_string)
    select case(ndims)
    case(1)
      nc_value_print = trim(adjustl(value%string_1(i)))
    case(2)
      nc_value_print = trim(adjustl(value%string_2(i, j)))
    case(3)
      nc_value_print = trim(adjustl(value%string_3(i, j, k)))
    end select
  end select

  return

end function nc_value_print
