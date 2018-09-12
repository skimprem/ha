function nc_xtype_info(xtype)

  use hamodule

  implicit none

  integer, intent(in) :: xtype
  character(kind=1, len=:), allocatable :: nc_xtype_info
  character(kind=1, len=:), allocatable :: xtype_name

  select case(xtype)
  case(nf90_byte)
    xtype_name = 'byte (8-bit signed integer)'
  case(nf90_ubyte)
    xtype_name = 'ubyte (8-bit unsigned integer)' 
  case(nf90_char)
    xtype_name = 'char (8-bit character byte)'
  case(nf90_short)
    xtype_name = 'short (16-bit signed integer)'
  case(nf90_ushort)
    xtype_name = 'ushort (16-bit unsigned integer)'
  case(nf90_int)
    xtype_name = 'int (32-bit signed integer)'
  case(nf90_uint)
    xtype_name = 'uint (32-bit unsigned integer)'
  case(nf90_int64)
    xtype_name = 'int64 (64-bit signed integer)'
  case(nf90_uint64)
    xtype_name = 'uint64 (64-bit signed integer)'
  case(nf90_float)
    xtype_name = 'float (32-bit floating point)'
  case(nf90_double)
    xtype_name = 'double (64-bit floating point)'
  case(nf90_string)
    xtype_name = 'string (variable length character string)'
  end select

  nc_xtype_info = xtype_name

  return

end function nc_xtype_info
