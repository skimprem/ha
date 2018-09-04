subroutine get_att_xtype(&
     ncid,& !
     varid,& !
     xtype,& !
     name,& !
     len,& !
     value,& !
     verbose) !

  use netcdf

  implicit none

  integer, intent(in) :: ncid, varid, xtype, len
  character(*), intent(in) :: name
  type(ncvalues), intent(out) :: value
  integer :: stdout = 6
  logical, intent(in), optional :: verbose

  if(verbose) write(stdout, '(a)') 'get_att_xtype: start'

  select case(xtype)
  case(0)
    ! 0
    if(verbose) write(stdout, '(2x,a)') 'error: '//nc_xtype_info(xtype)//' not set'
    stop
  case(nf90_byte)
    if(verbose) write(stdout, '(2x.a)') 'xtype: '//nc_xtype_info(xtype)
    ! NC_BYTE: 8-bit signed integer
    allocate( value%byte_1(len) )
    call nc_error_check(&
        'nc_get_att',&
        nf90_get_att(&
          ncid = ncid,&
          varid = varid,&
          name = name,&
          values = value%byte_1(1)&
          )&
        )
  case(nf90_ubyte)
    ! NC_UBYTE: 8-bit unsigned integer
    write(stdout, '(a)') nc_xtype_info(xtype)//' not set'
    stop
  case(nf90_char)
    ! NC_CHAR: 8-bit character byte
    allocate( value%char_1(len) )
    call nc_error_check(&
        'nc_get_att',&
        nf90_get_att(&
          ncid = ncid,&
          varid = varid,&
          name = name,&
          values = value%char_1(1)&
          )&
        )
  case(nf90_short)
    ! NC_SHORT: 16-bit signed integer
    allocate( value%short_1(len) )
    call nc_error_check(&
        'nc_get_att',&
        nf90_get_att(&
          ncid = ncid,&
          varid = varid,&
          name = name,&
          values = value%short_1&
          )&
        )
  case(nf90_ushort)
    ! NC_USHORT: 16-bit unsigned integer
    write(stdout, '(a)') nc_xtype_info(xtype)//' not set'
    stop
  case(nf90_int)
    ! NC_int: (NC_LONG): 32-bit signed integer
    allocate( value%int_1(len) )
    call nc_error_check(&
        'nc_get_att',&
        nf90_get_att(&
          ncid = ncid,&
          varid = varid,&
          name = name,&
          values = value%int_1&
          )&
        )
  case(nf90_uint)
    ! NC_Uint: 32-bit unsigned integer
    write(stdout, '(a)') nc_xtype_info(xtype)//' not set'
    stop
  case(nf90_int64)
    ! NC_int64: 64-bit signed integer
    allocate( value%int64_1(len) )
    call nc_error_check(&
         'nc_get_att',&
         nf90_get_att(&
           ncid = ncid,&
           varid = varid,&
           name = name,&
           values = value%int64_1&
           )&
         )
  case(nf90_uint64)
    ! NC_Uint64: 64-bit unsigned integer
    write(un, '(a)') nc_xtype_info(xtype)//' not set'
    stop
  case(nf90_float)
    ! NC_FLOAT: 32-bit floating point
    allocate( value%float_1(len) )
    call nc_error_check(&
         'nc_get_att',&
         nf90_get_att(&
           ncid = ncid,&
           varid = varid,&
           name = name,&
           values = value%float_1&
           )&
         )
  case(nf90_double)
    ! NC_double: 64-bit floating point
    allocate( value%double_1(len) )
    call nc_error_check(&
        'nc_get_att',&
        nf90_get_att(&
          ncid = ncid,&
          varid = varid,&
          name = name,&
          values = value%double_1&
          )&
        )
  case(nf90_string)
    ! NC_STRING: variable length character string
    allocate( value%string_1(len) )
    call nc_error_check(&
        'nc_get_att',&
        nf90_get_att(&
          ncid = ncid,&
          varid = varid,&
          name = name,&
          values = value%string_1(1)&
          )&
        )
  end select

end subroutine get_att_xtype
