subroutine get_att_xtype(ncid, varid, attribute, verbose_phrase)
  use netcdf
  use hamodule
  implicit none
  integer, intent(in) :: ncid, varid
  type(ncattributes), intent(inout) :: attribute 
  integer :: i, stdout = 6, error_stat
  character(*), intent(in), optional :: verbose_phrase
  logical :: verbose_mode
  error_stat = 0
  verbose_mode = .false.
  if(present(verbose_phrase) .eqv. .true.) then
    verbose_mode = .true.
  end if
  select case(attribute%xtype)
  case(0) ! 0
    error_stat = 1
  case(nf90_byte) ! NC_BYTE: 8-bit signed integer
    allocate( attribute%value_byte(attribute%len) )
    call nc_error_check(&
        'nc_get_att',&
        nf90_get_att(&
          ncid = ncid,&
          varid = varid,&
          name = attribute%name,&
          values = attribute%value_byte&
          )&
        )
  case(nf90_ubyte) ! NC_UBYTE: 8-bit unsigned integer
    error_stat = 1
  case(nf90_char) ! NC_CHAR: 8-bit character byte
    allocate( attribute%value_char(attribute%len) )
    call nc_error_check(&
        'nc_get_att',&
        nf90_get_att(&
          ncid = ncid,&
          varid = varid,&
          name = attribute%name,&
          values = attribute%value_char(1)&
          )&
        )
  case(nf90_short) ! NC_SHORT: 16-bit signed integer
    allocate( attribute%value_short(attribute%len) )
    call nc_error_check(&
        'nc_get_att',&
        nf90_get_att(&
          ncid = ncid,&
          varid = varid,&
          name = attribute%name,&
          values = attribute%value_short&
          )&
        )
  case(nf90_ushort) ! NC_USHORT: 16-bit unsigned integer
    error_stat = 1
  case(nf90_int) ! NC_int: (NC_LONG): 32-bit signed integer
    allocate( attribute%value_int(attribute%len) )
    call nc_error_check(&
        'nc_get_att',&
        nf90_get_att(&
          ncid = ncid,&
          varid = varid,&
          name = attribute%name,&
          values = attribute%value_int&
          )&
        )
  case(nf90_uint) ! NC_Uint: 32-bit unsigned integer
    error_stat = 1
  case(nf90_int64) ! NC_int64: 64-bit signed integer
    allocate( attribute%value_int64(attribute%len) )
    call nc_error_check(&
         'nc_get_att',&
         nf90_get_att(&
           ncid = ncid,&
           varid = varid,&
           name = attribute%name,&
           values = attribute%value_int64&
           )&
         )
  case(nf90_uint64) ! NC_Uint64: 64-bit unsigned integer
    error_stat = 1
  case(nf90_float) ! NC_FLOAT: 32-bit floating point
    allocate( attribute%value_float(attribute%len) )
    call nc_error_check(&
         'nc_get_att',&
         nf90_get_att(&
           ncid = ncid,&
           varid = varid,&
           name = attribute%name,&
           values = attribute%value_float&
           )&
         )
  case(nf90_double) ! NC_double: 64-bit floating point
    allocate( attribute%value_double(attribute%len) )
    call nc_error_check(&
        'nc_get_att',&
        nf90_get_att(&
          ncid = ncid,&
          varid = varid,&
          name = attribute%name,&
          values = attribute%value_double&
          )&
        )
  case(nf90_string) ! NC_STRING: variable length character string
    allocate(character(len=nf90_max_name) :: attribute%value_string(attribute%len))
    call nc_error_check(&
        'nc_get_att',&
        nf90_get_att(&
          ncid = ncid,&
          varid = varid,&
          name = attribute%name,&
          values = attribute%value_string(1)&
          )&
        )
  end select
  if(verbose_mode .eqv. .true.) then
    write(stdout, '(a)') verbose_phrase//'   nc_get_att(): done!'
    write(stdout, '(a)') verbose_phrase//'     value: '//trim(nc_attribute_print(attribute))
  end if
  if(error_stat /= 0) then
    if(verbose_mode .eqv. .true.) then
      write(stdout, '(a)') verbose_phrase//'   error: '//nc_xtype_info(attribute%xtype)//' not set'
    end if
    stop
  end if
  return
end subroutine get_att_xtype
