subroutine get_att_xtype(&
     ncid,& !
     varid,& !
     xtype,& !
     name,& !
     len,& !
     value,& !
     verbose) !

  use netcdf
  use hamodule

  implicit none

  integer, intent(in) :: ncid, varid, xtype, len
  character(*), intent(in) :: name
  type(ncvalues), intent(out) :: value
  integer :: i, stdout = 6
  character(*), intent(in), optional :: verbose
  logical :: verbose_mode

  if(present(verbose) .eqv. .true. .and. trim(adjustl(verbose)) /= '') then
    verbose_mode = .true.
  else if(present(verbose) .eqv. .true. .and. trim(adjustl(verbose)) == '') then
    verbose_mode = .false.
  else if(present(verbose) .eqv. .false.) then
    verbose_mode = .false.
  end if

  if(verbose_mode .eqv. .true.)&
  write(stdout, '(a)') verbose//' begin get_att_xtype(): '//trim(adjustl(name))

  select case(xtype)
  case(0)
    ! 0
    if(verbose_mode .eqv. .true.) write(stdout, '(a)')&
    verbose//'   error: '//nc_xtype_info(xtype)//' not set'
    stop
  case(nf90_byte)
    if(verbose_mode .eqv. .true.) write(stdout, '(a)') verbose//'   xtype: '//nc_xtype_info(xtype)
    ! NC_BYTE: 8-bit signed integer
    allocate( value%byte_1(len) )
    if(verbose_mode .eqv. .true.) write(stdout, '(a)')&
    verbose//'   allocated: len = '//number_to_string(len)
    call nc_error_check(&
        'nc_get_att',&
        nf90_get_att(&
          ncid = ncid,&
          varid = varid,&
          name = name,&
          values = value%byte_1&
          )&
        )
    if(verbose_mode .eqv. .true.) write(stdout, '(a)') (verbose//'   value: '//number_to_string(i)//&
    ' = '//number_to_string(value%byte_1(i)), i = 1, len)
  case(nf90_ubyte)
    ! NC_UBYTE: 8-bit unsigned integer
    if(verbose_mode .eqv. .true.) write(stdout, '(a)')&
    verbose//'   error: '//nc_xtype_info(xtype)//' not set'
    stop
  case(nf90_char)
    if(verbose_mode .eqv. .true.) write(stdout, '(a)') verbose//'   xtype: '//nc_xtype_info(xtype)
    ! NC_CHAR: 8-bit character byte
    allocate( value%char_1(len) )
    if(verbose_mode .eqv. .true.) write(stdout, '(a)')&
    verbose//'   allocated: len = '//number_to_string(len)
    call nc_error_check(&
        'nc_get_att',&
        nf90_get_att(&
          ncid = ncid,&
          varid = varid,&
          name = name,&
          values = value%char_1(1)&
          )&
        )
    if(verbose_mode .eqv. .true.) then
      write(stdout, '(a)', advance = 'no')&
      verbose//'   value: '
      do i = 1, len
        write(stdout, '(a)', advance = 'no') value%char_1(i)
      end do
      write(stdout, '(a)')
    end if
  case(nf90_short)
    if(verbose_mode .eqv. .true.) write(stdout, '(a)')&
    verbose//'   xtype: '//nc_xtype_info(xtype)
    ! NC_SHORT: 16-bit signed integer
    allocate( value%short_1(len) )
    if(verbose_mode .eqv. .true.) write(stdout, '(a)')&
    verbose//'   allocated: len = '//number_to_string(len)
    call nc_error_check(&
        'nc_get_att',&
        nf90_get_att(&
          ncid = ncid,&
          varid = varid,&
          name = name,&
          values = value%short_1&
          )&
        )
    if(verbose_mode .eqv. .true.) write(stdout, '(a)')&
    (verbose//'   value '//number_to_string(i)//&
    ': '//number_to_string(value%short_1(i)), i = 1, len)
  case(nf90_ushort)
    ! NC_USHORT: 16-bit unsigned integer
    if(verbose_mode .eqv. .true.) write(stdout, '(a)')&
    verbose//'   error: '//nc_xtype_info(xtype)//' not set'
    stop
  case(nf90_int)
    if(verbose_mode .eqv. .true.) write(stdout, '(a)')&
    verbose//'   xtype: '//nc_xtype_info(xtype)
    ! NC_int: (NC_LONG): 32-bit signed integer
    allocate( value%int_1(len) )
    if(verbose_mode .eqv. .true.) write(stdout, '(a)')&
    verbose//'   allocated: len = '//number_to_string(len)
    call nc_error_check(&
        'nc_get_att',&
        nf90_get_att(&
          ncid = ncid,&
          varid = varid,&
          name = name,&
          values = value%int_1&
          )&
        )
    if(verbose_mode .eqv. .true.)& 
    write(stdout, '(a)') (verbose//'   value '//number_to_string(i)//&
    ': '//number_to_string(value%int_1(i)), i = 1, len)
  case(nf90_uint)
    ! NC_Uint: 32-bit unsigned integer
    if(verbose_mode .eqv. .true.) write(stdout, '(a)')&
    verbose//'   error: '//nc_xtype_info(xtype)//' not set'
    stop
  case(nf90_int64)
    if(verbose_mode .eqv. .true.) write(stdout, '(a)')&
    verbose//'   xtype: '//nc_xtype_info(xtype)
    ! NC_int64: 64-bit signed integer
    allocate( value%int64_1(len) )
    if(verbose_mode .eqv. .true.) write(stdout, '(a)')&
    verbose//'   allocated: len = '//number_to_string(len)
    call nc_error_check(&
         'nc_get_att',&
         nf90_get_att(&
           ncid = ncid,&
           varid = varid,&
           name = name,&
           values = value%int64_1&
           )&
         )
    if(verbose_mode .eqv. .true.)&
    write(stdout, '(a)') (verbose//'   value '//number_to_string(i)//&
    ': '//number_to_string(value%int64_1(i)), i = 1, len)
  case(nf90_uint64)
    ! NC_Uint64: 64-bit unsigned integer
    if(verbose_mode .eqv. .true.) write(stdout, '(a)')&
    verbose//'   error: '//nc_xtype_info(xtype)//' not set'
    stop
  case(nf90_float)
    if(verbose_mode .eqv. .true.) write(stdout, '(a)')&
    verbose//'   xtype: '//nc_xtype_info(xtype)
    ! NC_FLOAT: 32-bit floating point
    allocate( value%float_1(len) )
    if(verbose_mode .eqv. .true.) write(stdout, '(a)')&
    verbose//'   allocated: len = '//number_to_string(len)
    call nc_error_check(&
         'nc_get_att',&
         nf90_get_att(&
           ncid = ncid,&
           varid = varid,&
           name = name,&
           values = value%float_1&
           )&
         )
    if(verbose_mode .eqv. .true.)&
    write(stdout, '(a)') (verbose//'   value '//&
    number_to_string(i)//': '//number_to_string(value%float_1(i)), i = 1, len)
  case(nf90_double)
    if(verbose_mode .eqv. .true.) write(stdout, '(a)')&
    verbose//'   xtype: '//nc_xtype_info(xtype)
    ! NC_double: 64-bit floating point
    allocate( value%double_1(len) )
    if(verbose_mode .eqv. .true.) write(stdout, '(a)')&
    verbose//'   allocated: len = '//number_to_string(len)
    call nc_error_check(&
        'nc_get_att',&
        nf90_get_att(&
          ncid = ncid,&
          varid = varid,&
          name = name,&
          values = value%double_1&
          )&
        )
    if(verbose_mode .eqv. .true.) write(stdout, '(a)') (verbose//'   value '//&
    number_to_string(i)//': '//number_to_string(value%double_1(i)), i = 1, len)
  case(nf90_string)
    if(verbose_mode .eqv. .true.) write(stdout, '(a)')&
    verbose//'   xtype: '//nc_xtype_info(xtype)
    ! NC_STRING: variable length character string
    allocate( value%string_1(len) )
    if(verbose_mode .eqv. .true.) write(stdout, '(a)')&
    verbose//'   allocated: len = '//number_to_string(len)
    call nc_error_check(&
        'nc_get_att',&
        nf90_get_att(&
          ncid = ncid,&
          varid = varid,&
          name = name,&
          values = value%string_1(1)&
          )&
        )
    if(verbose_mode .eqv. .true.) write(stdout, '(a)')&
    verbose//'   value = '//value%string_1
  end select

  if(verbose_mode .eqv. .true.) write(stdout, '(a)') verbose//' end get_att_xtype()'

  return

end subroutine get_att_xtype
