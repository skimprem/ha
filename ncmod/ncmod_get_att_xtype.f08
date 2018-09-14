subroutine get_att_xtype(ncid, varid, attribute, verbose_phrase)
  use netcdf
  use hamodule
  implicit none
  integer, intent(in) :: ncid, varid
  type(ncattribute), intent(inout) :: attribute 
  integer :: i, stdout = 6
  character(*), intent(in), optional :: verbose_phrase
  logical :: verbose_mode
  verbose_mode = .false.
  if(present(verbose_phrase) .eqv. .true.) then
    verbose_mode = .true.
  end if
  if(verbose_mode .eqv. .true.) then
    write(stdout, '(a)') verbose_phrase//' begin get_att_xtype(): '//attribute%name
  end if
  select case(attribute%xtype)
  case(0)
    ! 0
    if(verbose_mode .eqv. .true.) write(stdout, '(a)') then
      verbose_phrase//'   error: '//nc_xtype_info(attribute%xtype)//' not set'
    end if
    stop
  case(nf90_byte) ! NC_BYTE: 8-bit signed integer
    if(verbose_mode .eqv. .true.) then
      write(stdout, '(a)') verbose_phrase//'   xtype: '//nc_xtype_info(attribute%xtype)
    end if
    allocate( attribute%byte(len) )
    if(verbose_mode .eqv. .true.) then
      write(stdout, '(a)') verbose_phrase//'   allocated: len = '//number_to_string(attribute%len)
    end if
    call nc_error_check(&
        'nc_get_att',&
        nf90_get_att(&
          ncid = ncid,&
          varid = varid,&
          name = attribute%name,&
          values = attribute%byte&
          )&
        )
    if(verbose_mode .eqv. .true.) then
      write(stdout, '(a)') (verbose_phrase//'   value: '//number_to_string(i)//&
      ' = '//number_to_string(attribute%byte(i)), i = 1, attribute%len)
    end if
  case(nf90_ubyte) ! NC_UBYTE: 8-bit unsigned integer
    if(verbose_mode .eqv. .true.) then
      write(stdout, '(a)') verbose_phrase//'   error: '//nc_xtype_info(attribute%xtype)//' not set'
    end if
    stop
  case(nf90_char) ! NC_CHAR: 8-bit character byte
    if(verbose_mode .eqv. .true.) then
      write(stdout, '(a)') verbose_phrase//'   xtype: '//nc_xtype_info(attribute%xtype)
    end if
    allocate( attribute%char(attribute%len) )
    if(verbose_mode .eqv. .true.) then
      write(stdout, '(a)') verbose_phrase//'   allocated: len = '//number_to_string(attribute%len)
    call nc_error_check(&
        'nc_get_att',&
        nf90_get_att(&
          ncid = ncid,&
          varid = varid,&
          name = attribute%name,&
          values = attribute%char(1)&
          )&
        )
    if(verbose_mode .eqv. .true.) then
      write(stdout, '(a)', advance = 'no') verbose_phrase//'   value: '
      do i = 1, attribute%len
        write(stdout, '(a)', advance = 'no') attribute%char(i)
      end do
      write(stdout, '(a)')
    end if
  case(nf90_short) ! NC_SHORT: 16-bit signed integer
    if(verbose_mode .eqv. .true.) write(stdout, '(a)') then
      verbose_phrase//'   xtype: '//nc_xtype_info(attribute%xtype)
    end if
    allocate( attribute%short(attribute%len) )
    if(verbose_mode .eqv. .true.) then
      write(stdout, '(a)') verbose_phrase//'   allocated: len = '//number_to_string(attribute%len)
    end if
    call nc_error_check(&
        'nc_get_att',&
        nf90_get_att(&
          ncid = ncid,&
          varid = varid,&
          name = attribute%name,&
          values = attribute%short&
          )&
        )
    if(verbose_mode .eqv. .true.) then
      write(stdout, '(a)') (verbose_phrase//'   value '//number_to_string(i)//&
      ': '//number_to_string(attribute%short(i)), i = 1, attribute%len)
    end if
  case(nf90_ushort) ! NC_USHORT: 16-bit unsigned integer
    if(verbose_mode .eqv. .true.) then
      write(stdout, '(a)') verbose_phrase//'   error: '//nc_xtype_info(attribute%xtype)//' not set'
    end if
    stop
  case(nf90_int) ! NC_int: (NC_LONG): 32-bit signed integer
    if(verbose_mode .eqv. .true.) then
      write(stdout, '(a)') verbose_phrase//'   xtype: '//nc_xtype_info(attribute%xtype)
    end if
    allocate( attribute%int(attribute%len) )
    if(verbose_mode .eqv. .true.) then
      write(stdout, '(a)') verbose_phrase//'   allocated: len = '//number_to_string(attribute%len)
    end if
    call nc_error_check(&
        'nc_get_att',&
        nf90_get_att(&
          ncid = ncid,&
          varid = varid,&
          name = attribute%name,&
          values = attribute%int&
          )&
        )
    if(verbose_mode .eqv. .true.) then 
      write(stdout, '(a)') (verbose_phrase//'   value '//number_to_string(i)//&
      ': '//number_to_string(attribute%int(i)), i = 1, attribute%len)
    end if
  case(nf90_uint) ! NC_Uint: 32-bit unsigned integer
    if(verbose_mode .eqv. .true.) then
      write(stdout, '(a)') verbose_phrase//'   error: '//nc_xtype_info(attribute%xtype)//' not set'
    end if
    stop
  case(nf90_int64) ! NC_int64: 64-bit signed integer
    if(verbose_mode .eqv. .true.) then
      write(stdout, '(a)') verbose_phrase//'   xtype: '//nc_xtype_info(attribute%xtype)
    end if
    allocate( attribute%int64(attribute%len) )
    if(verbose_mode .eqv. .true.) then
      write(stdout, '(a)') verbose_phrase//'   allocated: len = '//number_to_string(attribute%len)
    end if
    call nc_error_check(&
         'nc_get_att',&
         nf90_get_att(&
           ncid = ncid,&
           varid = varid,&
           name = attribute%name,&
           values = attribute%int64&
           )&
         )
    if(verbose_mode .eqv. .true.) then
      write(stdout, '(a)') (verbose_phrase//'   value '//number_to_string(i)//&
      ': '//number_to_string(attribute%int64(i)), i = 1, attribute%len)
    end if
  case(nf90_uint64) ! NC_Uint64: 64-bit unsigned integer
    if(verbose_mode .eqv. .true.) then
      write(stdout, '(a)') verbose_phrase//'   error: '//nc_xtype_info(attributextype)//' not set'
    end if
    stop
  case(nf90_float) ! NC_FLOAT: 32-bit floating point
    if(verbose_mode .eqv. .true.) then
      write(stdout, '(a)') verbose_phrase//'   xtype: '//nc_xtype_info(attribute%xtype)
    end if
    allocate( attribute%float(attribute%len) )
    if(verbose_mode .eqv. .true.) then
      write(stdout, '(a)') verbose_phrase//'   allocated: len = '//number_to_string(attribute%len)
    end if
    call nc_error_check(&
         'nc_get_att',&
         nf90_get_att(&
           ncid = ncid,&
           varid = varid,&
           name = attribute%name,&
           values = attribute%float&
           )&
         )
    if(verbose_mode .eqv. .true.) then
      write(stdout, '(a)') (verbose_phrase//'   value '//&
      number_to_string(i)//': '//number_to_string(attribute%float(i)), i = 1, attribute%len)
    end if
  case(nf90_double) ! NC_double: 64-bit floating point
    if(verbose_mode .eqv. .true.) then
      write(stdout, '(a)') verbose_phrase//'   xtype: '//nc_xtype_info(attribute%xtype)
    end if
    allocate( attribute%double(attribute%len) )
    if(verbose_mode .eqv. .true.) then
      write(stdout, '(a)') verbose_phrase//'   allocated: len = '//number_to_string(attribute%len)
    end if
    call nc_error_check(&
        'nc_get_att',&
        nf90_get_att(&
          ncid = ncid,&
          varid = varid,&
          name = attribute%name,&
          values = attribute%double&
          )&
        )
    if(verbose_mode .eqv. .true.) then
      write(stdout, '(a)') (verbose_phrase//'   value '//&
      number_to_string(i)//': '//number_to_string(attribute%double(i)), i = 1, attribute%len)
    end if
  case(nf90_string) ! NC_STRING: variable length character string
    if(verbose_mode .eqv. .true.) then
      write(stdout, '(a)') verbose_phrase//'   xtype: '//nc_xtype_info(attribute%xtype)
    end if
    allocate( attribute%string(attribute%len) )
    if(verbose_mode .eqv. .true.) then
      write(stdout, '(a)') verbose_phrase//'   allocated: len = '//number_to_string(attribute%len)
    end if
    call nc_error_check(&
        'nc_get_att',&
        nf90_get_att(&
          ncid = ncid,&
          varid = varid,&
          name = attribute%name,&
          values = attribute%string(1)&
          )&
        )
    if(verbose_mode .eqv. .true.) then
      write(stdout, '(a)') verbose_phrase//'   value = '//attribute%string
    end if
  end select
  if(verbose_mode .eqv. .true.) then
    write(stdout, '(a)') verbose_phrase//' end get_att_xtype()'
  end if
  return
end subroutine get_att_xtype
