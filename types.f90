type ncfile
  character(1000) :: path
  character(nf90_max_name) ::& !
             !name,& !
             newname,& !
             curname !
  integer ::& !
             !cmode,& !
             !ncid,& !
             initialsize,& !
             chunksize,& !
             fillmode,& !
             old_mode,& !
             h_minfree,& !
             v_align,& !
             v_minfree,& !
             r_align,& !
             !ndimensions,& !
             !nvariables,& !
             !nattributes,& !
             !unlimiteddimid,& !
             !formatnum,& !
             !len,& !
             !dimid,& !
             xtype,& !
             !varid,& !
             ndims,& !
             natts,& !
             attnum,& !
             ncid_in,& !
             varid_in,& !
             ncid_out,& !
             varid_out !

  integer, dimension(:), allocatable ::& !
             dimids,& !
             int_values,& !
             start,& !
             count,& !
             stride,& !
             map !

  real(8), dimension(:), allocatable ::& !
             real_values !

  logical, dimension(:), allocatable ::& !
             logical_values !

end type ncfile
