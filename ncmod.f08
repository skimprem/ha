module ncmodule

  use netcdf

  include "ncmod_types.f90"

contains

  include "ncmod_get_att_xtype.f90"
  include "ncmod_get_var_xtype.f90"
  include "ncmod_nc_xtype_info.f90"
  include "ncmod_nc_error_check.f90"
  include "ncmod_nc_reader.f90"
  include "ncmod_nc_print_info.f90"
  include "ncmod_nc_print_data.f90"
  include "ncmod_nc_value_print.f90"
  include "ncmod_nc_allocate.f90"

end module ncmodule
