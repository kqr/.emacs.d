# Build and install executables for Ada mode.

# In December 2016, GNATCOLL changed its Xref interface. First, the
# GPR was split out; and second, one of the subprogram
# interfaces.changed.
#
# Determine whether the split-out gnatcoll_xref.gpr is available.
echo 'with "gnatcoll_xref"; abstract project check_xref is end check_xref;' > check_xref.gpr;
gprbuild -P check_xref.gpr > /dev/null 2>&1;
if test $? -eq 0 ; then HAVE_GNATCOLL_XREF="yes"; else HAVE_GNATCOLL_XREF="no"; fi

echo "HAVE_GNATCOLL_XREF=$HAVE_GNATCOLL_XREF"

gnatprep -DHAVE_GNATCOLL_XREF=$HAVE_GNATCOLL_XREF gpr_query-process_refresh.adb.gp gpr_query-process_refresh.adb
gnatprep -DHAVE_GNATCOLL_XREF=$HAVE_GNATCOLL_XREF gpr_query.gpr.gp gpr_query.gpr

gprbuild -p -P gpr_query.gpr
gprinstall -P gpr_query.gpr

gnatprep -DHAVE_GNATCOLL_XREF=$HAVE_GNATCOLL_XREF ada_mode_gps_indent.gpr.gp ada_mode_gps_indent.gpr
gprbuild -p -P ada_mode_gps_indent.gpr
gprinstall -p -P ada_mode_gps_indent.gpr

# end of file
