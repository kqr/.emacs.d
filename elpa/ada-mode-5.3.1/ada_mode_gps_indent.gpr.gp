--  Abstract :
--
--  build ada_mode_gps_indent
--
--  Copyright (C) 2014 Stephen Leake All Rights Reserved.
--
--  This program is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or (at
--  your option) any later version. This program is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without even
--  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--  PURPOSE. See the GNU General Public License for more details. You
--  should have received a copy of the GNU General Public License
--  distributed with this program; see file COPYING. If not, write to
--  the Free Software Foundation, 51 Franklin Street, Suite 500, Boston,
--  MA 02110-1335, USA.

with "gnat_util";
with "gnatcoll_iconv";
with "gnatcoll_sqlite";
#if HAVE_GNATCOLL_XREF="yes"
with "gnatcoll_xref";
#end if;
project Ada_Mode_GPS_Indent is
   for Main use ("ada_mode_gps_indent.adb");

   type GPS_Source_Type is ("ada-mode", "GPS");
   GPS_Source : GPS_Source_Type := External ("Ada_Mode_GPS_Source", "ada-mode");

   case GPS_Source is
   when "ada-mode" =>
      for Source_Dirs use (".", "gps_source");
   when "GPS" =>
      for Source_Dirs use
        (".",
         External ("GPS_ROOT") & "/common/core/src",
         External ("GPS_ROOT") & "/syntax/src",
         External ("GPS_ROOT") & "/language/src/"
        );
   end case;

   for Object_Dir use "obj";

   for Exec_Dir use ".";

   type Build_Type is ("Debug", "Normal");
   Build : Build_Type := External ("Ada_Mode_GPS_Indent_Build", "Normal");

   for Languages use ("Ada");

   package Compiler is

      Common_Switches :=
        ("-g",
         "-gnata",  -- assertions
         "-gnatfoqQ",
         "-gnatVa", -- validity checks
         "-gnaty3abcefhiklM120nprtx", -- no 'O'; gnatcoll-traces missing "overriding"
         "-gnateE", -- extra info in exceptions
         "-gnatwaBCeJL",
         "-fstack-check",
         "-gnat12");

      case Build is
         when "Debug" =>
            for Switches ("Ada") use Common_Switches & ("-O0");
         when "Normal" =>
            for Switches ("Ada") use Common_Switches & ("-O2", "-gnatn");
      end case;
   end Compiler;

   package Linker is
      for Default_Switches ("Ada") use ("-lpthread");
   end Linker;

   package Binder is
      for default_switches ("Ada") use ("-E"); -- symbolic traceback
   end Binder;

end Ada_Mode_GPS_Indent;
