--  Abstract :
--
--  build gpr_query
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

with "gnatcoll";
with "gnatcoll_sqlite";
#if HAVE_GNATCOLL_XREF="yes"
with "gnatcoll_xref";
#end if;
with "gnat_util";
project Gpr_query is
   for Main use ("gpr_query.adb");

   for Source_Dirs use (".");

   for Object_Dir use "obj";

   for Exec_Dir use ".";

   type Build_Type is ("Debug", "Normal");
   Build : Build_Type := External ("Gpr_Query_Build", "Normal");

   for Languages use ("Ada");

   package Compiler is

      Common_Switches :=
        ("-g",
         "-gnata",  -- assertions
         "-gnatfoqQ",
         "-gnatVa", -- validity checks
         "-gnaty3abcefhiklM120nOprtx",
         "-gnateE", -- extra info in exceptions
         "-gnatwaBCeJL",
         "-fstack-check",
         "-gnat12");

      case Build is
         when "Debug" =>
            for Default_Switches ("Ada") use Common_Switches & ("-O0");
         when "Normal" =>
            -- WORKAROUND (GNAT 7.2.1): adding -gnatn here gives errors about missing sources in installed gnatcoll.
            for Default_Switches ("Ada") use Common_Switches & ("-O2");
      end case;
   end Compiler;

   package Linker is
      for Default_Switches ("Ada") use ("-lpthread");
   end Linker;

   package Binder is
      for default_switches ("Ada") use ("-E"); -- symbolic traceback
   end Binder;

   package Install is
      for Mode use "usage";
   end Install;

end Gpr_query;
