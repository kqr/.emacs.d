--  Abstract :
--
--  call the GPS indentation engine with debug text
--
--  Copyright (C) 2017  All Rights Reserved.
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

pragma License (GPL);

with Ada.Text_IO; use Ada.Text_IO;
with Ada_Analyzer;
with Case_Handling;
with GNATCOLL.Symbols;
with Language;
procedure Debug_GPS_Indent is

   NL : constant Character := ASCII.LF;

   Buffer : constant String :=
     "procedure Bug_002 is begin" & NL &
     "Error_Report.Report_Error (Severity     =>" & NL &
     "Error_Report.Severe," & NL &
     "Description  => ""In theory the counterp_process..., "" &" & NL &
     """as they should not differ but they do... """ & NL &
     "& ""the indexes, so that we do not crash....""," & NL &
     "Title        => ""Curtain indexes should not differ but they do.""," & NL &
     "Caller_Scope => ""Curtain_Indexes_May_Differ"");" & NL &
     "end Bug_002;";

   procedure Replace_Cb
     (Line    : in Natural;
      First   : in Natural;
      Last    : in Natural;
      Replace : in String)
   is
      pragma Unreferenced (Last);
   begin
      --  Provide a place to put a debugger break when an indentation is computed.


      --  analyze calls replace_cb for ":", ":=" etc. We only
      --  want the leading spaces, for indentation.
      if Replace'Length > 0 and First = 1 then
         Put_Line (Natural'Image (Line) & Natural'Image (Replace'Length));
      end if;
   end Replace_Cb;

begin

   Ada_Analyzer.Analyze_Ada_Source
     (Buffer, GNATCOLL.Symbols.Allocate,
      Indent_Params          =>
        (Indent_Level        => 3,
         Indent_Continue     => 2,
         Indent_Decl         => 2,      -- no ada-mode equivalent
         Indent_Conditional  => 1,      -- no ada-mode equivalent
         Indent_Record       => 3,
         Indent_Case_Extra   => Language.Non_RM_Style,
         Casing_Policy       => Case_Handling.Disabled,
         Reserved_Casing     => Case_Handling.Unchanged,
         Ident_Casing        => Case_Handling.Unchanged,
         Format_Operators    => False,
         Use_Tabs            => False,
         Align_On_Colons     => False,
         Align_On_Arrows     => False,
         Align_Decl_On_Colon => False,
         Indent_Comments     => True,
         Stick_Comments      => False), -- no ada-mode equivalent.
      From                   => 1,
      To                     => 7,
      Replace                => Replace_Cb'Unrestricted_Access);

end Debug_GPS_Indent;
