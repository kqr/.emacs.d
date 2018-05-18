--  Abstract :
--
--  Back end for Emacs Ada mode indentation engine, using GPS
--  indentation code.
--
--  Copyright (C) 2014, 2017  All Rights Reserved.
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

with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Ada_Analyzer;
with Case_Handling;
with GNAT.OS_Lib;
with GNAT.Traceback.Symbolic;
with GNATCOLL.Symbols;
with Language;
with System.Storage_Elements;
procedure Ada_Mode_GPS_Indent is

   Programmer_Error : exception;

   Version : constant String := "1.001";
   --  Format: API_version . patch_level
   --
   --  API_version match ada-gps.el ada-gps-indent-exec-api-version.
   --
   --  Bump API_version for change in commands/response; bump patch
   --  level for patches to GPS indentation code.

   Prompt : constant String := "GPS_Indent> ";

   --  Indentation parameters to Analyze_Ada_Source
   --  Defaults for debugging.
   Indent_Level      : Natural               := 0;                     -- ada-indent
   Indent_Continue   : Natural               := 0;                     -- ada-broken-indent
   Indent_Case_Extra : Language.Indent_Style := Language.Non_RM_Style; -- derived from ada-indent-when
   Indent_Record     : Natural               := 0;                     -- ada-indent-record-rel-type

   procedure Usage
   is begin
      Put_Line ("Prompt is '" & Prompt & "'");
      Put_Line ("commands are case sensitive");
      Put_Line ("each command starts with a two-character decimal count of bytes in command");
      New_Line;
      Put_Line ("Commands: ");
      New_Line;
      Put_Line ("07version");
      Put_Line ("  outputs: <version>");
      New_Line;
      Put_Line ("NNset_params <ada-indent> <ada-indent-broken> <ada-indent-when>");
      New_Line;
      Put_Line ("NNcompute_indent <line> <text_byte_count><text>");
      Put_Line ("  first line is 1 (emacs convention)");
      Put_Line ("  text must be UTF8 encoded");
      Put_Line ("  outputs: <line number> <indent><newline>");
      Put_Line ("  no indent is 0 (emacs convention)");
      New_Line;
      Put_Line ("NNcompute_region_indent <start line> <end line> <text_byte_count><text>");
      Put_Line ("  outputs for each line: <line number> <indent><newline>");
      New_Line;
      Put_Line ("04exit");
   end Usage;

   procedure Read_Input
     (A : System.Address;
      N : Integer)
   is
      use System.Storage_Elements;

      B         : System.Address := A;
      Remaining : Integer        := N;
      Read      : Integer;
   begin
      --  WORKAROUND: with GNAT GPL 2016, GNAT.OS_Lib.Read does _not_
      --  wait for all N bytes or EOF; it returns as soon as it gets
      --  some bytes.
      --
      --  Note that with this loop we cannot detect EOF; that's ok for
      --  this application.
      loop
         Read := GNAT.OS_Lib.Read (GNAT.OS_Lib.Standin, B, Remaining);
         Remaining := Remaining - Read;
         exit when Remaining <= 0;
         B := B + Storage_Offset (Read);
      end loop;
   end Read_Input;

   function Get_Command_Length return Integer
   is
      Temp : aliased String (1 .. 2) := "  ";
   begin
      Read_Input (Temp'Address, 2);
      return Integer'Value (Temp);

   exception
   when Constraint_Error =>
      --  From Integer'Value
      raise Programmer_Error with "command byte count not provided; got '" & Temp & "'";
   end Get_Command_Length;

   function Get_Integer
     (Source : in     String;
      Last   : in out Integer)
     return Integer
   is
      use Ada.Exceptions;
      use Ada.Strings.Fixed;
      First : constant Integer := Last + 2; -- final char of previous word/integer, space
   begin
      Last := Index
        (Source  => Source,
         Pattern => " ",
         From    => First);

      if Last = 0 then
         Last := Source'Last;
      else
         Last := Last - 1;
      end if;

      return Integer'Value (Source (First .. Last));
   exception
   when E : others =>
      Put_Line ("bad integer '" & Source (First .. Source'Last) & "'");
      Put_Line ("Exception : " & Exception_Name (E));
      Put_Line (Exception_Message (E));
      raise;
   end Get_Integer;

   procedure Replace_Cb
     (Line    : in Natural;
      First   : in Natural;
      Last    : in Natural;
      Replace : in String)
   is
      pragma Unreferenced (Last);
   begin
      --  analyze calls replace_cb for ":", ":=" etc. We only
      --  want the leading spaces, for indentation.
      if Replace'Length > 0 and First = 1 then
         Put_Line (Natural'Image (Line) & Natural'Image (Replace'Length));
      end if;
   end Replace_Cb;

   procedure Compute_Indent
     (Start_Line : in Natural;
      End_Line   : in Natural;
      Byte_Count : in Integer)
   is
      use Ada.Strings.Unbounded;
      Buffer : aliased String_Access := new String (1 .. Byte_Count);
   begin
      Read_Input (Buffer.all'Address, Byte_Count);

      Ada_Analyzer.Analyze_Ada_Source
        (Buffer.all, GNATCOLL.Symbols.Allocate,
         Indent_Params          =>
           (Indent_Level        => Indent_Level,
            Indent_Continue     => Indent_Continue,
            Indent_Decl         => 2, -- no ada-mode equivalent
            Indent_Conditional  => 1, -- no ada-mode equivalent
            Indent_Record       => Indent_Record,
            Indent_Case_Extra   => Indent_Case_Extra,
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
         --  Stick_Comments = True causes comments to be not indented.

         From                   => Start_Line,
         To                     => End_Line,
         Replace                => Replace_Cb'Unrestricted_Access);

      Free (Buffer);
   exception
   when E : others =>
      declare
         use Ada.Exceptions;
         use GNAT.Traceback.Symbolic;
      begin
         Put_Line ("analyze failed on '" & Buffer.all & "'");
         Put_Line ("Exception : " & Exception_Name (E));
         Put_Line (Exception_Message (E));
         Put_Line (Symbolic_Traceback (E));
      end;
   end Compute_Indent;
begin

   Commands :
   loop
      Put (Prompt); Flush;

      declare
         use Ada.Strings.Fixed;
         Command_Length : constant Integer := Get_Command_Length;
         Command_Line   : aliased String (1 .. Command_Length);
         Last           : Integer;
      begin
         Read_Input (Command_Line'Address, Command_Length);
         Last := Index (Source => Command_Line, Pattern => " ");

         if Last = 0 then
            Last := Command_Line'Last;
         else
            Last := Last - 1;
         end if;

         if Command_Line (1 .. Last) = "exit" then
            exit Commands;

         elsif Command_Line (1 .. Last) = "version" then
            Put_Line (Version);

         elsif Command_Line (1 .. Last) = "set_params" then
            declare
               Ada_Indent                 : constant Integer := Get_Integer (Command_Line, Last);
               Ada_Broken_Indent          : constant Integer := Get_Integer (Command_Line, Last);
               Ada_Indent_When            : constant Integer := Get_Integer (Command_Line, Last);
               Ada_Indent_Record_Rel_Type : constant Integer := Get_Integer (Command_Line, Last);
            begin
               Indent_Level    := Ada_Indent;
               Indent_Continue := Ada_Broken_Indent;
               Indent_Record   := Ada_Indent_Record_Rel_Type;

               if Ada_Indent_When = 0 then
                  Indent_Case_Extra := Language.Non_RM_Style;
               else
                  Indent_Case_Extra := Language.RM_Style;
                  --  IMPROVEME: maybe Automatic if ada-indent-when < 0?
               end if;
            end;

         elsif Command_Line (1 .. Last) = "compute_indent" then
            declare
               Line       : constant Integer := Get_Integer (Command_Line, Last);
               Byte_Count : constant Integer := Get_Integer (Command_Line, Last);
            begin
               Compute_Indent (Line, Line, Byte_Count);
            end;

         elsif Command_Line (1 .. Last) = "compute_region_indent" then
            declare
               Start_Line : constant Integer := Get_Integer (Command_Line, Last);
               End_Line   : constant Integer := Get_Integer (Command_Line, Last);
               Byte_Count : constant Integer := Get_Integer (Command_Line, Last);
            begin
               Compute_Indent (Start_Line, End_Line, Byte_Count);
            end;

         else
            Put_Line ("unrecognized command '" & Command_Line & "'");
            Usage;
         end if;
      exception
      when E : Programmer_Error =>
         declare
            use Ada.Exceptions;
         begin
            Put_Line (Exception_Message (E));
            Usage;
            Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
            exit Commands;
         end;

      when E : others =>
         declare
            use Ada.Exceptions;
            use GNAT.Traceback.Symbolic;
         begin
            Put_Line ("Bad command '" & Command_Line & "'");
            Put_Line ("Exception : " & Exception_Name (E));
            Put_Line (Exception_Message (E));
            Put_Line (Symbolic_Traceback (E));
            Usage;
            Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
            exit Commands;
         end;
      end;
   end loop Commands;

end Ada_Mode_GPS_Indent;
