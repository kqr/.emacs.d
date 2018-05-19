--  Abstract :
--
--  Support Emacs Ada mode and gpr-query minor mode queries about
--  GNAT projects and cross reference data
--
--  requires gnatcoll 1.7w 20140330, gnat 7.2.1
--
--  Copyright (C) 2014-2017 Free Software Foundation All Rights Reserved.
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

with Ada.Characters.Handling;
with Ada.Command_Line;
with Ada.Directories;
with Ada.Environment_Variables;
with Ada.Exceptions.Traceback;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with GNAT.Command_Line;
with GNAT.Directory_Operations;
with GNAT.OS_Lib;
with GNAT.Strings;
with GNAT.Traceback.Symbolic;
with GNATCOLL.Arg_Lists;
with GNATCOLL.Paragraph_Filling;
with GNATCOLL.Projects;
with GNATCOLL.SQL.Sqlite;
with GNATCOLL.Traces; use GNATCOLL.Traces;
with GNATCOLL.Utils;
with GNATCOLL.VFS;
with GNATCOLL.VFS_Utils;
with GNATCOLL.Xref;
procedure Gpr_Query is
   use GNATCOLL;

   Me : constant GNATCOLL.Traces.Trace_Handle := GNATCOLL.Traces.Create ("gpr_query");

   Db_Error        : exception;
   Invalid_Command : exception;

   function "+" (Item : in Ada.Strings.Unbounded.Unbounded_String) return String
     renames Ada.Strings.Unbounded.To_String;

   function "+" (Item : in GNATCOLL.VFS.Filesystem_String) return String
   is begin
      return String (Item);
   end "+";

   procedure Process_Line (Line : String);
   --  Process a full line of commands.
   --  Raise Invalid_Command when the command is invalid.

   function Get_Entity (Arg : String) return GNATCOLL.Xref.Entity_Information;
   --  Return the entity matching the "name:file:line:column" argument

   type My_Xref_Database is new GNATCOLL.Xref.Xref_Database with null record;
   --  Derived so we can override Image to output full paths

   overriding function Image (Self : My_Xref_Database; File : GNATCOLL.VFS.Virtual_File) return String;
   function Image (Self : GNATCOLL.Xref.Entity_Information) return String;
   --  Return a display version of the argument

   Xref              : aliased My_Xref_Database;
   Env               : GNATCOLL.Projects.Project_Environment_Access;
   Tree              : aliased GNATCOLL.Projects.Project_Tree;
   Previous_Progress : Natural                          := 0;
   Progress_Reporter : access procedure (Current, Total : Integer) := null;

   --  Subprogram specs for subprograms used before bodies
   procedure Check_Arg_Count (Args : in GNATCOLL.Arg_Lists.Arg_List; Expected : in Integer);
   procedure Dump (Curs : in out GNATCOLL.Xref.Entities_Cursor'Class);
   procedure Dump (Refs : in out GNATCOLL.Xref.References_Cursor'Class);
   --  Display the results of a query

   procedure Put (Item : GNATCOLL.VFS.File_Array);

   generic
      with function Compute
        (Self   : in GNATCOLL.Xref.Xref_Database'Class;
         Entity : in GNATCOLL.Xref.Entity_Information)
        return GNATCOLL.Xref.Entity_Information;
   procedure Process_Command_Single (Args : GNATCOLL.Arg_Lists.Arg_List);
   --  Get the entity identified by Args, which must contain a single
   --  argument. Then call Compute, and output the result.
   --
   --  Appropriate for queries that return a single entity result.

   procedure Process_Command_Single (Args : GNATCOLL.Arg_Lists.Arg_List)
   is
      use GNATCOLL.Arg_Lists;
      use GNATCOLL.Xref;

      Entity : Entity_Information;
      Comp   : Entity_Information;
   begin
      Check_Arg_Count (Args, 1);

      Entity := Get_Entity (Nth_Arg (Args, 1));
      Comp := Compute (Xref, Entity);
      if Comp /= No_Entity then
         Ada.Text_IO.Put_Line (Image (Comp));
      end if;
   end Process_Command_Single;

   generic
      with procedure Compute
        (Self   : in     GNATCOLL.Xref.Xref_Database'Class;
         Entity : in     GNATCOLL.Xref.Entity_Information;
         Cursor :    out GNATCOLL.Xref.Entities_Cursor'Class);
   procedure Process_Command_Multiple (Args : GNATCOLL.Arg_Lists.Arg_List);

   procedure Process_Command_Multiple (Args : GNATCOLL.Arg_Lists.Arg_List)
   is
      use GNATCOLL.Arg_Lists;
      use GNATCOLL.Xref;

      Entity      : Entity_Information;
      Descendants : Recursive_Entities_Cursor;

      --  Apparently a generic formal parameter cannot match a subprogram access type, so we need this:
      procedure Do_Compute
        (Self   : in     GNATCOLL.Xref.Xref_Database'Class;
         Entity : in     GNATCOLL.Xref.Entity_Information;
         Cursor :    out GNATCOLL.Xref.Entities_Cursor'Class)
      is begin
         Compute (Self, Entity, Cursor);
      end Do_Compute;
   begin
      Check_Arg_Count (Args, 1);

      Entity := Get_Entity (Nth_Arg (Args, 1));

      Recursive
        (Self    => Xref'Unchecked_Access,
         Entity  => Entity,
         Compute => Do_Compute'Unrestricted_Access,
         Cursor  => Descendants);

      Dump (Descendants);

   end Process_Command_Multiple;

   --  Command procedures; Args is the command line.
   --
   --  Infrastructure commands
   procedure Process_Help (Args : GNATCOLL.Arg_Lists.Arg_List);
   procedure Process_Refresh (Args : GNATCOLL.Arg_Lists.Arg_List);

   --  Queries; alphabetical
   procedure Process_Overridden is new Process_Command_Single (GNATCOLL.Xref.Overrides);
   procedure Process_Overriding is new Process_Command_Multiple (GNATCOLL.Xref.Overridden_By);
   procedure Process_Parent_Types is new Process_Command_Multiple (GNATCOLL.Xref.Parent_Types);
   procedure Process_Project_Path (Args : GNATCOLL.Arg_Lists.Arg_List);
   procedure Process_Refs (Args : GNATCOLL.Arg_Lists.Arg_List);
   procedure Process_Source_Dirs (Args : GNATCOLL.Arg_Lists.Arg_List);

   type Command_Descr is record
      Name    : GNAT.Strings.String_Access;
      Args    : GNAT.Strings.String_Access;
      Help    : GNAT.Strings.String_Access;
      Handler : access procedure (Args : GNATCOLL.Arg_Lists.Arg_List);
   end record;

   Commands : constant array (Natural range <>) of Command_Descr :=
     ((new String'("help"),
       new String'("[command or variable name]"),
       new String'("Display the list of commands and their syntax."),
       Process_Help'Access),

      (new String'("refresh"),
       null,
       new String'("Refresh the contents of the xref database."),
       Process_Refresh'Access),

      --  queries

      (new String'("overridden"),
       new String'("name:file:line:column"),
       new String'("The entity that is overridden by the parameter"),
       Process_Overridden'Access),

      (new String'("overriding"),
       new String'("name:file:line:column"),
       new String'("The entities that override the parameter"),
       Process_Overriding'Access),

      (new String'("parent_types"),
       new String'("name:file:line:column"),
       new String'("The parent types of the entity."),
       Process_Parent_Types'Access),

      (new String'("project_path"),
       null,
       new String'("The project search path."),
       Process_Project_Path'Access),

      (new String'("refs"),
       new String'("name:file:line:column"),
       new String'("All known references to the entity."),
       Process_Refs'Access),

      (new String'("source_dirs"),
       null,
       new String'("The project source directories, recursively."),
       Process_Source_Dirs'Access));

   --  Parsed command line info
   Cmdline              : GNAT.Command_Line.Command_Line_Configuration;
   Commands_From_Switch : aliased GNAT.Strings.String_Access;
   DB_Name              : aliased GNAT.Strings.String_Access := new String'("gpr_query.db");
   Force_Refresh        : aliased Boolean;
   Nightly_DB_Name      : aliased GNAT.Strings.String_Access;
   Show_Progress        : aliased Boolean;
   Project_Name         : aliased GNAT.Strings.String_Access;
   Traces_Config_File   : aliased GNAT.Strings.String_Access;
   Gpr_Config_File      : aliased GNAT.Strings.String_Access;
   ALI_Encoding         : aliased GNAT.Strings.String_Access := new String'("");

   ----------
   --  Procedure bodies, alphabetical

   procedure Display_Progress (Current, Total : Integer) is
      Now : constant Integer := Integer (Float'Floor
        (Float (Current) / Float (Total) * 100.0));
   begin
      if Now /= Previous_Progress then
         Ada.Text_IO.Put_Line
           ("completed" & Current'Img
              & " out of" & Total'Img
              & " (" & GNATCOLL.Utils.Image (Now, Min_Width => 0) & "%)...");
         Previous_Progress := Now;
      end if;
   end Display_Progress;

   procedure Dump (Curs : in out GNATCOLL.Xref.Entities_Cursor'Class)
   is
      use GNATCOLL.Xref;
   begin
      while Curs.Has_Element loop
         Ada.Text_IO.Put_Line (Image (Curs.Element));
         Curs.Next;
      end loop;
   end Dump;

   procedure Dump (Refs : in out GNATCOLL.Xref.References_Cursor'Class)
   is
      use GNATCOLL.Xref;
   begin
      while Has_Element (Refs) loop
         declare
            Ref : constant Entity_Reference := Refs.Element;
         begin
            Ada.Text_IO.Put_Line (Xref.Image (Ref) & " (" & (+Ref.Kind) & ")");
         end;
         Next (Refs);
      end loop;
   end Dump;

   function Get_Entity (Arg : String) return GNATCOLL.Xref.Entity_Information
   is
      use GNAT.Directory_Operations;
      use GNATCOLL.Xref;

      Words  : GNAT.Strings.String_List_Access := GNATCOLL.Utils.Split (Arg, On => ':');
      Ref    : GNATCOLL.Xref.Entity_Reference;
   begin
      case Words'Length is
      when 4         =>
         Ref := Xref.Get_Entity
           (Name     => Words (Words'First).all,
            File     => Format_Pathname
              (Style => UNIX,
               Path  => Words (Words'First + 1).all),
            Project  => GNATCOLL.Projects.No_Project,
            Line     => Integer'Value (Words (Words'First + 2).all),
            Column   => Visible_Column
              (Integer'Value (Words (Words'First + 3).all)));

      when 3 =>
         Ref := Xref.Get_Entity
           (Name     => Words (Words'First).all,
            File     => Format_Pathname
              (Style => UNIX,
               Path  => Words (Words'First + 1).all),
            Project  => GNATCOLL.Projects.No_Project,
            Line     => Integer'Value (Words (Words'First + 2).all));

      when 2 =>
         Ref := Xref.Get_Entity
           (Name     => Words (Words'First).all,
            File     => Format_Pathname
              (Style => UNIX,
               Path  => Words (Words'First + 1).all),
            Project  => GNATCOLL.Projects.No_Project);

      --  Xref.Get_Entity treats 'File => ""' as searching for pre-defined entities such as "Integer".

      when others =>
         raise Invalid_Command with "Invalid parameter '" & Arg & "', expecting name:file:line:column";
      end case;

      GNAT.Strings.Free (Words);

      if Ref.Entity = GNATCOLL.Xref.No_Entity then
         Ada.Text_IO.Put_Line ("Error: entity not found '" & Arg & "'");

      elsif GNATCOLL.Xref.Is_Fuzzy_Match (Ref.Entity) then
         Ada.Text_IO.Put_Line ("warning: fuzzy match for the entity");
      end if;

      return Ref.Entity;
   end Get_Entity;

   overriding function Image (Self : My_Xref_Database; File : GNATCOLL.VFS.Virtual_File) return String
   is
      pragma Unreferenced (Self);
   begin
      return File.Display_Full_Name;
   end Image;

   function Image (Self : GNATCOLL.Xref.Entity_Information) return String
   is
      use GNATCOLL.Xref;
   begin
      if Self = No_Entity then
         return "Unknown entity";
      else
         declare
            Decl : constant Entity_Declaration := Xref.Declaration (Self);
         begin
            if Is_Predefined_Entity (Decl) then
               return "predefined entity: " & (+Decl.Name);
            else
               return Xref.Image (Decl.Location);
            end if;
         end;
      end if;
   end Image;

   procedure Check_Arg_Count (Args : in GNATCOLL.Arg_Lists.Arg_List; Expected : in Integer)
   is
      Count : constant Integer := GNATCOLL.Arg_Lists.Args_Length (Args);
   begin
      if Count /= Expected then
         raise Invalid_Command with "Invalid number of arguments" & Integer'Image (Count) &
           "; expecting" & Integer'Image (Expected);
      end if;
   end Check_Arg_Count;

   procedure Process_Help (Args : GNATCOLL.Arg_Lists.Arg_List)
   is
      use Ada.Text_IO;
      use GNATCOLL.Arg_Lists;
      use type GNAT.Strings.String_Access;
   begin
      for C in Commands'Range loop
         if Args_Length (Args) <= 0 -- Empty_Command_Line returns -1
           or else Nth_Arg (Args, 1) = Commands (C).Name.all
         then
            Put ("  " & Commands (C).Name.all);
            if Commands (C).Args = null then
               New_Line;
            else
               Put_Line (" " & Commands (C).Args.all);
            end if;

            Put
              (Ada.Strings.Unbounded.To_String
                 (GNATCOLL.Paragraph_Filling.Knuth_Fill
                    (Commands (C).Help.all,
                     Max_Line_Length => 70,
                     Line_Prefix     => "      ")));
         end if;
      end loop;
      New_Line;
      Put_Line ("'exit' to quit");
   end Process_Help;

   procedure Process_Line (Line : String)
   is
      Expr : GNAT.Strings.String_List_Access;
   begin
      if Ada.Strings.Fixed.Trim (Line, Ada.Strings.Both) = "" then
         return;
      end if;

      Expr := GNATCOLL.Utils.Split (Line, On => ';');

      for C in Expr'Range loop
         if Ada.Strings.Fixed.Trim (Expr (C).all, Ada.Strings.Both) = "" then
            null;

         else
            declare
               use GNATCOLL.Arg_Lists;
               List  : constant Arg_List := Parse_String (Expr (C).all, Mode => Separate_Args);
               Cmd   : constant String   := Ada.Characters.Handling.To_Lower (Get_Command (List));
               Found : Boolean           := False;
            begin
               for Co in Commands'Range loop
                  if Commands (Co).Name.all = Cmd then
                     Commands (Co).Handler (List);
                     Found := True;
                     exit;
                  end if;
               end loop;

               if not Found then
                  raise Invalid_Command with "Invalid command: '" & Cmd & "'";
               end if;
            end;

         end if;
      end loop;

      GNAT.Strings.Free (Expr);
   end Process_Line;

   procedure Process_Project_Path (Args : GNATCOLL.Arg_Lists.Arg_List)
   is
      pragma Unreferenced (Args);
      Dirs : constant GNATCOLL.VFS.File_Array := GNATCOLL.Projects.Predefined_Project_Path (Env.all);
   begin
      Put (Dirs);
   end Process_Project_Path;

   procedure Process_Refresh (Args : GNATCOLL.Arg_Lists.Arg_List) is separate;
   --  Requires different code for GNAT GPL 2016 vs 2017

   procedure Process_Refs (Args : GNATCOLL.Arg_Lists.Arg_List)
   is
      use GNATCOLL.Arg_Lists;
   begin
      Check_Arg_Count (Args, 1);

      declare
         use GNATCOLL.Xref;
         Entity  : constant Entity_Information := Get_Entity (Nth_Arg (Args, 1));
         Refs    : References_Cursor;
      begin
         Xref.References (Entity, Cursor => Refs);
         Dump (Refs);
      end;
   end Process_Refs;

   procedure Process_Source_Dirs (Args : GNATCOLL.Arg_Lists.Arg_List)
   is
      pragma Unreferenced (Args);
      use GNATCOLL.VFS;
      use GNATCOLL.Projects;

      Dirs : constant File_Array := Source_Dirs
        (Project   => Tree.Root_Project,
         Recursive => True) &
        Predefined_Source_Path (Env.all);
   begin
      Put (Dirs);
   end Process_Source_Dirs;

   procedure Put (Item : GNATCOLL.VFS.File_Array)
   is
      use GNATCOLL.VFS;
   begin
      for I in Item'Range loop
         Ada.Text_IO.Put_Line (+Full_Name (Item (I)));
      end loop;
   end Put;

begin
   declare
      use GNAT.Command_Line;
   begin
      Set_Usage
        (Cmdline,
         Help => "Query project info and cross-references on source code. See ada-mode docs for more help.");

      --  Switch variable alphabetic order
      Define_Switch
        (Cmdline,
         Output      => ALI_Encoding'Access,
         Long_Switch => "--encoding=",
         Switch      => "-e=",
         Help        => "The character encoding used for source and ALI files");
      Define_Switch
        (Cmdline,
         Output      => Commands_From_Switch'Access,
         Switch      => "-c:",
         Long_Switch => "--command=",
         Help        => "Execute the commands from ARG, and exit");
      Define_Switch
        (Cmdline,
         Output      => DB_Name'Access,
         Long_Switch => "--db=",
         Help        => "Specifies the name of the database (or ':memory:')");
      Define_Switch
        (Cmdline,
         Output      => Force_Refresh'Access,
         Long_Switch => "--force_refresh",
         Help        => "Force rebuilding the database.");
      Define_Switch
        (Cmdline,
         Output      => Gpr_Config_File'Access,
         Long_Switch => "--autoconf=",
         Help        => "Specify the gpr configuration file (.cgpr)");
      Define_Switch
        (Cmdline,
         Output      => Nightly_DB_Name'Access,
         Long_Switch => "--nightlydb=",
         Help        => "Specifies the name of a prebuilt database");
      Define_Switch
        (Cmdline,
         Output      => Project_Name'Access,
         Switch      => "-P:",
         Long_Switch => "--project=",
         Help        => "Load the given project (mandatory)");
      Define_Switch
        (Cmdline,
         Output      => Show_Progress'Access,
         Long_Switch => "--display_progress",
         Switch      => "-d",
         Help        => "Show progress as LI files are parsed");
      Define_Switch
        (Cmdline,
         Output      => Traces_Config_File'Access,
         Long_Switch => "--tracefile=",
         Help        => "Specify a traces configuration file, set projects lib verbose");

      Getopt (Cmdline, Callback => null);
   end;

   if Project_Name.all = "" then
      Ada.Text_IO.Put_Line ("No project file specified");
      GNAT.Command_Line.Display_Help (Cmdline);
      return;
   end if;

   --  Only trace if user specifies --tracefile
   if Traces_Config_File.all /= "" and then GNAT.OS_Lib.Is_Regular_File (Traces_Config_File.all) then
      GNATCOLL.Traces.Parse_Config_File
        (Filename         => Traces_Config_File.all,
         Force_Activation => False);
      Trace (Me, "trace enabled");
   end if;

   GNATCOLL.Projects.Initialize (Env); -- for register_default_language

   if Gpr_Config_File.all /= "" and then GNAT.OS_Lib.Is_Regular_File (Gpr_Config_File.all) then
      Env.Set_Config_File
        (GNATCOLL.VFS.Create_From_UTF8
           (GNAT.OS_Lib.Normalize_Pathname
              (Name => Gpr_Config_File.all,
               Directory => GNAT.Directory_Operations.Get_Current_Dir)));
   else
      --  Apparently Ada language extensions are already registered (sigh)

      Env.Register_Default_Language_Extension
        (Language_Name       => "C",
         Default_Spec_Suffix => ".h",
         Default_Body_Suffix => ".c");

      Env.Register_Default_Language_Extension
        (Language_Name       => "C++",
         Default_Spec_Suffix => ".hh",
         Default_Body_Suffix => ".cpp");

   end if;

   declare
      use Ada.Environment_Variables;
      use Ada.Text_IO;
      use GNATCOLL.VFS;
      use GNATCOLL.VFS_Utils;
      use GNAT.Directory_Operations;
      use type GNAT.Strings.String_Access;

      Gpr_Project_Path : constant String :=
        (if Exists ("GPR_PROJECT_PATH") then Ada.Directories.Current_Directory &
           GNAT.OS_Lib.Path_Separator &
           Value ("GPR_PROJECT_PATH")
         else Ada.Directories.Current_Directory);

      Path : constant Virtual_File := -- must be an absolute file name
        (if Is_Absolute_Path (+Project_Name.all) then
           Create_From_UTF8 (Project_Name.all, Normalize => True)
         else
           Locate_Regular_File (+Project_Name.all, From_Path (+Gpr_Project_Path)));
   begin
      if not Path.Is_Regular_File then
         Put (Project_Name.all & ": not found on path " & Gpr_Project_Path);
         Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
         return;
      end if;

      Trace (Me, "using project file " & (+Path.Full_Name));

      if Show_Progress then
         Progress_Reporter := Display_Progress'Unrestricted_Access;
      end if;

      begin
         --  Recompute_View => True registers all the source files
         --  (among other things), so we will know that a .[ag]li
         --  belongs to this project
         Tree.Load
           (Path, Env,
            Errors         => Ada.Text_IO.Put_Line'Access,
            Recompute_View => True);
      exception
      when GNATCOLL.Projects.Invalid_Project =>
         Ada.Text_IO.Put_Line ("project search path:");
         Put (GNATCOLL.Projects.Predefined_Project_Path (Env.all));
         raise GNATCOLL.Projects.Invalid_Project with +Path.Full_Name & ": invalid project";
      end;
   end;

   if DB_Name.all /= ":memory:" then
      declare
         use GNATCOLL.VFS;

         N    : constant String := DB_Name.all;
         Temp : Virtual_File    := Tree.Root_Project.Object_Dir;
         Dir2 : Virtual_File;
      begin
         GNAT.Strings.Free (DB_Name);

         --  If the project does not have an object directory, create
         --  the database in the directory containing the project file.
         if Temp = No_File then
            Temp := Tree.Root_Project.Project_Path.Dir;
         end if;

         Temp := Create_From_Base (Base_Dir => Temp.Full_Name.all, Base_Name => +N);
         Dir2 := Create (Temp.Dir_Name);

         if not Dir2.Is_Directory then
            Dir2.Make_Dir (Recursive => True);
         end if;

         DB_Name := new String'(Temp.Display_Full_Name);
      end;
   end if;

   declare
      use type GNAT.Strings.String_Access;
      Error : GNAT.Strings.String_Access;
   begin
      Trace (Me, "using database " & DB_Name.all);

      Setup_DB
        (Self  => Xref,
         Tree  => Tree'Unchecked_Access,
         DB    => GNATCOLL.SQL.Sqlite.Setup (Database => DB_Name.all),
         Error => Error);

      if Error /= null then
         --  old db schema
         raise Db_Error with Error.all;
      end if;
   end;

   Process_Refresh (GNATCOLL.Arg_Lists.Empty_Command_Line);

   if Commands_From_Switch.all /= "" then
      Process_Line (Commands_From_Switch.all);
      return;
   end if;

   loop
      Ada.Text_IO.Put (">>> ");
      declare
         Input : constant String := Ada.Text_IO.Get_Line;
      begin
         exit when Input = "exit";
         Process_Line (Input);
      exception
      when E : Invalid_Command =>
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (E));
         Process_Help (GNATCOLL.Arg_Lists.Empty_Command_Line);
      end;
   end loop;

exception
when E : GNATCOLL.Projects.Invalid_Project =>
   Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (E));
   Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
when E : Db_Error =>
   Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (E));
   Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
when E : Invalid_Command =>
   Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (E));
   Process_Help (GNATCOLL.Arg_Lists.Empty_Command_Line);
   Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
when GNAT.Command_Line.Invalid_Switch =>
   GNAT.Command_Line.Display_Help (Cmdline);
   Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
when E : others =>
   Ada.Text_IO.Put_Line ("Unexpected exception");
   Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
   Ada.Text_IO.Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (Ada.Exceptions.Traceback.Tracebacks (E)));
   Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
end Gpr_Query;
