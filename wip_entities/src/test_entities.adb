------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2011-2012, AdaCore                     --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

with Ada.Text_IO;           use Ada.Text_IO;
with GNAT.Command_Line;     use GNAT.Command_Line;
with GNAT.Strings;          use GNAT.Strings;
with GNATCOLL.ALI;          use GNATCOLL.ALI;
with GNATCOLL.SQL.Exec;     use GNATCOLL.SQL.Exec;
with GNATCOLL.SQL.Inspect;  use GNATCOLL.SQL.Inspect;
with GNATCOLL.SQL.Postgres;
with GNATCOLL.SQL.Sqlite;
with GNATCOLL.Traces;       use GNATCOLL.Traces;
with GNATCOLL.Projects;     use GNATCOLL.Projects;
with GNATCOLL.VFS;          use GNATCOLL.VFS;

procedure Test_Entities is
   Use_Postgres : aliased Boolean := False;
   --  Whether to use sqlite or postgreSQL

   Do_Not_Perform_Queries : aliased Boolean := False;
   --  Whether to perform the queries in the database

   Nightly_DB  : aliased String_Access;
   DB_Name     : aliased String_Access;
   Tmp_DB_Name : aliased String_Access;

   Omit_Runtime_Files : aliased Boolean := False;
   --  Whether to parse files from predefined directories

   GPR_File     : Virtual_File;

   Env     : Project_Environment_Access;
   Tree    : Project_Tree;
   GNAT_Version : String_Access;
   Cmdline_Config : Command_Line_Configuration;
   Xref    : Xref_Database;

begin
   GNATCOLL.Traces.Parse_Config_File;

   Set_Usage
     (Cmdline_Config,
      Help => "In general, there are up to three databases involved:"
      & ASCII.LF
      & " 'nightly' is a database that could be generated during nightly"
      & " builds. If unspecified, it defaults to the value of 'db'."
      & ASCII.LF
      & " 'db' is the user's database on disk. If it doesn't exist yet, it"
      & " will be either initialized by copying 'nightly', or created anew."
      & " On exit, it will have been updated with the new contents."
      & ASCII.LF
      & " 'tmpdb' is the database that is modified during the parsing. In"
      & " general, it should be the same as 'db', and the tool might decide to"
      & " create a temporary in-memory database if the number of files to"
      & " update is significant. But you can force it to :memory: to force an"
      & " update in memory.");
   Define_Switch
     (Cmdline_Config, Do_Not_Perform_Queries'Access,
      Long_Switch => "--nodb",
      Help => "Disable all SQL commands (timing measurement only)");
   Define_Switch
     (Cmdline_Config, Use_Postgres'Access,
      Long_Switch => "--postgres",
      Help => "Use postgreSQL as the backend, instead of sqlite");
   Define_Switch
     (Cmdline_Config, Nightly_DB'Access,
      Long_Switch => "--nightly:",
      Help =>
        "Name of the pregenerated database (for instance from nightly builds");
   Define_Switch
     (Cmdline_Config, DB_Name'Access,
      Long_Switch => "--db:",
      Help => "Name of the database on disk");
   Define_Switch
     (Cmdline_Config, Tmp_DB_Name'Access,
      Long_Switch => "--tmpdb:",
      Help =>
        "Name of the temporary database (use :memory: to copy to memory)");
   Define_Switch
     (Cmdline_Config, Omit_Runtime_Files'Access,
      Long_Switch => "--noruntime",
      Help => "Do not parse runtime files");

   Getopt (Cmdline_Config);

   if DB_Name.all = "" then
      Free (DB_Name);
      DB_Name := new String'("entities.db");
   end if;

   if Nightly_DB.all = "" then
      Free (Nightly_DB);
      Nightly_DB := new String'(DB_Name.all);
   end if;

   if Tmp_DB_Name.all = "" then
      Free (Tmp_DB_Name);
      Tmp_DB_Name := new String'(DB_Name.all);
   end if;

   GPR_File := Create (+GNAT.Command_Line.Get_Argument);

   GNATCOLL.SQL.Exec.Perform_Queries := not Do_Not_Perform_Queries;

   --  Prepare database

   if Use_Postgres then
      Xref.Setup_DB
        (GNATCOLL.SQL.Postgres.Setup (Database => DB_Name.all));
   else
      Xref.Setup_DB
        (GNATCOLL.SQL.Sqlite.Setup (Database => Tmp_DB_Name.all));
   end if;

   --  Load project

   Initialize (Env);
   Env.Set_Path_From_Gnatls
     (Gnatls       => "gnatls",
      GNAT_Version => GNAT_Version,
      Errors       => Put_Line'Access);
   Env.Register_Default_Language_Extension
     (Language_Name       => "C",
      Default_Spec_Suffix => ".h",
      Default_Body_Suffix => ".c");
   Free (GNAT_Version);
   Tree.Load
     (Root_Project_Path => GPR_File,
      Env               => Env,
      Errors            => Put_Line'Access);

   --  Parse LI files (loading and dumping to DB_Name)

   Xref.Parse_All_LI_Files
     (Tree         => Tree,
      Project      => Tree.Root_Project,
      Parse_Runtime_Files => not Omit_Runtime_Files,
      From_DB_Name => Nightly_DB.all,
      To_DB_Name   => DB_Name.all);

   --  Free memory

   Tree.Unload;
   Free (Env);
   Xref.Free;
   GNATCOLL.Projects.Finalize;

exception
   when GNAT.Command_Line.Exit_From_Command_Line =>
      null;
end Test_Entities;
