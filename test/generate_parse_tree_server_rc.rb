#!/usr/bin/env ruby
=begin copyright
    redparse - a ruby parser written in ruby
    Copyright (C) 2008,2009, 2012, 2016  Caleb Clausen

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU Lesser General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
=end
require 'rubygems'
require 'parse_tree'

    ##
    # Finds the user's home directory. 
    #--
    # Some comments from the ruby-talk list regarding finding the home
    # directory:
    #
    #   I have HOME, USERPROFILE and HOMEDRIVE + HOMEPATH. Ruby seems
    #   to be depending on HOME in those code samples. I propose that
    #   it should fallback to USERPROFILE and HOMEDRIVE + HOMEPATH (at
    #   least on Win32).
    #(originally stolen from rubygems)
    def find_home
      ['HOME', 'USERPROFILE'].each do |homekey|
        return ENV[homekey] if ENV[homekey]
      end

      if ENV['HOMEDRIVE'] && ENV['HOMEPATH'] then
        return "#{ENV['HOMEDRIVE']}#{ENV['HOMEPATH']}"
      end

      begin
        File.expand_path("~")
      rescue
        if File::ALT_SEPARATOR then
            "C:/"
        else
            "/"
        end
      end
    end

PARSETREE_AND_DEPENDANCIES=
  %r{/(?:ZenTest|RubyInline|sexp_processor|ParseTree)-}

pt_needs_dirs=$:.grep PARSETREE_AND_DEPENDANCIES

open find_home+"/.redparse/parse_tree_server.rc","w" do |rcfile|
  rcfile.write pt_needs_dirs.join("\n")+"\n"  
end
