# Copyright (C) 2008  Caleb Clausen
# Distributed under the terms of Ruby's license.
require 'rubygems'
require 'hoe'
require './lib/redparse/version.rb'

if $*==["test"]
  #hack to get 'rake test' to stay in one process
  #which keeps netbeans happy
  $:<<"lib"
  require "test/test_redparse.rb"
  Test::Unit::AutoRunner.run
  exit
end
 
   readme=open("README.txt")
   readme.readline("\n== DESCRIPTION:")
   readme.readline("\n\n")
   desc=readme.readline("\n\n")
 
   hoe=Hoe.new("redparse", RedParse::VERSION) do |_|
     _.author = "Caleb Clausen"
     _.email = "redparse-owner @at@ inforadical .dot. net"
     _.url = ["http://github.com/coatl/redparse/", "http://rubyforge.org/projects/redparse/"]
     _.extra_deps << ['rubylexer', '>= 0.7.4']
     _.extra_deps << ['reg', '>= 0.4.7']
#     _.test_globs=["test/*"]
     _.description=desc
     _.summary=desc[/\A[^.]+\./]
     _.spec_extras={:bindir=>'bin/'}
#     _.rdoc_pattern=/\A(README\.txt|lib\/.*\.rb)\Z/
#     _.remote_rdoc_dir="/"
   end


