name=RedParse
lname=redparse
gemname=redparse

lib/redparse/ReduceWithsFor_RedParse_1_8.rb: lib/redparse.rb
	redparse --cache=no -c

lib/redparse/ReduceWithsFor_RedParse_1_9.rb: lib/redparse.rb
	redparse --cache=no -c '1.9'

parser: lib/redparse/ReduceWithsFor_RedParse_1_8.rb lib/redparse/ReduceWithsFor_RedParse_1_9.rb
.PHONY: parser

#everything after this line is generic

version=$(shell ruby -r ./lib/$(lname)/version.rb -e "puts $(name)::VERSION")
filelist=$(shell git ls-files)

.PHONY: all test docs gem tar pkg email
all: test

test:
	ruby -Ilib test/test_all.rb

docs:
	rdoc lib/*

pkg: gem tar

gem:
	gem build $(lname).gemspec

tar:
	tar cf - $(filelist) | ( mkdir $(gemname)-$(version); cd $(gemname)-$(version); tar xf - )
	tar czf $(gemname)-$(version).tar.gz $(gemname)-$(version)
	rm -rf $(gemname)-$(version)

email: README.txt History.txt
	ruby -e ' \
  require "rubygems"; \
  load "./$(lname).gemspec"; \
  spec= Gem::Specification.list.find{|x| x.name=="$(gemname)"}; \
  puts "\
Subject: [ANN] $(name) #{spec.version} Released \
\n\n$(name) version #{spec.version} has been released! \n\n\
#{Array(spec.homepage).map{|url| " * #{url}\n" }} \
 \n\
#{$(name)::Description} \
\n\nChanges:\n\n \
#{$(name)::Latest_changes} \
"\
'
