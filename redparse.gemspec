# -*- encoding: utf-8 -*-
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

dir=File.dirname(__FILE__)
require "#{dir}/lib/redparse/version"
RedParse::Description=open("#{dir}/README.txt"){|f| f.read[/^==+ ?description[^\n]*?\n *\n?(.*?\n *\n.*?)\n *\n/im,1] }
RedParse::Latest_changes="###"+open("#{dir}/History.txt"){|f| f.read[/\A===(.*?)(?====)/m,1] }

Gem::Specification.new do |s|
  s.name = "redparse"
  s.version = RedParse::VERSION
  s.date = Time.now.strftime("%Y-%m-%d")
  s.authors = ["Caleb Clausen"]
  s.email = %q{caleb (at) inforadical (dot) net}
  s.summary = "RedParse is a ruby parser written in pure ruby."
  s.description = RedParse::Description
  s.homepage = %{http://github.com/coatl/redparse}
  s.rubyforge_project = %q{redparse}

  s.files = `git ls-files`.split
  s.test_files = %w[test/test_all.rb]
  s.require_paths = ["lib"]
  s.bindir = "bin"
  s.extra_rdoc_files = ["README.txt", "COPYING.LGPL"]
  s.has_rdoc = true
  s.rdoc_options = %w[--main README.txt]
  s.license = 'LGPL-2.1'

  s.rubygems_version = %q{1.3.0}
  s.required_rubygems_version = Gem::Requirement.new(">= 0") if s.respond_to? :required_rubygems_version=

  if s.respond_to? :specification_version then
    current_version = Gem::Specification::CURRENT_SPECIFICATION_VERSION
    s.specification_version = 2

    if Gem::Version.new(Gem::RubyGemsVersion) >= Gem::Version.new('1.2.0') then
      s.add_runtime_dependency("rubylexer", '0.8.0')
      s.add_runtime_dependency("reg", ['>= 0.4.8'])
    else
      s.add_dependency("rubylexer", '0.8.0')
      s.add_dependency("reg", ['>= 0.4.8'])
    end
  else
    s.add_dependency("rubylexer", '0.8.0')
    s.add_dependency("reg", ['>= 0.4.8'])
  end
end
