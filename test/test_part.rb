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

$:.push File.expand_path(File.dirname(__FILE__))
require 'test_redparse'

HASH=(ENV['PART']||0).to_i 
ObjectSpace.each_object(Class){|k|
  next unless k<=Test::Unit::TestCase
  kn=k.name
  k.instance_methods.each{|m| mm=m.to_s
    if /^test_/===mm and (mm+kn).hash.&(0xFF) != HASH
      k.instance_eval{undef_method(m.to_sym)} 
    end
  }
}

