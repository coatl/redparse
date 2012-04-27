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

