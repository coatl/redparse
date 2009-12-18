=begin           
    redparse - a ruby parser written in ruby
    Copyright (C) 2008  Caleb Clausen

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

  #this is actually fixing a bug in reg 0.4.7, grrrrr
  unless Array===(Class*5).subregs
    module ::Reg
      class Repeat
        undef subregs if allocate.respond_to? :subregs
        def subregs
          [@reg]
        end
      end
    end
  end


  unless defined? ::Reg::Transform #and ::Reg::Transform.ancestors.include? ::Reg::HasBmatch
  #hack, until support for this syntax makes it into the release of reg
    module ::Reg
      class Transform;
        def initialize(left,right)
          @left,@right=left,right
        end
        attr_reader :left,:right
      end
      module Reg
        def >>(rep)
          Transform.new(self,rep)
        end
      end
    end    
  end

  unless Object.allocate.extend(::Reg::Reg).respond_to? :lb
    module ::Reg
      module Reg
        def lb
          LookBack.new(self)
        end
      end
      class LookBack
        def initialize(reg)
          @reg=reg
        end

        def regs(i)
          @reg
        end

        def subregs
          [@reg]
        end

        def itemrange
          0..0
        end
      end
    end 
  end
 
  unless Object.allocate.extend(::Reg::Reg).respond_to? :la
    module ::Reg
      module Reg
        def la
          LookAhead.new(self)
        end
      end
      class LookAhead
        def initialize(reg)
          @reg=reg
        end

        def regs(i)
          @reg
        end

        def subregs
          [@reg]
        end

        def itemrange
          0..0
        end
      end
    end 
  end

  unless Object.allocate.extend(::Reg::Reg).respond_to? :watch
    module ::Reg
      module Reg
        def watch
          result=dup
          class<<result
            def ===(other)
              result=super
              result and p other
              return result            
            end
          end
          return result
        end
      end
    end
  end

