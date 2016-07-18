=begin
    redparse - a ruby parser written in ruby
    Copyright (C) 2012, 2016  Caleb Clausen

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
class RedParse
  def self.remove_silly_begins(pt)
    pt.each_with_index{|x,i|
      if Array===x
        remove_silly_begins(x)
        if x.size==2 and x.first==:begin
          pt[i]=x=x.last
        end
      end
    }
  end
end
