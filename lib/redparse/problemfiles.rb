=begin           
    redparse - a ruby parser written in ruby
    Copyright (C) 2008, 2012  Caleb Clausen

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
=end

class CachedResults
  def initialize(name="problemfiles")
    @bad2offset={}
    @good2offset={}

    #read list from disk
    @file=File.open(name,File.exist?(name) ? "r+" : "w+")
    until @file.eof?
      at=@file.pos
      line=@file.readline
      if line[0]==?# 
        @good2offset[line[1...-1]]=at
      else
        @bad2offset[line[1...-1]]=at
      end
    end
  end

  def bad! pf
    @bad2offset[pf] and return self

    if pos=@good2offset.delete(pf)
      @bad2offset[pf]=pos
      @file.pos=pos
      @file.putc ' '
    else
      @file.seek(0,IO::SEEK_END)
      @bad2offset[pf]=@file.pos
      @file.puts " "+pf
    end
    @file.flush

    return self
  end

  def good! pf
    @good2offset[pf] and return self

    if offset=@bad2offset.delete(pf)
      @good2offset[pf]=offset
      @file.pos=offset
      @file.putc '#'
    else
      @file.seek(0,IO::SEEK_END)
      @good2offset[pf]=@file.pos
      @file.puts "#"+pf
    end
    @file.flush
    return pf
  end

  def badlist
    @bad2offset.keys 
  end

  def goodlist
    @good2offset.keys
  end
end

class ProblemFiles < CachedResults #old interface
  alias push bad!
  alias delete good!
  alias list badlist
end
