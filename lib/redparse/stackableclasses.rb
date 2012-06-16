=begin
    redparse - a ruby parser written in ruby
    Copyright (C) 2012  Caleb Clausen

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
  module StackableClasses
    #just the left side (the stack/lookahead matchers)
    def LEFT
      @rules.map{|r| r.left.subregs }.flatten
    end

    #remove lookahead and lookback decoration
    def LEFT_NO_LOOKING
    l=LEFT()
    l.map!{|m|
      case m #
      when Reg::LookAhead,Reg::LookBack; m.subregs[0]
      when Proc; []
      else m #
      end #
    }
    l
    end

    #all classes mentioned in rules, on left and right sides
    def STACKABLE_CLASSES #
      return @sc_result unless  @sc_result.nil?
      @sc_result=false
      l=LEFT_NO_LOOKING()
      l=l.map{|lm| sc_juice lm}.flatten.compact
      r=  @rules.map{|rr| rr.right }.grep(Class) #classes in productions
      result=l+r
      @sc_result=result.grep(Class).uniq
      fail if @sc_result.empty?
      return @sc_result
    end

    def juice(m)
      case m  #
      when Class;
        return [m] unless @subclasses_of
        result=[m]  # and subclasses too
        i=0
        while item=result[i]
          #p item
          result.concat @subclasses_of[item]
          i += 1
        end
        result
      when String,Regexp; juice(RedParse.KW(m))
      when Reg::And; m.subregs.map{|x| juice(x).flatten.compact}.inject{|sum,rr| sum&rr}
      when Reg::Or; m.subregs.map( &method(:juice) )
      when Reg::Not;
        m=m.subregs[0]
        if Class===m or (Reg::Or===m  and
             m.subregs.inject{|sum,x| sum && (Class===x) })
          j=juice(m)
          STACKABLE_CLASSES()-j.flatten.compact rescue j
        else
          STACKABLE_CLASSES()
        end
      else STACKABLE_CLASSES()
      end
    end

    def sc_juice(m)
      case m #
      when Class; [m]
      when String,Regexp; juice(RedParse.KW(m))
#      when String,Regexp; [KeywordToken]
      when Reg::And; m.subregs.map{|x| sc_juice(x)}.compact.map{|x| x.flatten.compact}.inject{|sum,rr| sum&rr }
      when Reg::Or; m.subregs.map( &method(:sc_juice) )
      when Reg::Not; sc_juice(m.subregs[0])
      when Reg::LookAhead, Reg::LookBack; sc_juice(m.subregs[0])
      else []
      end
    end

    def LOOKAHEAD_CLASSES rule
      last=rule.left.subregs.last
      return STACKABLE_CLASSES() unless Reg::LookAhead===last
      la= last.subregs[0]
      return juice(la).flatten.compact
    end

    def TOS_CLASSES rule
      i=-1
      mats=rule.left.subregs
      m=mats[i]
      m=mats[i-=1] if Reg::LookAhead===m || Proc===m
      result=[]
      while Reg::Repeat===m and m.times.min.zero?
        result<<juice(m.subregs[0])
        m=mats[i-=1]
      end
      return (result+juice(m)).flatten.compact
    end
  end
end
