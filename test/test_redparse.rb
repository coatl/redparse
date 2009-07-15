=begin           
    redparse - a ruby parser written in ruby
    Copyright (C) 2008  Caleb Clausen

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

require 'test/unit'
require 'rubygems'
require 'parse_tree'
require 'tempfile'

require "redparse"

require "rubylexer/test/testcases"

$VERBOSE=1

class Test::Unit::TestCase
  def known_error
    from=caller.first
    from=from[/ in `.*'\Z/] || from[/\A[^:]*:[^:]*/]
    yield
  rescue Test::Unit::AssertionFailedError=>e
    warn "an expected error occurred in #{from}: #{e.message}"
    if defined? @@known_errors
      @@known_errors+=1
    else
      @@known_errors=1
      at_exit {warn "!!!UNFIXED KNOWN ERRORS!!!: #@@known_errors"}
    end
  rescue Exception=>e
    raise
  else
    warn "expected error in #{from}, but was fixed(?!)"
    if defined? @@known_errors_fixed
      @@known_errors_fixed+=1
    else
      @@known_errors_fixed=1
      at_exit {warn "unexpectedly fixed known errors: #@@known_errors_fixed"}
    end
  end

  def known_failure
    from=caller.first
    from=from[/ in `.*'\Z/] || from[/\A[^:]*:[^:]*/]
    yield
  rescue Exception=>e
    warn "an expected failure occurred in #{from}: #{e}"
    if defined? @@known_failures
      @@known_failures+=1
    else
      @@known_failures=1
      at_exit {warn "!!!UNFIXED KNOWN FAILURES!!!: #@@known_failures"}
    end
  else
    warn "expected failure in #{from}, but was fixed(?!)"
    if defined? @@known_failures_fixed
      @@known_failures_fixed+=1
    else
      @@known_failures_fixed=1
      at_exit {warn "unexpectedly fixed known failures: #@@known_failures_fixed"}
    end
  end

  def slow
    if ENV['SLOW']
      yield
    else
      if defined? @@slow_spots
        @@slow_spots+=1
      else
        @@slow_spots=1
        at_exit {warn "slow test code skipped in #@@slow_spots places. (set SLOW to enable)"}
      end
    end
  end
end

#print output immediately on failing test (at end too....)
require 'test/unit/ui/console/testrunner'
class Test::Unit::UI::Console::TestRunner
  alias add_fault__no_immed_output add_fault
  def add_fault fault
    @fault_count||=0
    output("\n%3d) %s" % [@fault_count+=1, fault.long_display])
    add_fault__no_immed_output fault
  end
end

=begin nice idea, don't work yet
class Test::Unit::TestResult
  @@FAILING_CASES=[]

  begin
    huh #but this is running at the wrong time... 
    huh #need to run before tests are run but after defined
    eval Marshal.load(huh).map{|name,case|
      name=huh quote name
      "
       class #{case}      
         alias_method :'first_off_#{name}', :'#{name}'
         undef_method :'#{name}'
       end
      "
    }.to_s
    
    in_a_loop{
      huh #ensure methods named first_off_test* get run first
      some_test_case._method=name
      some_test_case.run(huh result){huh}
    }

  rescue Exception
    #ignore it
  end

  alias add_error__no_error_memory add_error
  def add_error x
    name=x.test_name
    i=name.rindex('(')
    @@FAILING_CASES.push [name[0...i],name[i..-1]]

    add_error__no_error_memory x
  end

  alias add_failure__no_error_memory add_failure
  def add_failure x
    name=x.test_name
    i=name.rindex('(')
    @@FAILING_CASES.push [name[0...i],name[i..-1]]

    add_failure__no_error_memory x
  end

  at_exit {huh @@FAILING_CASES}
end
=end

class ParseTree
  def put o
    o=Marshal.dump o
    @@out.write [o.size].pack("N")+o
  end
  def get
    Marshal.load @@in.read(@@in.read(4).unpack("N")[0])
  end
  def fork_server?
    return if defined? @@out 
    si,co=IO::pipe
    ci,so=IO::pipe
    fork{
      begin
      co.close; ci.close
      @@out=so; @@in=si
      warnstash=Tempfile.new "warnstash"
      STDERR.reopen warnstash
      while 1
        str=get
        exit! if str==:exit!

        pos=STDERR.pos

        tree=
        begin
          parse_tree_for_string(str) #tree
        rescue Exception=>e; 
          tree=e
        end
        put tree

        open(STDERR.path){|f| 
          f.pos=pos
          put f.read.split #warnings
        }
      end
      rescue Exception=>e; p e; raise
      ensure exit!
      end
    }
    si.close; so.close
    @@out=co; @@in=ci
    at_exit { put :exit! }
  end

  #returns +[parse_tree|Exception, +[String.*]]
  def parse_tree_and_warnings str
    fork_server?
    put str
    tree=get
    warnings=get
    raise tree if Exception===tree
    return tree,warnings
  end

  #this way is bad enough, but there's a fd leak this way,
  #so I have to use the even more complicated version above
  def parse_tree_and_warnings_leaks_stderr(str)
    oldSTDERR=STDERR.dup
    warnstash=Tempfile.new "warnstash"
    warnings=[]

    STDERR.reopen warnstash
      tree=parse_tree_for_string(str)

    return tree,warnings
  ensure
    STDERR.reopen oldSTDERR

    warnstash.rewind
    warnings.replace warnstash.read.split
    warnstash.close
  end
end

class RedParseTest<Test::Unit::TestCase
  ERROR_EXAMPLES=[
   '%W"is #{x}#{"Slim #{2?"W":"S"} "}."',
   '%W"is #{x}#{"Slim #{2?"W":"S"}"}#{xx}."',
   '%W"is #{x}#{"Slim #{2?W: S} "}."',
   '%W"is #{x}#{%Q\'Slim #{2?W: S} \'}."',
   '%W"is #{x}#{%W"Slim #{2?"W":"S"}"}#{xx}."',
   '%W"is #{x}#{%W\'Slim #{2?W: S} \'}."',
   '%W"is #{x}#{%q\'Slim #{2?W: S} \'}."',
   '%W"is #{x}#{%r\'Slim #{2?W: S} \'}."',
   '%W"is #{x}#{%w\'Slim #{2?W: S} \'}."',
   '%W"is #{x}#{%x\'Slim #{2?W: S} \'}."',
   '%W"is #{x}#{/Slim #{2?W: S} /}."',
   '%W"is #{x}#{`Slim #{2?W: S} `}."',
   '%W"is_#{"Slim_#{2?"W":"S"}"}#{xx}."',
   '%W"is_#{x}#{"Slim_#{2?"W":"S"}"}#{xx}."',
   '%W"is_#{x}#{"Slim_#{2?"W":"S"}_"}."',
  ]
  FAILURE_EXAMPLES=[
  ]
  RUBYBUG_EXAMPLES=[
'    case
    when 0
      guecoding
    else case
      when eucjp_match_length
        guing
      end
    end',

    'case; when false; else case; when nil; else 5; end; end',
    'def foo(a=b=c={}) end',
    "$11111111111111111111111111111111111111111111111111111111111111111111",
    "c do p (110).m end",
    "case F;when G; else;case; when j; end;end",
    "p = p m %(1)",
    "p = p m %(1) ,&t",
    "p = p m %(1) ,*t",
    "p = p m %(1) ,t",
    "p = p m %(1) do end",
    "p=556;p (e) /a",
    "z{|| p (1).m}",
    'def sum(options = {:weights => weights = Hash.new(1)}); options.empty? or options.keys.size > 1; end',
    'def sum(options = {:weights => weights = Hash}); 1 end',
  ]

  ONELINERS=[
    'p (a,b=c,d); a +h'...'',
    'x{return (a,b=c,d)}'...'',
    'x{r (a,b=c,d)}'...'',
    'x{return (a,b=c,d)|1}'...'',
    'x{r (a,b=c,d)|1}'...'',
    'case; when false; else case; when nil; else 5; end; end'...'',
    'case;else case; else; end;end'...'',
    'case; else; end'...'',
    'c while d and 2.a?(b)..8'...'',
    'c while d and 888888888888888888888888888888888888888888888888888888..2.a?(b)'...'',
    'c while d and 8.8..2.a?(b)'...'',
    'c while d and 8..2.a?(b)'...'',
    'c while d and :a8..2.a?(b)'...'',
    'c while d and :a8..:b8'...'',
    'c while d and a8..:b8'...'',
    'c while d and 8..:b8'...'',
    'c while d and /8/..2.a?(b)'...'',
    'c while d and /8/../2.a?(b)/'...'',
    'c while d and 8../2.a?(b)/'...'',
    'c while d and a8../2.a?(b)/'...'',
    'z = valueo_s rescue "?"'...'',
    '"#{publi}#{}>"'...'',
    'return (@images = @old_imgs)'...'',
    ' /\A#{__FILE__}tcase/n =~ i '...'',
    'doc_status, err_args = Documeh_status{fcgi_state = 3; docespond do doc_response =fcgi_state =  1; end }'...'',
    'print "coled: " + $! +" wiin #{@ray}\n";'...'',
    'class A;def b;class <<self;@@p = false end;end;end'...'',
    'def d; return (block_given? ? begin; yield f; ensure; f.close; end : f); end'...'',
    'def sum(options = {:weights => weights = Hash.new(1)}); options.empty? or options.keys.size > 1; end'...'',
    'def d;e install_dir;end'...'',
    '(m).kk,(m+n).kk=3'...'',
    '((m).kk,(m+n).kk)=3'...'',
    '((m).kk,(m+n).kk),t=3'...'',
    's,((m).kk,(m+n).kk)=3'...'',
    's,((m).kk,(m+n).kk),t=3'...'',
    'proc{|(m).kk,(m+n).kk| }'...'',
    "p p:'b'"...'', #changes semantics in 1.9
    "p:'b'"...'', #but this doesn't
    't ?6e0 : 5'...'',
    "super[]"...'',
    'p end=5'...'', #illegal
    'p or=5'...'', #illegal
    'p else=5'...'', #illegal
    'p if=5'...'', #illegal
    "x{\x0afor i in (begin\n[44,55,66,77,88] end) do p i**Math.sqrt(i) end\n}\n"...'',
    'e { |c|; print "%02X" % c }'...'',
    %[p <<-heredoc "x y z" and 5\n       a b c\n     heredoc]...'',
    "File.open() {|f|  ;  }"...'',
    %[     p <<-heredoc "x y z" and 5\n       a b c\n     heredoc]...'',
    'a rescue b until 1'...'',
    'a rescue b while 1'...'',
    '%W[r\\c     3]'...'',

    "%w[a\\C- b]"...'',
    "%w[a\\M- b]"...'',
    "%w[a\\c b]"...'',
    "%W[a\\c b]"...'',
    "%w[a\\\\c b]"...'',
    "%W[a\\\\c b]"...'',
    "%w[a\\c  b]"...'',
    "%W[a\\c  b]"...'',
    "%w[a\\\\c  b]"...'',
    "%W[a\\\\c  b]"...'',
    "module A; b; rescue C=>d; e; else g; ensure f; end"...'',
    "class<<A; b; rescue C=>d; e; else g; ensure f; end"...'',
    "class A; b; rescue C=>d; e; else g; ensure f; end"...'',
    'def i;"..#{@@c = 1}";end'...'',
    "def wait; begin; ync; mup; end ;end"...'',
    "def e; begin; y; g else t; m; end ;end"...'',
    '"#{}"""'...'',
    %[%W(white\\  \\  \\ \\  \\ space).should == ["white ", " ", "  ", " space"]]...'',
    "%w[- \\\\ ]"...'',
    "%w[- \\\\ e]"...'',
    "begin begin; ync; p1; end;rr end"...'',
    "begin;mode;rescue;o_chmod rescue nil;end"...'',
    "%w![ ] { } ( ) | - * . \\\\ ? + ^ $ #!"...'',

    'def foo(a = 1)    end; def foo(a=b=c={})  end; def bar(a=b=c=1,d=2)  end'...'',
    '() until 1'...'',
    '(a) until 1'...'',
    '(a) while l 1'...'',
    "module A; B='' end"...'',
    "{'one' => {}}"...'',
    "a[3, 2] = 'a', 'b'"...'',
    'def st;  begin ire end end'...'',
    'x{k=1; return-k}'...'',
    'x{k=1; return -k}'...'',
    'x{k=1; return- k}'...'',
    'x{k=1; return - k}'...'',
    'x{k=1; return + k}'...'',
    'x{k=1; return * k}'...'',
    'x{k=1; break - k}'...'',
    'x{k=1; next - k}'...'',
    "p\r44"...'',
    'yield []'...'',
    'yield [1]'...'',
    'f{a rescue b}'...'',
    '%w[- \\\\\\\\ e]'...'',
    '%w[- \\\\\\\\ ]'...'',
    'x{a=[]; a [42] = 24}'...'',   #not legal unless a is a local var
    '%w![ ] { } ( ) | - * . \\\\\\\\ ? + ^ $ #!'...'',
    '%W(white\  \  \ \  \ space).should == ["white ", " ", "  ", " space"]'...'',
    'module A; b; rescue C=>d; e; else g; ensure f; end'...'',
    'class A; b; rescue C=>d; e; else g; ensure f; end'...'',
    'class<<A; b; rescue C=>d; e; else g; ensure f; end'...'',
    'File.open() {|f|  ;  }'...'',
    'e { |c|; print "%02X" % c }'...'',
    ' /\A#{__FILE__}/'...'',
    ' /\A#{"F"}/'...'',
    '"\\\\\\\\\\""'...'',
    '"\\\\\\""'...'',
    '"\\\\\\\\\\\"'...'',
    '"\\\\\\\"'...'',

    'alias :"foo#{bar}" :"baz#{quux}"'...'',
    'undef :"foo#{bar}", :"baz#{quux}"'...'',
    'A::B::C'...'',
    's=3; s::T'...'',
    'return ENV[]'...'',
    ' [TypeValue, ClassValue = Clalue] '...'',
    '{44 => TypeValue, ClassValue = Clalue => 55}'...'',
    '{44 => TypeValue = Type, ClassValue = Clalue => 55}'...'',
    'module::A::B::Special::Extend C end'...'',
    'module ::A::B::Special::Extend C end'...'',
    'module A::B::Special::Extend C end'...'',
    'module A::B ::Special::Extend C end'...'',
    'module A::B :: Special::Extend C end'...'',
    'module A::B:: Special::Extend C end'...'',
    'class BUNE end'...'',
    'class BUNE; extend end'...'',
    'module BUNE end'...'',
    'module BUNE extend end'...'',
    'module BUNE extend OptiFgset end'...'',
    '0.5'...'',
    'class BUG_ONE extend OptiFlag::Flagset; end'...'', #illegal
    'module ::BUG_ONE extend OptiFlag::Flagset; end'...'',
    'module ::BUG::ONE extend OptiFlag::Flagset; end'...'',
    'module BUG::ONE extend OptiFlag::Flagset; end'...'',
    'module BUG_ONE extend OptiFlag::Flagset; end'...'',
    'module BUG_ONE; extend OptiFlag::Flagset; end'...'',
    ' ClassHash[TypeValue = Type, ClassValue = Clalue] =1'...'',
    ' ClassHash[[TypeValue = Type, ClassValue = Clalue]] =1'...'',
    ' ClassHash[[[TypeValue = Type, ClassValue = Clalue]]] =1'...'',
    ' ClassHash[TypeValue = Type, ClassValue = Clalue] '...'',
    ' ClassHash[[TypeValue = Type, ClassValue = Clalue]] '...'',
    ' ClassHash[[[TypeValue = Type, ClassValue = Clalue]]] '...'',
    ' [TypeValue = Type, ClassValue = Clalue] '...'',
    ' [[TypeValue = Type, ClassValue = Clalue]] '...'',
    ' [[[TypeValue = Type, ClassValue = Clalue]]] '...'',
    'begin r;rescue *re;r;end'...'',
    'if v; else return([vUew]).e  end'...'',
    'unless v; else return ([vUew]).e  end'...'',
    'if v; else return ([vUew]).e  end'...'',
    'begin v; else return ([vUew]).e  end'...'',
    'case v; when 4; 5; else return ([vUew]).e  end'...'',
    'def v; t; else return ([vUew]).e  end'...'',
    'def i;"..#{@@c = 1}";end'...'',
    '"""Universal feed parser in Ruby""";$K=fgdf'...'',
    "case F;when G; else;case; when j; end;end"...'',
    ' m.cn= 1, V'...'',
    '__LINE__'...'',
    '"#{}"""'...'',
    'def line(host = host,guest=host); end'...'',
    'def e; begin; y; g else t; m; end ;end'...'',
    '[a,*b=c]'...'',
    'p a,*b=c'...'',
    'p(a,*b=c)'...'',
    '[*b=c]'...'',
    'p *b=c'...'',
    'p(*b=c)'...'',
    'p = p m %(1)'...'',  #the parsing of this one makes no sense to me!
    'p = p m %(1) do end'...'',  #the parsing of this one makes no sense to me!
    'p = p m %(1) ,t'...'',  #the parsing of this one makes no sense to me!
    'p = p m %(1) ,*t'...'',  #the parsing of this one makes no sense to me!
    'p = p m %(1) ,&t'...'',  #the parsing of this one makes no sense to me!
    'p = k.p m %(1)'...'',  #the parsing of this one makes no sense to me!
    'p = k.p m %(1) do end'...'',  #the parsing of this one makes no sense to me!
    'p = k::p m %(1)'...'',  #the parsing of this one makes no sense to me!
    'p = k::p m %(1) do end'...'',  #the parsing of this one makes no sense to me!
    'p=556;p (e) /a'...'',
    '(a;b;c);d;e;f;(g;h;i)'...'',
    'def __DATA__.f; end'...'',
    'def continue.f; end'...'',
    'a0 = k ? a rescue b : d'...'',
    'a0 = a rescue k ? b : d'...'',
    'k ? a0 = a rescue b : d'...'',
    '{a0 = a rescue b => c}'...'',
    'p(a0 = a rescue b,c)'...'',
    'p a0 = a rescue b,c'...'',
    '[a0 = a rescue b,c]'...'',
    'a0 = a rescue b and c'...'',
    'a0 = a rescue b or c'...'',
    'a0 = a rescue b if c'...'',
    'a0 = a rescue b && c'...'',
    'begin begin; ync; p1; end;rr end'...'',
    'a= y 1,2 rescue c'...'',
    '"f";p (1..10).method(:each)'...'',
    '"\C-\p "'...'',
    'p (&a)'...'',
    'p (*a)'...'',
    'return % 1 '...'',
    'return / 1 /'...'',
    'a.return % 1 '...'',
    'a.return / 1 /'...'',
    'break % 1 '...'',
    'break / 1 /'...'',
    'a.break % 1 '...'',
    'a.break / 1 /'...'',
    'break % 1 '...'',
    'break / 1 /'...'',
    'a.break % 1 '...'',
    'a.break / 1 /'...'',

    'return[1]'...'',
    'next[1]'...'',
    'break[1]'...'',
    'continue[1]'...'',
    'a.return[1]'...'',
    'a.next[1]'...'',
    'a.break[1]'...'',
    'a.continue[1]'...'',

    'yield *a=r'...'',
    'begin; q; rescue a=r,b; end'...'',
    'case g; when a=r,b; end'...'',
    'begin; g; rescue *a=r; end'...'',
    'case g; when *a=r; end'...'',
    'm.yield *a=r'...'',
    'm.rescue a=r,b'...'',
    'm.when a=r,b'...'',
    'm.rescue *a=r'...'',
    'm.when *a=r'...'',

    '(*a=c)'...'',
    '((*a=c))'...'',
    '(((*a=c)))'...'',
    '((((*a=c))))'...'',

    'p *a=c'...'',
    'p(*a=c)'...'',
    'p((*a=c))'...'',
    'p(((*a=c)))'...'',
    'p((((*a=c))))'...'',
    'p (*a=c)'...'',
    'p (1),*a=c'...'',
    '{p (1) => b}'...'',
    '"1#{p (1,2).m}2"'...'',
    '"1#{p ().m}2"'...'',
    '"1#{p (1).m}2"'...'',
    '"1#{tt;p (1).m}2"'...'',
    'a0 = az=d,y'...'',
    'begin; rescue A=>b.c : end'...'',
    'begin; rescue A=>b.c then end'...'',
    'begin; rescue A=>@a; end'...'',
    'begin; rescue A=>@@a; end'...'',
    'begin; rescue A=>$a; end'...'',
    'begin; rescue A=>$!; end'...'',
    'begin; rescue A=>A; end'...'',
    'begin; rescue A=>::B; end'...'',
    'begin; rescue A=>A::B; end'...'',
    'begin; rescue A=>a.b; end'...'',
    'begin; rescue A=>p[4]; end'...'',
    'begin; rescue A=>p (1).m; end'...'',
    'm ? n : (b)'...'',
    'm ? n : (begin; end)'...'',
    'begin; rescue =>e; p end'...'',
    'begin; rescue E=>e; p end'...'',
    'a0 = *t rescue b0'...'',
    'a0 = p 1.q rescue b0'...'',
    'a0 = p 1.q,2 rescue b0'...'',
    'def a(b,c) e=*g end'...'',
    'def a(b,c) d,e=f,g end'...'',
    'def a b,d=p (1.10).m,*f,&g; end'...'',
    'a=*b rescue c rescue d'...'',
    'a=b rescue c rescue d'...'',
    '*a0 = *a=b'...'',
    'a0 = *a=b'...'',
    '*a[z]=c rescue b'...'',
    'a0 = @data.[]=(*args) rescue b0'...'',
    'm{|p (1..10).method(:each)[*b]| c}'...'',
    'm{|p (1)[b]| }'...'',
    'm{|p ()[b]| }'...'',
    'm{|p (1,2)[b]| }'...'',
    'a0 = b 1 do end rescue c'...'',
    'a0 = b do end rescue c'...'',
    'a0 = b() do end rescue c'...'',
    'p (1.10).me,z=c'...'',
    'p(1.10).me,z=c'...'',
    'p (1..10).method(:each)[z,*b],z=c,y'...'',
    'a = p rescue b'...'',
    'a,g = p rescue b'...'',
    ' false and  ( t) '...'',
    'a0 = begin;rescue;end rescue b0'...'',
    'a0 = def a b,d=e; end rescue b0'...'',
    'a0 = def a b=c,d=e; end rescue b0'...'',
    'def a b,d=e; end'...'',
    '{p (1) => a}'...'',
    '{p (1).m => a}'...'',
    'a0 = begin; end rescue b0'...'',
    'a0 = begin; a; end rescue b0'...'',
    'a0 = (a;b;) rescue b0'...'',
    'a0= y,begin a end rescue b0'...'',
    'a0= y,*begin a end rescue b0'...'',
    'a0= *begin a end rescue b0'...'',
    'a0= y,begin a end rescue b0,z'...'',
    'a0= begin a end rescue b0,z'...'',
    'a0= begin a end rescue b0,*z'...'',
    '*a0= begin a end rescue b0'...'',
    'y,*a0= begin a end rescue b0'...'',
    'y,a0= begin a end rescue b0'...'',
    'y,a0,= begin a end rescue b0'...'',
    'y,a0,* = begin a end rescue b0'...'',
    'y,a0,*j= begin a end rescue b0'...'',
    '(a0,)= begin a end rescue b0'...'',
    'begin a end rescue b0'...'',

    'c do p (110).m end'...'',
    'a=a do end'...'',
    'b a=a do end'...'',
    'b=1;a=b do end'...'',
    'st = a::F f rescue p'...'',
    'st = a.F f rescue p'...'',
    'st = F f rescue p'...'',
    '{p (1).metho => 4}'...'',
    'p (1).c'...'',
    'k=z=c,d rescue b'...'',
    'a=1,2 rescue 4'...'',
    'a,*k=p,z=c rescue b'...'',
    'a,k=p,z=c rescue b'...'',
    'a=p,z=c rescue 0'...'',
    'a =p (1).m,z=c rescue b'...'',
    'z{|| p (1).m}'...'',
    'b or p (1).m'...'',
    'b and p (1).m'...'',
    'b || p (1).m'...'',
    'b && p (1).m'...'',
    'b ? p (1).m : g'...'',

    'c unless p (1).m'...'',
    'c if p (1).m'...'',
    'c until p (1).m'...'',
    'c while p (1).m'...'',

    'unless p (1).m; end'...'',
    'if p (1).m; end'...'',
    'until p (1).m; end'...'',
    'while p (1).m; end'...'',
    'z if c..p (1).m'...'',
    'z if c...p (1).m'...'',
    'yield *p (1).m'...'',
    'p p (1).m'...'',
    'a.p p (1).m'...'',
    'p p ().m'...'',
    'p p (1,2).m'...'',
    'p p(1).m'...'',
    'p(p (1).m)'...'',

    'begin; m; rescue A=>p (1).metho; end'...'',

    'h(a=>p (1).metho)'...'',
    'h a=>p (1).metho'...'',
    'h(a=>p (1).metho,2)'...'',
    'h a=>p (1).metho,2'...'',

    '{a=>p (1).metho}'...'',
    '{a=>p ().metho}'...'',
    '{a=>p (1,2).metho}'...'',

    '"#{begin}"'...'',
    'a0 = "#{begin; a; rescue b; p (1..10).method(:each)}" rescue b0'...'',

    'begin; p; rescue p(4).m; end'...'',
    'begin; p; rescue p (4).m; end'...'',
    'begin; p; rescue p ().m; end'...'',
    'begin; p; rescue p (4,5).m; end'...'',



    ' p rescue p a4.m'...'',
    ' p rescue p (4).m'...'',
    ' p rescue p ().m'...'',
    ' p rescue p (4,5).m'...'',
    'a0 += p (1).d'...'',
    'a0 = p (1..10).method(:each)[*b]=c rescue b0'...'',
    '*a0 = *(a= c)'...'',
    '*a0 = (*a= c)'...'',
    '*a0 = *a=c'...'',
    'a0 = (*a=c)'...'',
    'a0 = *a=c'...'',
    'a0 = *(a=c)'...'',
    'a0 = m,*a=c'...'',
    'a0 = m,*a=c rescue b0'...'',
    'a0 = *a+=c'...'',
    'a0 += *a=c'...'',
    'a0 += *a+=c'...'',

    'a0 = begin raise "foo" end rescue b0'...'',
    'a0= a0 = su rescue b0 rescue b0'...'',
    'class Using_Paths < Doc < FigureMaker; end'...'',
    'a=begin end rescue b'...'',
    'a=begin b end rescue c'...'',
    'a=begin b; rescue c; d end rescue e'...'',
    'a=() rescue b'...'',
    'a=(b) rescue c'...'',
    'a=(b c) rescue d'...'',
    'a=(b(c)) rescue d'...'',

    'a rescue b if c'...'',
    'begin end until a'...'',
    'o ? begin end : l'...'',
    'o && begin end'...'',
    'o || begin end'...'',
    'o and begin end'...'',
    'o or begin end'...'',
    'not begin end'...'',
    '! begin end'...'',
    'Sw.===(anything).should == true'...'',     
    'def line(type, txt, selector, host = host, port = port); end'...'',
    'a0 = def a.b; end rescue b0'...'',
    'def a.b; end rescue b0'...'',
 
    'a = (r(e) +t(j)) rescue 2'...'',
    'a = (r e +t(j)) rescue 2'...'',
    'a = (p) rescue 2'...'',
    'a = (p(1)) rescue 2'...'',
    'a = (p(1,g)) rescue 2'...'',
    'a = (p 1,g) rescue 2'...'',

    'a = r(e) +t(j) rescue 2'...'',
    'a = r e +t(j) rescue 2'...'',
    'a = p rescue 2'...'',
    'a = p(1) rescue 2'...'',
    'a = p(1,g) rescue 2'...'',
    'a = p 1,g rescue 2'...'',
    '//nonsense'...'',
    '//xenon'...'',
    'def wait; begin; ync; mup; end ;end'...'',
    'first or (first,last=*rand_pos_pair)'...'',
    'return - @com_disk'...'',
    'continue - @com_disk'...'',
    'yield - @com_disk'...'',
    'break - @com_disk'...'',
    'next - @com_disk'...'',
    'raise - @com_disk'...'',
    'undef - com_disk'...'',
    'alias - com_disk'...'',
    'defined? - @com_disk'...'',
    'a rescue - @com_disk'...'',
    'if a; else - @com_disk;end'...'',
    'if - @com_disk;end'...'',
    'unless - @com_disk;end'...'',
    'while - @com_disk;end'...'',
    'until - @com_disk;end'...'',
    'begin - @com_disk;end'...'',
    'end - @com_disk'...'',
    'b do - @com_disk end'...'',
    'for - @com_disk in a do b end'...'',
    'for a in - @com_disk do b end'...'',
    'if a; b; elsif - @com_disk; c end'...'',
    'begin a; ensure - @com_disk; end'...'',
    'catch - @com_disk'...'',
    'throw - @com_disk'...'',

    'return * @com_disk'...'',
    'continue * @com_disk'...'',
    'yield * @com_disk'...'',
    'break * @com_disk'...'',
    'next * @com_disk'...'',
    'raise * @com_disk'...'',
    'undef * com_disk'...'',
    'alias * com_disk'...'',
    'defined? * @com_disk'...'',
    'a rescue * @com_disk'...'',
    'if a; else * @com_disk;end'...'',
    'if * @com_disk;end'...'',
    'unless * @com_disk;end'...'',
    'while * @com_disk;end'...'',
    'until * @com_disk;end'...'',
    'begin * @com_disk;end'...'',
    'end * @com_disk'...'',
    'b do * @com_disk end'...'',
    'for * @com_disk in a do b end'...'',
    'for a in * @com_disk do b end'...'',
    'if a; b; elsif * @com_disk; c end'...'',
    'begin a; ensure * @com_disk; end'...'',
    'catch * @com_disk'...'',
    'throw * @com_disk'...'',


    'a.return - @com_disk'...'',
    'm.continue - @com_disk'...'',
    'm.yield - @com_disk'...'',
    'm.break - @com_disk'...'',
    'm.next - @com_disk'...'',
    'm.raise - @com_disk'...'',
    'm.undef - com_disk'...'',
    'm.alias - com_disk'...'',
    'm.defined? - @com_disk'...'',
    'm.rescue - @com_disk'...'',
    'm.else - @com_disk'...'',
    'm.if - @com_disk'...'',
    'm.unless - @com_disk'...'',
    'm.while - @com_disk'...'',
    'm.until - @com_disk'...'',
    'm.begin - @com_disk'...'',
    'm.end - @com_disk'...'',
    'm.do - @com_disk'...'',
    'm.for - @com_disk'...'',
    'm.in - @com_disk '...'',
    'm.elsif - @com_disk'...'',
    'm.ensure - @com_disk'...'',
    'm.catch - @com_disk'...'',
    'm.throw - @com_disk'...'',

    'm.return * @com_disk'...'',
    'm.continue * @com_disk'...'',
    'm.yield * @com_disk'...'',
    'm.break * @com_disk'...'',
    'm.next * @com_disk'...'',
    'm.raise * @com_disk'...'',
    'm.undef * com_disk'...'',
    'm.alias * com_disk'...'',
    'm.defined? * @com_disk'...'',
    'm.rescue * @com_disk'...'',
    'm.else * @com_disk'...'',
    'm.if * @com_disk'...'',
    'm.unless * @com_disk'...'',
    'm.while * @com_disk'...'',
    'm.until * @com_disk'...'',
    'm.begin * @com_disk'...'',
    'm.end * @com_disk'...'',
    'm.do * @com_disk'...'',
    'm.for * @com_disk '...'',
    'm.in * @com_disk'...'',
    'm.elsif * @com_disk'...'',
    'm.ensure * @com_disk'...'',
    'm.catch * @com_disk'...'',
    'm.throw * @com_disk'...'',


    '%W[Ml.#{G.ma}.Z M.#{G.m} yaml.Z ]'...'',
    '@data.[]=(*args)'...'',
    'have_header(*curses=%w"ncurses.h")'...'',
    'Proc{|a,&b|}'...'',
    'Proc{|a,&b| c}'...'',
    'e if f rescue 0'...'',
    'e unless f rescue 0'...'',
    'e while f rescue 0'...'',
    'e until f rescue 0'...'',
    'begin;mode;rescue;o_chmod rescue nil;end'...'',
    'stat = File.stat f rescue error'...'',
    'case a;else;end'...'',
    'n = o ? begin end : l'...'',
    'o ? begin end : l'...'',
    'if o then begin end else l end'...'',
    'o && begin end'...'',
    'o || begin end'...'',
    'o & begin end'...'',
    'o | begin end'...'',
    'o and begin end'...'',
    'o or begin end'...'',
    'o if begin end'...'',
    'o unless begin end'...'',
    'o while begin end'...'',
    'o until begin end'...'',

    'def self; end'...'',
    'def nil; end'...'',
    'def true; end'...'',
    'def false; end'...'',
    'def __FILE__; end'...'',
    'def __LINE__; end'...'',
    'def __END__; end'...'',

    'def continue; end'...'',
    'def yield; end'...'',
    'def break; end'...'',
    'def next; end'...'',
    'def return; end'...'',
    'def raise; end'...'',
    'def undef; end'...'',
    'def alias; end'...'',
    'def def; end'...'',
    'def module; end'...'',
    'def class; end'...'',
    'def defined?; end'...'',
    'def rescue; end'...'',
    'def else; end'...'',
    'def if; end'...'',
    'def unless; end'...'',
    'def while; end'...'',
    'def until; end'...'',
    'def begin; end'...'',
    'def end; end'...'',
    'def do; end'...'',
    'def for; end'...'',
    'def in; end'...'',
    'def elsif; end'...'',
    'def ensure; end'...'',
    'def catch; end'...'',
    'def throw; end'...'',

    'def continue.x; end'...'',
    'def yield.x; end'...'',
    'def break.x; end'...'',
    'def next.x; end'...'',
    'def return.x; end'...'',
    'def raise.x; end'...'',
    'def defined?.x; end'...'',
    'def catch.x; end'...'',
    'def throw.x; end'...'',
    'def self.x; end'...'',
    'def nil.x; end'...'',
    'def true.x; end'...'',
    'def false.x; end'...'',
    'def __FILE__.x; end'...'',
    'def __LINE__.x; end'...'',
    'def __END__.x; end'...'',

    'p p {} do end'...'',
    ' ca (:f).gg '...'',
    '     return gsub() {}'...'',
    'p timeout(5) {45}'...'',
    '%w[  a b]'...'',

    'defined?(MOD_RUBY)..ENV'...'',
    'defined?(MOD_RUBY)...ENV'...'',
    'defined? MOD_RUBY..ENV'...'',
    'defined? MOD_RUBY...ENV'...'',

    'defined?(MOD_RUBY).ENV'...'',
    'defined?(MOD_RUBY)::ENV'...'',
    'defined? MOD_RUBY.ENV'...'',
    'defined? MOD_RUBY::ENV'...'',

    'defined?(MOD_RUBY)[ENV]'...'',
    'defined?(MOD_RUBY)[ENV]=1'...'',
    'defined? MOD_RUBY[ENV]'...'',
    'defined? MOD_RUBY[ENV]=1'...'',
    'defined?(MOD_RUBY)**ENV'...'',
    'defined? MOD_RUBY**ENV'...'',


    'defined?(MOD_RUBY)<=ENV'...'',
    'defined?(MOD_RUBY)>=ENV'...'',
    'defined? MOD_RUBY<=ENV'...'',
    'defined? MOD_RUBY>=ENV'...'',
    'defined?(MOD_RUBY)<ENV'...'',
    'defined?(MOD_RUBY)>ENV'...'',
    'defined? MOD_RUBY<ENV'...'',
    'defined? MOD_RUBY>ENV'...'',
    'defined?(MOD_RUBY)|ENV'...'',
    'defined?(MOD_RUBY)&ENV'...'',
    'defined? MOD_RUBY|ENV'...'',
    'defined? MOD_RUBY&ENV'...'',
    'defined?(MOD_RUBY)<<ENV'...'',
    'defined?(MOD_RUBY)>>ENV'...'',
    'defined? MOD_RUBY<<ENV'...'',
    'defined? MOD_RUBY>>ENV'...'',
    'defined?(MOD_RUBY)+ENV'...'',
    'defined?(MOD_RUBY)-ENV'...'',
    'defined? MOD_RUBY+ENV'...'',
    'defined? MOD_RUBY-ENV'...'',
    'defined?(MOD_RUBY)*ENV'...'',
    'defined?(MOD_RUBY)/ENV'...'',
    'defined? MOD_RUBY*ENV'...'',
    'defined? MOD_RUBY/ENV'...'',
    'defined?(MOD_RUBY)%ENV'...'',
    'defined?(MOD_RUBY)^ENV'...'',
    'defined? MOD_RUBY%ENV'...'',
    'defined? MOD_RUBY^ENV'...'',


    'defined?(MOD_RUBY)==ENV'...'',
    'defined?(MOD_RUBY)===ENV'...'',
    'defined? MOD_RUBY==ENV'...'',
    'defined? MOD_RUBY===ENV'...'',
    'defined?(MOD_RUBY)!=ENV'...'',
    'defined?(MOD_RUBY)<=>ENV'...'',
    'defined? MOD_RUBY!=ENV'...'',
    'defined? MOD_RUBY<=>ENV'...'',
    'defined?(MOD_RUBY)=~ENV'...'',
    'defined?(MOD_RUBY)!~ENV'...'',
    'defined? MOD_RUBY=~ENV'...'',
#    'defined? MOD_RUBY!~ENV'...'', #not legal


    'defined? MOD_RUBY=ENV'...'',
    'defined? MOD_RUBY+=ENV'...'',
    'defined? MOD_RUBY-=ENV'...'',
    'defined? MOD_RUBY/=ENV'...'',
    'defined? MOD_RUBY*=ENV'...'',
    'defined? MOD_RUBY%=ENV'...'',
    'defined? MOD_RUBY**=ENV'...'',
    'defined? MOD_RUBY<<=ENV'...'',
    'defined? MOD_RUBY>>=ENV'...'',
    'defined? MOD_RUBY&&=ENV'...'',
    'defined? MOD_RUBY||=ENV'...'',
    'defined? MOD_RUBY&=ENV'...'',
    'defined? MOD_RUBY|=ENV'...'',
    'defined? MOD_RUBY^=ENV'...'',

    'self[-1] += succ_table'...'',
    'begin; p rescue;o_chmod rescue nil;end'...'',
    'defined?(MOD_RUBY) && ENV'...'',
    'defined?(MOD_RUBY) || ENV'...'',
    'defined?(MOD_RUBY) and ENV'...'',
    'defined?(MOD_RUBY) or ENV'...'',
    'defined? MOD_RUBY && ENV'...'',
    'defined? MOD_RUBY || ENV'...'',
    'defined? MOD_RUBY and ENV'...'',
    'defined? MOD_RUBY or ENV'...'',
    
    'alias $ERROR_INFO $!'...'',
    'alias :$ERROR_INFO $!'...'',
    'alias $ERROR_INFO :$!'...'',
    'alias :$ERROR_INFO :$!'...'',
    'defined?(BINARY) ? BINARY : 0'...'',
    'ca (:f){}'...'',
    'def run; system; else;end'...'',
    '"n#@t#{l}#@m"'...'',
    'f a rescue b'...'',
    'f g a rescue b'...'',
    'defined? a rescue b'...'',
    'yield a rescue b'...'',
    'return a rescue b'...'',
    'break a rescue b'...'',
    'next a rescue b'...'',
    "%q[asdfadfas\\']"...'',
    ' begin; rescue ;;error; end'...'',
    'a.b=~c'...'',
    "x::I = 1"...'',
    "x.I = 1"...'',
    "X::I = 1"...'',
    "X.I = 1"...'',
    "  alias :'=$=' :e "...'',
    "  alias :'==' :e "...'',
    '  alias :"==" :e '...'',
    '  alias :"==\\1" :e '...'',
    'alias :\'foo\' bar'...'',
    'alias :"foo#{dfgdfg}" bar'...'',
    'alias :"foo" bar#{mdfgnxc}'...'',
    'alias :"foo#{dfgdfg}" bar#{fgsdg}'...'',
    '+-+-+++++---+++-++-0'...'',
    "%q[\\']"...'',
    '%((((())#{})))'...'',
    '%(((#{}(()))))'...'',
    '%(((((#{})))))'...'',
    'ev.length/=2'...'',
    'ev.length /=2'...'',
    'ev.length/= 2'...'',
    'ev.length /= 2'...'',
    'a.~@'...'',
    'a.~'...'',
    'a::~@'...'',
    'a::~'...'',
    'def ~@; end'...'',
    'def a.~@; end'...'',
    'def a::~@; end'...'',
    ':~@'...'',
    "%\n\\c\nyy\n"...'',
    "(%\r\neee\#{kk}\\\r\n\r\n)"...'',
    "(%\r\neee\#{kk}\\\n\r\n)"...'',
    "(%\r\neee\#{kk}\r\n)"...'',
    "(%\r\neee\#{kk}\\\r\n\n)"...'',
    "(%\r\neee\#{kk}\\\n\n)"...'',
    "(%\r\neee\#{kk}\n)"...'',
    "(%\neee\#{kk}\\\r\n\r\n)"...'',
    "(%\neee\#{kk}\\\n\r\n)"...'',
    "(%\neee\#{kk}\r\n)"...'',
    "(%\neee\#{kk}\\\r\n\n)"...'',
    "(%\neee\#{kk}\\\n\n)"...'',
    "(%\neee\#{kk}\n)"...'',
    '%@#@_@'...'',
    '%$#$_$'...'',
    '$;'...'',
    '$;=1'...'',
    '"\c""'...'',
    '"\C-""'...'',
    '"\M-""'...'',
    '"\C-\M-""'...'',
    '"\M-\C-""'...'',
    'begin a; rescue b=>e; @c=e.f; end'...'',
    'a=b do end'...'',
    'c a=b do end'...'',
    'filename =~ //'...'',
    'filename !~ //'...'',
    'Dir.chdir DIR do end'...'',
    'DIR do end'...'',
    'Dir.chdir DIR { }'...'',
    'p ()'...'',
    'p (1,2,3)'...'',
    'p(1..10).method(:each)'...'',
    'a.p(1..10).method(:each)'...'',
    'a::p(1..10).method(:each)'...'',
    'p (1..10).method(:each)'...'',
    'a.p (1..10).method(:each)'...'',
    'a::p (1..10).method(:each)'...'',
    'p(1..10,1).method(:each)'...'',
    'a.p(1..10,1).method(:each)'...'',
    'a::p(1..10,1).method(:each)'...'',
    'p (1..10,1).method(:each)'...'',
    'a.p (1..10,1).method(:each)'...'',
    'a::p (1..10,1).method(:each)'...'',
    'p().method(:each)'...'',
    'a.p().method(:each)'...'',
    'a::p().method(:each)'...'',
    'p ().method(:each)'...'',
    'a.p ().method(:each)'...'',
    'a::p ().method(:each)'...'',
    'p 1;2;p 1'...'',
    'def self.zz(*,&x) y end'...'',
    'def self.zz(&x) y end'...'',
    'def self.zz(*,&x); end'...'',
    'def self.zz(&x); end'...'',
    '"is #{x}#{"Slim #{2?"W":"S"} "}."'...'',
    '"is #{"Slim #{2?"W":"S"} "}."'...'',
    '"is #{"Slim #{2?"W":"S"} " "rr"}."'...'',
    '%W"is #{x}#{"Slim #{2?"W":"S"} "}."'...'',
    '%W"is #{"Slim #{2?"W":"S"} "}."'...'',
    '%W"is #{"Slim #{2?"W":"S"} " "rr"}."'...'',

    '"is #{"Slim #{2?"W":"S"}"}."'...'',
    '%W"is #{"Slim #{2?"W":"S"}"}."'...'',
    '"is #{"Slim #{2?"W":"S"}"}#{xx}."'...'',
    '%W"is #{"Slim #{2?"W":"S"}"}#{xx}."'...'',
    '"is #{x}#{"Slim #{2?"W":"S"}"}#{xx}."'...'',
    '%W"is #{x}#{"Slim #{2?"W":"S"}"}#{xx}."'...'',
    '"is #{x}#{%W"Slim #{2?"W":"S"}"}#{xx}."'...'',
    '%W"is #{x}#{%W"Slim #{2?"W":"S"}"}#{xx}."'...'',

    '%W"is_#{x}#{"Slim_#{2?"W":"S"}"}#{xx}."'...'',
    '%W"is_#{"Slim_#{2?"W":"S"}"}#{xx}."'...'',
    '%W"is_#{"Slim_#{2?"W":"S"}"}."'...'',
    '%W"is_#{x}#{"Slim_#{2?"W":"S"}_"}."'...'',
    '%W"is_#{"Slim_#{2?"W":"S"}_"}."'...'',
    '%W"is_#{"Slim_#{2?"W":"S"}_" "rr"}."'...'',

    "%W\"is \#{x}\#{%W'Slim \#{2?W: S} '}.\""...'',
    "%W\"is \#{x}\#{%w'Slim \#{2?W: S} '}.\""...'',
    "%W\"is \#{x}\#{%Q'Slim \#{2?W: S} '}.\""...'',
    "%W\"is \#{x}\#{\"Slim \#{2?W: S} \"}.\""...'',
    "%W\"is \#{x}\#{%q'Slim \#{2?W: S} '}.\""...'',
    "%W\"is \#{x}\#{%x'Slim \#{2?W: S} '}.\""...'',
    "%W\"is \#{x}\#{%r'Slim \#{2?W: S} '}.\""...'',
    "%W\"is \#{x}\#{`Slim \#{2?W: S} `}.\""...'',
    "%W\"is \#{x}\#{/Slim \#{2?W: S} /}.\""...'',
    "%\"is \#{x}\#{%r'Slim \#{2?W: S} '}.\""...'',
    "\"is \#{x}\#{%r'Slim \#{2?W: S} '}.\""...'',
    "\"is \#{x}\#{%'Slim \#{2?W: S} '}.\""...'',
    "\"is \#{x}\#{'Slim \#{2?W: S} '}.\""...'',

    'case $1; when 0,*[2,66]: p 1; when 3; 4 else p 2 end'...'',
    'case $1; when 0,*a: p 1; when 3; 4 else p 2 end'...'',
    ' p(String <<- Class)'...'',
    ' p(String << - Class)'...'',
    ' p(String >>- Class)'...'',
    ' p(String >> - Class)'...'',
    '"2266"**"#{22}" "#{44}" "55" "#{66}"'...'',
    '["2266", "#{22}" "#{44}" "55" "#{66}"]'...'',
    'assert_equal("22aacd445566", "#{22}aa" "cd#{44}" "55" "#{66}")'...'',
    '"fff#{r}" "#{66}"'...'',
    '"fff#{r}" "#{66}3"'...'',
    '%q[\\[\\]\\\\]'...'"[]\\\\"',
    '%w[\\[\\]\\\\]'...'(array "[]\\\\")',
    '%r[\\[\\]\\\\]'...'/\\[\\]\\\\/',
    '%s[\\[\\]\\\\]'...'',
    '%q"\\"\\\\"'...'"\\"\\\\"',
    '%w"\\"\\\\"'...'(array "\\"\\\\")',
    '%r"\\"\\\\"'...'/"\\\\/',
    '%s"\\"\\\\"'...'',
    "%q'\\'\\\\'"...'"\'\\\\"',
    "%w'\\'\\\\'"...'(array "\'\\\\")',
    "%r'\\'\\\\'"...'/\'\\\\/',
    "%s'\\'\\\\'"...'',
    "'\\'\\\\'"...'"\'\\\\"',
    '0x1134234aefeb'...'18915628085227',
    '0d57896675'...'57896675',
    '0o234526546'...'41069926',
    '0234526546'...'41069926',
    '0b1010001001011111'...'41567',
    '"a"; b'...'"a" (b)',
    "()"...'()',
    "1"...'1',
    "1+1"...'(+ 1 1)',
    "2+3*4"...'(+ 2 (* 3 4))',
    "a"...'(a)',
    "a.b"...'((a) b)',
    "[]"...'[]',
    "[3]"...'[3]',
    "[3,4]"...'[3 4]',
    "list()"...'()',
    "list(3)"...'(3)',
    "list(3,4)"...'(3 4)',
    "b[]"...'((b) [])',
    "b[3]"...'((b) [] 3)',
    "a.b[]"...'(((a) b) [])',
    "a.b[3]"...'(((a) b) [] 3)',
    "a::b[3]"...'(((a) b) [] 3)',
    "a if b"...'(if (b) (a))',
    "a unless b"...'(unless (b) (a))',
    "a while b"...'(while (b) (a))',
    "a until b"...'(until (b) (a))',
    "a and b"...'(and (a) (b))',
    "a or b"...'(or (a) (b))',
    "2**3[4]"...'(** 2 (3 [] 4))',
    "+a[2]"...'(+@ ((a) [] 2))',
    "$a=b"...'(set $a (b))',
    '@a=b'...'(set @a (b))',
    '@@a=b'...'(set @@a (b))',
    'A=b'...'(set A (b))',
    'a=b=1'...'(set a (set b 1))',
    '$a+=b'...'(set $a (+ $a (b)))',
    '@a+=b'...'(set @a (+ @a (b)))',
    '@@a+=b'...'(set @@a (+ @@a (b)))',
    'A+=b'...'(set A (+ A (b)))',
    'a=b+=1'...'(set a (set b (+ b 1)))',
    'a+=b=1'...'(set a (+ a (set b 1)))',
    'a+=b+=1'...'(set a (+ a (set b (+ b 1))))',
    'a(b)'...'(a (b))',
    'b=1;a(b)'...'(set b 1) (a b)',
    'a(*b)'..."(eval (cons 'a ((b) list)))",
    'a(b,*c)'...'',
    'a(&b)'...'(a (b))',
    'a(b,&c)'...'(a (b) (c))',
    'a(b,c,&c)'...'(a (b) (c) (c))',
    'a(b,*c,&c)'...'',
    'a(){b}'...'(a (do () (b)))',
    'a(b){c}'...'(a (b) (do () (c)))',
    'a(b,c){c}'...'(a (b) (c) (do () (b)))',
    'a(b,*c){c}'...'',
    'a()'...'(a)',
    'a{b}'...'(a (do () (b)))',
    'a b{c}'...'(a (b (do () (c))))',
    'a b,c {c}'...'(a (b (do () (c))))',
    'a b,*c {c}'...'',
    'a do b end'...'(a (do () (b)))',
    'a b do c end'...'(a (b) (do () (c)))',
    'a b,c do c end'...'(a (b) (c) (do () (b)))',
    'a b,*c  do c end'...'',
    'a%=b'...'(set a (% a (b)))',
    'a.b%c'...'(% ((a) b) (c))', 
    'a.b%=c'...'(set _t1* (a)) (_t1* b= (% (_t1* b) (c)))', 
    'a.b.c%=d'...'(set _t1* ((a) b)) (_t1* c= (% (_t1* c) (d)))',
    'a::b%=c'...'(set _t1* (a)) (_t1* b= (% (_t1* b) (c)))',
    'a.b[]=d'...'(((a) b) []= (d))',
    'a.b[c]=d'...'(((a) b) []= (c) (d))',  #???
    'a.b[c,c2]=d'...'(((a) b) []= (c) (c2) (d))',
#    'a.b[]%=d'...'',  #causes segfault with ruby 1.8.5 and ParseTree... fixed in 1.8.6
    'a.b[c]%=d'...'',
    'a.b[c,c2]%=d'...'',
    'a(b).c%=d'...'',
    'a b c'...'(a (b (c)))',
    'if a; b end'...'(if (a) (b))',
    'if a: b end'...'(if (a) (b))',
    'if a then b end'...'(if (a) (b))',
    'if a; b else c end'...'(if (a) (b) (else (c)))',
    'if a; b elsif c; d end'...'',
    'if a; b elsif c; d else e end'...'',
    'if a; else c end'...'',
    'if a; elsif c; d end'...'',
    'if a; elsif c; d else e end'...'',
    'if a+0; b end'...'',
    'if a+0: b end'...'',
    'if a+0 then b end'...'',
    'if a+0; b else c end'...'',
    'if a+0; b elsif c; d end'...'',
    'if a+0; b elsif c; d else e end'...'',
    'if a+0; else c end'...'',
    'if a+0; elsif c; d end'...'',
    'if a; elsif c; d else e end'...'',

    'unless a; b end'...'',
    'unless a; b else c end'...'',
	
    'while a; b end'...'',
    'until a; b end'...'',

    'case; when b; c end'...'',
    'case a; when b; c end'...'',
    'case a; when b: c end'...'',
    'case a; when b then c end'...'',
    'case a; when b,c,d then d end'...'',
    'case a; when b,c,d: d end'...'',
    'case a; when B,C,D: d end'...'',

    'case a; when b; when c; end'...'',
    'case a; when b; c when d; e end'...'',
    'case a; when b; c; when d; e end'...'',
    'case a; when b; c; when d; e else f end'...'',

    'for a in b; c end'...'',
    'for a in b do c end'...'',
    'for a in b: c end'...'',
    'for a,b in c: d end'...'',
    'for a,b in c; d end'...'',
    'for a,b in c do d end'...'',

    '(a+b)'...'(+ a b)',
    ';a;2;'...'(a) 2',
    ';1;2;'...'1 2',
    '(;1;2;)'...'1 2',
    '!a'...'',
    '::A'...'',
    ':a'...'',
    'defined? a'...'',

    'a**b**c'...'((a) ** ((b) ** (c)))',

    '{a=>b}'...'',
    '{A=>b}'...'',
    '{a a=>b}'...'', #syntax error
    '{a+a=>b}'...'',
    '{a()=>b}'...'',
    '{a{}=>b}'...'',
    '{+a=>b}'...'',
    '{-a=>b}'...'',
    '{~a=>b}'...'',
    '{!a=>b}'...'',
    '{::A=>b}'...'',
    '{:a=>b}'...'',
    '{not a=>b}'...'', #syntax error
    '{defined? a=>b}'...'',
    '{1, 2, 2, 4, 3, 6}'...'',

    
    'begin rescue B; rescue ; end'...'',
    'begin rescue ; end'...'',
    'begin; a; rescue; end'...'',
    'begin; a; rescue B; end'...'',
    'begin; a; rescue B; d; end'...'',
    'begin; a; rescue B; d; e; end'...'',
    'begin; a; rescue B=>c; end'...'',
    'begin; a; rescue =>c; end'...'',
    'begin; a; rescue B=>c; d; end'...'',
    'begin; a; rescue B=>c; d; e; end'...'',
    'begin; a; rescue B; d; else f; end'...'',
    'begin; a; rescue B; d; else f; ensure; e; end'...'',
    'begin; a; rescue B=>c; d; else f; ensure; e; end'...'',

    'begin; a; else f; ensure; e; end'...'',
    'begin; a; b; else f; g ensure; e; end'...'',
    'begin; a; else f; g ensure; e; end'...'',
    'begin; a; b; else f ensure; e; end'...'',

    'begin end'...'',
    'begin; end'...'',
    'begin; else; ensure; end'...'',
    'begin else ensure end'...'',

    'begin a end while b'...'',
    'begin a end until b'...'',
    'begin a end while !b'...'',
    'begin a end until !b'...'',
    'begin a end while not b'...'',
    'begin a end until not b'...'',
    'begin a end while b==c'...'',
    'begin a end until b==c'...'',
    'begin a end while b=~c'...'',
    'begin a end until b=~c'...'',
    'begin a end while b!=c'...'',
    'begin a end until b!=c'...'',
    'begin a end while b!~c'...'',
    'begin a end until b!~c'...'',

    'begin end while b'...'',
    'begin a;b end while c'...'',

    'begin rescue a; b end while c'...'',
    'begin a; rescue b; c end while d'...'',
    'begin a;b; rescue c; d end while e'...'',

    'begin else a end while b'...'',
    'begin a; else b end while c'...'',
    'begin a;b; else c end while d'...'',

    'begin ensure a end while b'...'',
    'begin a; ensure b end while c'...'',
    'begin a;b; ensure c end while d'...'',

    'a+b=c'...'(+ (a) (set b (c)))',
    'b.c=1'...'((b) c= 1)',
    'b.c=d'...'((b) c= (d))',
    'b::c=d'...'((b) c= (d))',
    'B::c=d'...'(B c= (d))',
    'a+b.c=d'...'(+ a ((b) c= (d)))',
    'a+b[c]=d'...'',
    'a+B::c=d'...'(+ (a) (B c= (d)))',
    
    'def b; c end'...'',
    'b do c end'...'',

    'true'...'true',
    'false'...'false',
    'nil'...'nil',
    ':foo'...'',
    '[]'...'',
    '[a]'...'',
    '[a,b]'...'',
    '{}'...'',
    '{a=>b}'...'',
    '{a=>b,c=>d}'...'',
    '0'...'0',
    '0.0'...'0.0',
    '$1'...'$1',
    '$0'...'$0',
    '$&'...'$&',
    '$$'...'$$',
    '$:'...'$:',
    '$*'...'$*',
    '$a'...'$a',
    '@a'...'@a',
    '@@a'...'@@a',
    'a=1;a'...'(set a 1) a',
    'proc{yeild}'...'',
    "'a'"..."",
    '/a/'...'',
    '`a`'...'',
    '%w[a]'...'',
    '%w[a b]'...'',
    '%w[a b c]'...'',
    '%W[a]'...'',
    '%W[a b]'...'',
    '%W[a b c]'...'',
    '%W[a#{}]'...'',
    '%W[a b#{}]'...'',
    '%W[a b c#{}]'...'',

    '%[a]'...'"a"',
    '%[a b]'...'"a b"',
    '%[a b c]'...'"a b c"',
    '%q[a]'..."'a'",
    '%q[a b]'..."'a b'",
    '%q[a b c]'..."'a b c'",
    'self'...'self',
    '__FILE__'...'',
    '__LINE__'...'',

    'while false; break end'...'',
    'while false; next end'...'',
    'while false; redo end'...'',
    'while false; retry end'...'',

    'while false; break end'...'',
    'until false; break end'...'',
    'for a in b; break end'...'',
    'loop do break end'...'',

    'loop { break 1,2,*[3,4] }'...'',
    'loop { break 1,2 }'...'',
    'loop { break 1}'...'',
    'loop { break(1)}'...'',
    'loop { break }'...'',
    'loop { break() }'...'',

    'while false; break *[3,4] end'...'',
    'while false; break 1,2,*[3,4] end'...'',
    'while false; break 1,2 end'...'',
    'while false; break(1) end'...'',

    'a.b c'...'((a) b (c))',
    'RuntimeError.new()'...'(RuntimeError new)',
    'RuntimeError.new("foo")'...'(RuntimeError new "foo")',
    'if (true) :foo end'...'',
    'raise'...'',
    'raise "foo"'...'',
    'raise RuntimeError'...'',
    'raise RuntimeError,"foo"'...'',
    'raise RuntimeError.new("foo")'...'',
    'return'...'',
    'return a'...'',
    'return a,b'...'',
    'return a,b,*c'...'',

    'BEGIN {a}'...'',
    'END {a}'...'',
    'BEGIN {}'...'',
    'END {}'...'',
    'BEGIN {a};b'...'',
    'END {a};b'...'',
    'BEGIN {};a'...'',
    'END {};a'...'',

    '"foo" "bar"'...'"foobar"',

    '""'...'""',

    'loop{break{}}'...'',
    'loop{break{a=>b}}'...'',
    'def foo; return{} end'...'',
    'def foo; return{a=>b} end'...'',

    'foo do return{} end'...'',
    'foo do return{a=>b} end'...'',

    'a.return'...'((a) return)',
    'a.return {}'...'',
    'a.break {}'...'',
    'a.return {b}'...'',
    'a.break {b}'...'',
    '()'...'()',
    'nil;a'...'() (a)',
    'true;a'...'(true) (a)',
    'false;a'...'(false) (a)',
    'self;a'...'(self) (a)',
    '();a'...'() (a)',
    '__FILE__;a'...'',
    '__LINE__;a'...'',
    '1;a'...'',
    '"1";a'...'',
    '"1#{2}";a'...'',

    '~a**2'...'', 
    '!a**2'...'', 
    '+a**2'...'', 
    '-a**2'...'', 
  
    '--a'...'',

    '%W[a#{b}c]'...'',
    '%W[a #{b}c]'...'',
    '%W[a#{b} c]'...'',
    '%W[a #{b} c]'...'',

    '"a b c #{d}"'...'',
    '"a b c #{d}\n"'...'',
    '"a b c #{d} "'...'',
    '"#{a} b c"'...'',

    'a.BEGIN {}'...'',
    'a.END {}'...'',

    'p??y'...'',       #bug in rubylexer 0.6.2, no workaround
    '1.+?y'...'',      #bug in rubylexer 0.6.2, no workaround
    'loop{break{}.size}'...'',

    '/a b #{c}/'...'',

    '`a b #{c}`'...'',
    'a{}'...'',
    'a rescue b and c'...'',
    'a rescue b or c'...'',
    'a rescue b if c'...'',

    'def a.b; end'...'',
    'a=""; def a.b; end'...'',
    'def A.b; end'...'',
    'a=""; def (a).b; end'...'',
    'a=""; def (a()).b; end'...'',
    'a=""; def ("a"+"").b; end'...'',
    'def (a.b.c).b; end'...'',
    'def (a).b; end'...'',

    'a.b do end'...'',
    'a=""; a.b do end'...'',
    'A.b'...'',
    'A.b do end'...'',
    '(a).b'...'',
    'a=""; (a).b'...'',
    '(a).b do end'...'',
    'a=""; (a).b do end'...'',
    'a=""; (a()).b do end'...'',
    'a=""; ("a"+"").b do end'...'',
    '(a.b.c).b do end'...'',
    '(a).b do end'...'',

    'def a b=c,d=e,*f; g end'...'',
    'def a b=c,d=e,*f; end'...'',
    'def a b=c,d=e,&f; g end'...'',
    'def a b=c,d=e,&f; end'...'',
    'def a b=c,d=e,*f,&g; h end'...'',
    'def a b=c,d=e,*f,&g; end'...'',

    'a b=c,d=e,*f do g end'...'',
    'a b=c,d=e,*f do end'...'',
    'a b=c,d=e,&f'...'',
    'a b=c,d=e,&f'...'',
    'a b=c,d=e,*f,&g'...'',
    'a b=c,d=e,*f,&g'...'',

    'def a b=c,d=e; f end'...'',
    'def a b=c,d=e; end'...'',
    'def a b=c; end'...'',

    'a b=c,d=e do f end'...'',
    'a b=c,d=e do end'...'',
    'a b=c do end'...'',

    'def a; rescue; b; end'...'',
    'def a; else b end'...'',
    'def a; ensure b end'...'',

    'def a; rescue; b; else c end'...'',
    'def a; else b; ensure c end'...'',
    'def a; rescue; b; ensure c end'...'',


    'def a;d;e; rescue; b; end'...'',
    'def a;d;e; else b end'...'',
    'def a;d;e; ensure b end'...'',

    'def a; d;e;rescue; b; else c end'...'',
    'def a; d;e;else b; ensure c end'...'',
    'def a; d;e;rescue; b; ensure c end'...'',

    'def a; rescue; b; else c; ensure d end'...'',
    'def a; e;f; rescue; b; else c; ensure d end'...'',

    'def a.b; c end'...'',
    'def a.b=; c end'...'',
    'def b i=1; c end'...'',
    'def a.b i=1; c end'...'',
    'def a.b= i=1; c end'...'',
    'def a.b=i=1; c end'...'',
    'def a.b=(i=1); c end'...'',

    'a rescue b'...'', #bug in rubylexer 0.6.2, no workaround
    'a rescue b.c'...'', #bug in rubylexer 0.6.2, no workaround
    'a rescue b;c'...'', #bug in rubylexer 0.6.2, no workaround
    'a rescue (b;c)'...'', #bug in rubylexer 0.6.2, no workaround

    '%W[a]'...'',
    '%W[ a]'...'',
    '%W[a ]'...'',
    '%W[ a ]'...'',

    '%W[a#{}]'...'',
    '%W[ a#{}]'...'',
    '%W[a #{}]'...'',
    '%W[ a #{}]'...'',

    "%W[\va#{}]"...'',
    "%W[\v#{}]"...'',

    '"#{}"'...'',      #bug in rubylexer 0.6.2, no workaround
    "'#{}'"...'',      #bug in rubylexer 0.6.2, no workaround
    '/#{}/'...'',      #bug in rubylexer 0.6.2, no workaround
    '`#{}`'...'',      #bug in rubylexer 0.6.2, no workaround
    '%w"#{}"'...'',      #bug in rubylexer 0.6.2, no workaround
    '%W"#{}"'...'',      #bug in rubylexer 0.6.2, no workaround
    '%W" #{}"'...'',      #bug in rubylexer 0.6.2, no workaround
    '%W"#{} "'...'',      #bug in rubylexer 0.6.2, no workaround
    '%W" #{} "'...'',      #bug in rubylexer 0.6.2, no workaround
    '%q"#{}"'...'',      #bug in rubylexer 0.6.2, no workaround
    '%Q"#{}"'...'',      #bug in rubylexer 0.6.2, no workaround
    '%r"#{}"'...'',      #bug in rubylexer 0.6.2, no workaround
    '%x"#{}"'...'',      #bug in rubylexer 0.6.2, no workaround
    '"#{a}"'...'',      #bug in rubylexer 0.6.2, no workaround

    '%w"a"'...'',     
    '%W"a"'...'',      
    '%W"a#{}"'...'',      
    '%W"a #{}"'...'',      
    '%q"a"'...'',      #
    '%Q"a"'...'',    
    '%r"a"'...'',      
    '%x"a"'...'',      

    '%W[#{a}]'...'',
    '%W[#{a} b]'...'',
    '%W[a #{b}]'...'',
    '%W[a b #{c}]'...'',


    ':"a#{b}"'...'',

    '"a"; b'...'',
    "'a'; b"...'',
    '/a/; b'...'',
    '`a`; b'...'',
    '%w"a"; b'...'',
    '%W"a"; b'...'',
    '"a#{}"; b'...'',

    'module A; end'...'',
    'class A; end'...'',
    'class A<B; end'...'',
    'class<<a; end'...'',

    'module A; b end'...'',
    'class A; b end'...'',
    'class A<B; c end'...'',
    'class<<a; b end'...'',

    'module A; b;c end'...'',
    'class A; b;c end'...'',
    'class A<B; c;d end'...'',
    'class<<a; b;c end'...'',


    '/a/i'...'',
    '/a/m'...'',
    '/a/o'...'',
    '/a/x'...'',
    '/a/imox'...'',

    '/a#{b}/i'...'',
    '/a#{b}/m'...'',
    '/a#{b}/o'...'',
    '/a#{b}/x'...'',
    '/a#{b}/imox'...'',

    '/a#{b}c/i'...'',
    '/a#{b}c/m'...'',
    '/a#{b}c/o'...'',
    '/a#{b}c/x'...'',
    '/a#{b}c/imox'...'',


    #ellipsis as flow control.. annoying
    'a..b'...'',
    'a...b'...'',
    'a..b and c'...'',
    'a..b or c'...'',
    'c and a..b and c'...'',
    'c or a..b or c'...'',

    'not a...b'...'',
    'not a..b'...'',
    'c if a..b'...'',
    'c unless a..b'...'',
    'c if a..b and c'...'',
    'c unless a..b or c'...'',
    'c if c and a..b and c'...'',
    'c unless c or a..b or c'...'',

    'not (a..b)'...'',
    'not (a..b) and c'...'',
    'not z and (a..b) and c'...'',
    'not z and (a..b)'...'',
    'not z and a..b and c'...'',
    'not z and a..b'...'',
    'not (z and (a..b) and c)'...'',
    'not (z and (a..b))'...'',
    'not (z and a..b and c)'...'',
    'not (z and a..b)'...'',
    '!(a..b)'...'',
    'c if (a..b)'...'',
    'c if ((a..b))'...'',
    'c if (((((((((a..b)))))))))'...'',
    'c if !(a..b)'...'',
    'c if not (a..b)'...'',
    'c unless (a..b)'...'',
    'c if (a..b) and c'...'',
    'c unless (a..b) or c'...'',
    'c if c and (a..b) and c'...'',
    'c unless c or (a..b) or c'...'',
    'z if a || b or c..d'...'',
    'z if a || c..d'...'',



    #regex as flow control.. annoying
    '/a..b/'...'',
    '/a...b/'...'',
    '/a..b/ and c'...'',
    '/a..b/ or c'...'',
    'c and /a..b/ and c'...'',
    'c or /a..b/ or c'...'',

    'not /a...b/'...'',
    'not /a..b/'...'',
    'c if /a..b/'...'',
    'c unless /a..b/'...'',
    'c if /a..b/ and c'...'',
    'c unless /a..b/ or c'...'',
    'c if c and /a..b/ and c'...'',
    'c unless c or /a..b/ or c'...'',

    'not (/a..b/)'...'',
    'not (/a..b/) and c'...'',
    'not z and (/a..b/) and c'...'',
    'not z and (/a..b/)'...'',
    'not z and /a..b/ and c'...'',
    'not z and /a..b/'...'',
    'not (z and (/a..b/) and c)'...'',
    'not (z and (/a..b/))'...'',
    'not (z and /a..b/ and c)'...'',
    'not (z and /a..b/)'...'',
    '!(/a..b/)'...'',
    'c if (/a..b/)'...'',
    'c if ((/a..b/))'...'',
    'c if (((((((((/a..b/)))))))))'...'',
    'c if !(/a..b/)'...'',
    'c if not (/a..b/)'...'',
    'c unless (/a..b/)'...'',
    'c if (/a..b/) and c'...'',
    'c unless (/a..b/) or c'...'',
    'c if c and (/a..b/) and c'...'',
    'c unless c or (/a..b/) or c'...'',
    'z if a || b or /c..d/'...'',
    'z if a || /c..d/'...'',

    'a&&b'...'',
    'a||b'...'',
    'a&&=b'...'',
    'a||=b'...'',
    'not a and b'...'',

    'a&&b&&c'...'',
    'a||b||c'...'',
    'a and b and c'...'',
    'a or b or c'...'',
    'a and b or c'...'',
    'a or b and c'...'',
    'a&&b||c'...'',
    'a||b&&c'...'',

    'a!~b'...'',
    'a !~ b'...'',
    'a=~b'...'',
    'z.a=~b'...'',
    'a!=b'...'',
    'a==b'...'',
    'a===b'...'',

    'a<=>b'...'',
    'a<=b'...'',
    'a>=b'...'',
    'a<b'...'',
    'a>b'...'',

    'a^b'...'',
    'a|b'...'',
    'a&b'...'',

    'a>>b'...'',
    'a<<b'...'',

    'a and b && c'...'',
    'a && b and c'...'',
    'a or b || c'...'',
    'a || b or c'...'',

    '(a and b) && c'...'',
    '(a && b) and c'...'',
    '(a or b) || c'...'',
    '(a || b) or c'...'',

    'a&&(b&&c)'...'',
    'a||(b||c)'...'',
    'a and (b and c)'...'',
    'a or (b or c)'...'',

    'a&&b'...'',
    'a &&b'...'',
    'a&& b'...'',
    'a && b'...'',

    'a<<b'...'',
#    'a <<b'...'', #here document, tested elsewhere
    'a<< b'...'',
    'a << b'...'',

    'if not a; b end'...'',
    'unless not a; b end'...'',
    'while not a; b end'...'',
    'until not a; b end'...'',

    '$a||=b'...'',
    '@a||=b'...'',
    '@@a||=b'...'',
    'A||=b'...'',
    'a.b||=c'...'',
    'a[b]||=c'...'',
    'def a() end'...'',
    'def a(b) c end'...'',
    'def a(b=nil) c end'...'',
    'a ? b : c'...'',
    'a=b=c=1;a ?b:c'...'',
    'eof!?nil:true'...'',
    'eof!? a: b'...'',
    'eof!?a'...'',
    'eof??ni : 0'...'',
    'eof ?ni : 0'...'',
    'eof??n'...'',
    'eof ?n'...'',

    'eof??ni() : 0'...'',
    'eof??ni(1) : 0'...'',
    'eof??ni(*a) : 0'...'',
    'eof??ni(&b) : 0'...'',
    'eof??ni{a} : 0'...'',
    'eof??ni(a,b,*c,&d) : 0'...'',
    'eof??ni(a,b,*c){|d| e} : 0'...'',
    'eof??z.ni(a,b,*c){|d| e} : 0'...'',  #syntax error
    'eof??zz.ni(a,b,*c){|d| e} : 0'...'',
    'eof??z0.ni(a,b,*c){|d| e} : 0'...'',
    'ni{|| }'...'',    
    'ni{|b| }'...'',    
    'ni{|b,c| }'...'',    
    'ni{|b,c,*d| }'...'',    
    'ni{|b.c,d[e],*f| }'...'',    
    'ni{|b.c,d.e,*f| }'...'',    
    'ni{|b,(c,d),e| }'...'',    
    'ni{|b,(c,d),*e| }'...'',    
    'ni{|b,(c,*d),*e| }'...'',    
    'ni{|b,(c.f,*d),*e| }'...'',    
    'ni{|b,(c[f],*d),*e| }'...'',    
    'ni{|b,(c.f,g[h],*d),*e| }'...'',    

    'b,c=z'...'',    
    'b,c,*d=z'...'',    
    'b.c,d[e],*f=z'...'',    
    'b,(c,d),e=z'...'',    
    'b,(c,d),*e=z'...'',    
    'b,(c,*d),*e=z'...'',    
    'b,(c.f,*d),*e=z'...'',    
    'b,(c[f],*d),*e=z'...'',    
    'b,(c.f,g[h],*d),*e=z'...'',    
    'k{b,(i,j,c.f,g[h],*d),*e=z}'...'',    

    '::B'...'',
    'A::B'...'',

    'a,b=c,d=e,f'...'',
    'a,*b=c,d=e,*f'...'',

    'a=*b'...'',
    '*a=b'...'',
    '*a=*b'...'',

    '*d=e,*f'...'',
    'd=e,*f'...'',
    '*d=e,f'...'',
    'd=e,f'...'',

    'z{|a,*b.c| d}'...'',
#    'z{|a,*B::C| d}'...'', #ParseTree 2.1.1 segfault
    'z{|a,*b[c]| d}'...'',
    'z{|a,*$b| d}'...'',
    'z{|a,a[b]| d}'...'',
    'z{|a,a[b]| a.c;d}'...'',
    'z{|a,c[b],c| a.c;d}'...'',
    'a=0;z{|a,a[b]| a.c;d}'...'',
    'z{a=0}'...'',
    'a,b=c,d'...'',
    '(a,b)=c,d'...'',
    '((a,b))=c,d'...'',
    '(((a,b)))=c,d'...'',

    '%w[]'...'',
    '%W[]'...'',

    'alias a b'...'',
    'undef a'...'',
    'undef a,b'...'',
    'undef a,b,c'...'',

    'alias :a :b'...'',
    'undef :a'...'',
    'undef :a,:b'...'',
    'undef :a,:b,:c'...'',

    'begin a; rescue b,c,f; d end'...'',

    'begin a;g; rescue b,c,f; d end'...'',

    #for clause can contain multiple assignment too...
    'for a,(b,*c),k,*l in d do e end'...'',
    'for *a in d do e end'...'',

    'f{a=1;g{a=2}}'...'',
    'f{a=1;p a;g{a=2; p g}}'...'',
    'a=1;f{|a| a}'...'',
    'a=1;f{|a,b| a+b}'...'',
    'xxx=0; xxx ?111:222'...'',
    'xxx=yyy=zzz=0; xxx ?yyy:zzz'...'',
    %[xxx=0; xxx ?'':"" ]...'',

    '(a,b),c=[1,2],3'...'',

    'z(1){|a,a[b]| d}'...'',
    
    'a,=1'...'',
    'a,b,=1'...'',
    'a,* =1'...'',
    'a,b,* =1'...'',
    
    'aa,(a,)=1'...'',
    'aa,(a,b,)=1'...'',
    'aa,(a,*) =1'...'',
    'aa,(a,b,*) =1'...'',
    
    'f{|a,|d}'...'',
    'f{|a,b,|d}'...'',
    'f{|a,*|d}'...'',
    'f{|a,b,*|d}'...'',
    
    'f{|aa,(a,)|d}'...'',
    'f{|aa,(a,b,)|d}'...'',
    'f{|aa,(a,*)|d}'...'',
    'f{|aa,(a,b,*)|d}'...'',
    'for a.b in c do e end'...'',
    '(*a)=1'...'',
    '(*a)=b'...'',
    'for a in b do end'...'',
    'for *a in b do end'...'',
    'for (*a) in b do end'...'',
    'for (a,) in b do end'...'',

    'def f(a,b,*); c end'...'',

    ' * =f,g'...'',
    '(*)=f,g'...'',
    '(a,)=b'...'',

      'return(a and b)'...'',
      'break(a and b)'...'',
      'next(a and b)'...'',
      'begin a; rescue b,f: c end'...'',
      'begin a; rescue b,f=>g: c end'...'',

      'begin a;h; rescue b,f: c end'...'',
      'begin a;h; rescue b,f=>g: c end'...'',

      'a ? defined? b : c'...'',
      'if defined? b : c end'...'',
      'if not b : c end'...'',
      'class A::B; end'...'', #ParseTree 2.1.1 gets this wrong... had to quirk it


      '"#{a}#{b}"'...'',
      '"1#{a}2#{b}3"'...'',
      '"1#{a}2#{b}3" "1#{a}2#{b}3"'...'',
      '"1#{a}2#{b}" "1#{a}2#{b}3"'...'',
      '"1#{a}2#{b}3""1#{a}2#{b}3"'...'',
      '"1#{a}2#{b}""1#{a}2#{b}3"'...'',

      '"1#{a}2#{b}3" "#{a}2#{b}3"'...'',
      '"1#{a}2#{b}" "#{a}2#{b}3"'...'',

      '"a#{m}a" "b#{n}b"'...'',
      '"a#{m}a" "bb"'...'',
      '"aa" "b#{n}b"'...'',
      '"aa" "bb"'...'',
      'a=1;"#{a}"'...'',
      '"#{A}"'...'',
      '"#$A"'...'',
      '"#@A"'...'',
      '"#@@A"'...'',

      'p(a and b)'...'',   #not legal

      '1..2'...'',
      'a if 1..2'...'',

      'nc=(nextchar unless eof?)'...'',
      'super'...'',
      'super()'...'',
      'super(1)'...'',
      'super(1,2)'...'',
      'super(1,2,3)'...'',
      'super(*a)'...'',
      'super(1,*b)'...'',
      'super(1,2,*c)'...'',
      'super(&a)'...'',
      'super(1,&b)'...'',
      'super(1,2,&c)'...'',
      'super(*a,&b)'...'',
      'super(1,*b,&c)'...'',
      'super(1,2,*c,&d)'...'',
      'a.super'...'',
      'a.super()'...'',
      'a.super(1)'...'',
      'a.super(1,2)'...'',
      'a.super(1,2,3)'...'',
      'a.super(*a)'...'',
      'a.super(1,*b)'...'',
      'a.super(1,2,*c)'...'',
      'a.super(&a)'...'',
      'a.super(1,&b)'...'',
      'a.super(1,2,&c)'...'',
      'a.super(*a,&b)'...'',
      'a.super(1,*b,&c)'...'',
      'a.super(1,2,*c,&d)'...'',

      'yield'...'',
      'yield()'...'',
      'yield(1)'...'',
      'yield(1,2)'...'',
      'yield(1,2,3)'...'',
      'yield(*a)'...'',
      'yield(1,*b)'...'',
      'yield(1,2,*c)'...'',
      #ruby 1.9
#      'yield(&a)'...'',
#      'yield(1,&b)'...'',
#      'yield(1,2,&c)'...'',
#      'yield(*a,&b)'...'',
#      'yield(1,*b,&c)'...'',
#      'yield(1,2,*c,&d)'...'',
      'a.yield'...'',
      'a.yield()'...'',
      'a.yield(1)'...'',
      'a.yield(1,2)'...'',
      'a.yield(1,2,3)'...'',
      'a.yield(*a)'...'',
      'a.yield(1,*b)'...'',
      'a.yield(1,2,*c)'...'',
      'a.yield(&a)'...'',
      'a.yield(1,&b)'...'',
      'a.yield(1,2,&c)'...'',
      'a.yield(*a,&b)'...'',
      'a.yield(1,*b,&c)'...'',
      'a.yield(1,2,*c,&d)'...'',
      
      'return'...'',
      'return()'...'',
      'return(1)'...'',
      'return 1'...'',
      'break'...'',
      'break()'...'',
      'break(1)'...'',
      'break 1'...'',
      'next'...'',
      'next()'...'',
      'next(1)'...'',
      'next 1'...'',

      #these aren't legal with parens around param list
      'return 1,2'...'',
      'return 1,2,3'...'',
      'return *a '...'',
      'return 1,*b'...'',
      'return 1,2,*c'...'',
      'break 1,2'...'',
      'break 1,2,3'...'',
      'break *a '...'',
      'break 1,*b'...'',
      'break 1,2,*c'...'',
      'next 1,2'...'',
      'next 1,2,3'...'',
      'next *a '...'',
      'next 1,*b'...'',
      'next 1,2,*c'...'',

      'a.return'...'',
      'a.return()'...'',
      'a.return(1)'...'',
      'a.return(1,2)'...'',
      'a.return(1,2,3)'...'',
      'a.return(*a)'...'',
      'a.return(1,*b)'...'',
      'a.return(1,2,*c)'...'',
      'a.return(&a)'...'',
      'a.return(1,&b)'...'',
      'a.return(1,2,&c)'...'',
      'a.return(*a,&b)'...'',
      'a.return(1,*b,&c)'...'',
      'a.return(1,2,*c,&d)'...'',

      'a.break'...'',
      'a.break()'...'',
      'a.break(1)'...'',
      'a.break(1,2)'...'',
      'a.break(1,2,3)'...'',
      'a.break(*a)'...'',
      'a.break(1,*b)'...'',
      'a.break(1,2,*c)'...'',
      'a.break(&a)'...'',
      'a.break(1,&b)'...'',
      'a.break(1,2,&c)'...'',
      'a.break(*a,&b)'...'',
      'a.break(1,*b,&c)'...'',
      'a.break(1,2,*c,&d)'...'',

      'a.next'...'',
      'a.next()'...'',
      'a.next(1)'...'',
      'a.next(1,2)'...'',
      'a.next(1,2,3)'...'',
      'a.next(*a)'...'',
      'a.next(1,*b)'...'',
      'a.next(1,2,*c)'...'',
      'a.next(&a)'...'',
      'a.next(1,&b)'...'',
      'a.next(1,2,&c)'...'',
      'a.next(*a,&b)'...'',
      'a.next(1,*b,&c)'...'',
      'a.next(1,2,*c,&d)'...'',
      '[*a]'...'',
      '[a,*b]'...'',
      '[a,b,*c]'...'',
      'a="0";class<<a; b end'...'',
      'k=Hash;class CornedBeef<k; end'...'',
      'a+b'...'',
      'a +b'...'',
      'a+ b'...'',
      'a + b'...'',
      'a-b'...'',
      'a -b'...'',
      'a- b'...'',
      'a - b'...'',
      'a*b'...'',
      'a *b'...'',
      'a* b'...'',
      'a * b'...'',
      'a&b'...'',
      'a &b'...'',
      'a& b'...'',
      'a & b'...'',

      'A::B'...'',
      'A ::B'...'',
      'A:: B'...'',
      'A :: B'...'',

      'a(&b)'...'',
      '/[}]/'...'',
      '%{\}}'...'',
      '/\}/'...'',
      '/\@/'...'',
      '/\y/'...'',
      '/\ /x'...'',
      '"\}"'...'',
      'i?::R'...'',
      'i? ::R'...'',
      'i? +R'...'',
      'i? -R'...'',
      'i? *R'...'',
      'i? &R'...'',
      '%w[\|\|]'...'',
      '%W[\|\|]'...'',
      '%x[\|\|]'...'',
      '%s[\|\|]'...'',
      '%q[\|\|]'...'',
      '%Q[\|\|]'...'',
      '%r[\|\|]'...'',

      'def evaluate rule;  rescue Exception;    puts "";  end'...'',
      'def evaluate rule; foo; rescue Exception;    puts "";  end'...'',
      'def evaluate rule; foo; rescue Exception=>e;    puts e;  end'...'',
      'def evaluate rule; foo; rescue RX; f(g); rescue Exception=>e;    puts e;  end'...'',
      'def evaluate rule; foo; rescue RX; f(g); rescue Exception=>e;    puts e;  else k end'...'',

      'def evaluate rule; foo; bar;rescue Exception;    puts "";  end'...'',
      'def evaluate rule; foo; bar;rescue Exception=>e;    puts e;  end'...'',
      'def evaluate rule; foo; bar;rescue RX; f(g); rescue Exception=>e;    puts e;  end'...'',
      'def evaluate rule; foo; bar;rescue RX; f(g); rescue Exception=>e;    puts e;  else k end'...'',
      'def a(b,c,d,e=f,g=h,*i,&j) k;l;m rescue n; rescue o,p,q=>r: s; rescue t; u; rescue; v else w ensure x end'...'',
      'begin k;l;m rescue n; rescue o,p,q=>r: s; rescue t; u; rescue; v else w ensure x end'...'',

      '%w[ ]'...'',
      '%W[ ]'...'',
      '%w[\ ]'...'',
      '%W[\ ]'...'',
      '%w[\	]'...'',
      '%W[\	]'...'',
      '%W[\n]'...'',
      '%W[\q]'...'',


      '%W[ #{}]'...'',
      '%W[\ #{}]'...'',
      '%W[\	#{}]'...'',
      '%W[\n#{}]'...'',
      '%W[\q#{}]'...'',

      '%[\"]'...'',
      '%W[\"]'...'',
      '%W[\C-a]'...'',
      '%W[\"#{}]'...'',
      '%W[\C-a#{}]'...'',
      '"\M-\M-\M-\M-\c4"'...'',
      '"\M-\M-\M-\M-\M-4"'...'',
      '"\M-\C-4"' ...'',
      '"\C-\M-4"'...'',
      '"\\C-\\C-\\C-\\c\\C-a"'...'',
      '"\c6"'...'',
      ':"y"'...'',
      ':"y?"'...'',
      ':"y!"'...'',
      ':"y="'...'',
      ':"\y"'...'',
      ':"\\y"'...'',
      ':"\\\\y"'...'',
      ':"y\""'...'',
      '%{\\C-\\\\}'...'',
      '%{\\C-\\}}'...'',
      '%{\\C-\\{}'...'',
      '%{\\C-\\"}'...'',
      '%{\\C-"}'...'',
      '"\\C-\\""'...'',
      ":'\\\\'"...'',
      '%W[\\#]'...'',
      '%W[\\"]'...'',
      '%W[\\\\]'...'',
      '%W[\\##{}]'...'',
      '%W[\\"#{}]'...'',
      '%W[\\\\#{}]'...'',

      '"\\\\M-\\C-?"'...'',
      '"\\C-?"'...'',

      "%w[\\\v]"...'',
      "%W[\\\v]"...'',
      "%W[\\\v#{}]"...'',
      "%w[\v]"...'',
      "%W[\v]"...'',
      "%W[\v#{}]"...'',

      '+?c'...'',
      '+:s'...'',
      '+%s{s}'...'',
      '+/r/'...'',
      '+%r{r}'...'',
      '+:"s"'...'',
      "+:'s'"...'',

      '+/r#{1}/'...'',
      '+%r{r#{1}}'...'',
      '+:"s#{1}"'...'',

      '+ ?c'...'',
      '+ :s'...'',
      '+ %s{s}'...'',
      '+ /r/'...'',
      '+ %r{r}'...'',
      '+ :"s"'...'',
      "+ :'s'"...'',

      '+ /r#{1}/'...'',
      '+ %r{r#{1}}'...'',
      '+ :"s#{1}"'...'',

      '+1'...'',
      '+ 1'...'',
      'def a; 0..0 end'...'',

      '0.0;a'...'',
      '0..0;a'...'',
      '0...0;a'...'',

      '+++++++++++1'...'',
      '+++++++++++-1'...'',
      '++++++-+++++1'...'',
      '-+++++++++++1'...'',
      '--1'...'',
      '- 1'...'',
      '"\C-""'...'',
      '+/reg#{exp}/'...'',
      '?\C-\?'...'',
      '?\C-?'...'',
      'a=b rescue c'...'',
      'a=b=d rescue c'...'',
      '(a=b) rescue c'...'',
      'a.d=b rescue c'...'',
      'a[d]=b rescue c'...'',     
#      'A::D=b rescue c'...'', #segfaults ParseTree 2.1.1
      'a,=b rescue c'...'',
      'a,* =b rescue c'...'',
      'a,d=b rescue c'...'',
      'a,*d=b rescue c'...'',
      '(a,)=b rescue c'...'',
      '(a,*)=b rescue c'...'',
      '(a,d)=b rescue c'...'',
      '(a,*d)=b rescue c'...'',

      '*a=b rescue c'...'',
      'a=*b rescue c'...'',
      'a=b,d rescue c'...'',
      'a+=b rescue c'...'',
      '"#{__FILE__}"'...'',
      '"#{__LINE__}"'...'',
      '"#{nil}"'...'',
      '"#{true}"'...'',
      '"#{false}"'...'',
      '"#{self}"'...'',
      '"#{()}"'...'',
      '"sdfe#{g5}4t#{(((((__FILE__)))))}dfsd#{g}"'...'',
      '"#{"foo"}"'...'',
      '"#{("foo")}"'...'',
      '"#{"foo"}bar#{baz}"'...'',

      'begin; a; rescue b; end; c'...'',
      'if begin; a; rescue b; end then c end'...'',
      'while begin; a; rescue b; end ;  end'...'',
      'begin; m; rescue begin; a; rescue b; end:  end'...'',
      'begin; m; rescue begin; a; rescue b; end,o:  end'...'',
      'def l; m; rescue begin; a; rescue b; end:  end'...'',
      'def l; m; rescue begin; a; rescue b; end,o:  end'...'',
      'for i in begin; a; rescue b; end do gf  end'...'',
      'def m n=begin; a; rescue b; end; end'...'',
      'def (begin; a; rescue b; end).m; end'...'',
      'def ((begin; a; rescue b; end)).m; end'...'',
      'begin; a; rescue b; end.m'...'',
      '(begin; a; rescue b; end).m'...'',
      'c=begin; a; rescue b; end'...'',
      'c+=begin; a; rescue b; end'...'',
      'p begin; a; rescue b; end'...'',
      'p(begin; a; rescue b; end)'...'',
      'c=begin; a; rescue b; end,0'...'',
      'c=0,begin; a; rescue b; end'...'',
      'c=0,begin; a; rescue b; end,0'...'',
      '[begin; a; rescue b; end]'...'',
      '{begin; a; rescue b; end=>m}'...'',
      '{m=>begin; a; rescue b; end}'...'',
      'begin; a; rescue b; end+m'...'',
      'm+begin; a; rescue b; end'...'',
      '+begin; a; rescue b; end'...'',
      '-begin; a; rescue b; end'...'',
      '~begin; a; rescue b; end'...'',
      'p(*begin; a; rescue b; end)'...'',
      'p(&begin; a; rescue b; end)'...'',
      'begin; a; rescue b; end ? m : n'...'',
      'm rescue begin; a; rescue b; end'...'',
      'm > begin; a; rescue b; end'...'',
      'm < begin; a; rescue b; end'...'',
      'm >= begin; a; rescue b; end'...'',
      'm <= begin; a; rescue b; end'...'',
      'm <=> begin; a; rescue b; end'...'',
      'm === begin; a; rescue b; end'...'',
      'm == begin; a; rescue b; end'...'',
      'm = begin; a; rescue b; end'...'',
      'm % begin; a; rescue b; end'...'',
      'm %= begin; a; rescue b; end'...'',
      'm >> begin; a; rescue b; end'...'',
      'm << begin; a; rescue b; end'...'',
      'm >>= begin; a; rescue b; end'...'',
      'm <<= begin; a; rescue b; end'...'',
      'm **= begin; a; rescue b; end'...'',
      'm *= begin; a; rescue b; end'...'',
      'm /= begin; a; rescue b; end'...'',
      'm -= begin; a; rescue b; end'...'',
      'm += begin; a; rescue b; end'...'',
      'm ^= begin; a; rescue b; end'...'',
      'm &= begin; a; rescue b; end'...'',
      'm |= begin; a; rescue b; end'...'',
      'm &&= begin; a; rescue b; end'...'',
      'm ||= begin; a; rescue b; end'...'',
      'm * begin; a; rescue b; end'...'',
      'm ** begin; a; rescue b; end'...'',
      'm / begin; a; rescue b; end'...'',
      'm - begin; a; rescue b; end'...'',
      'm + begin; a; rescue b; end'...'',
      'm ^ begin; a; rescue b; end'...'',
      'm & begin; a; rescue b; end'...'',
      'm | begin; a; rescue b; end'...'',

      'm ; begin; a; rescue b; end'...'',
      'begin; a; rescue b; end ; m'...'',
      'begin; a; rescue b; end if m'...'',
      'begin; a; rescue b; end while m'...'',
      'begin begin; a; rescue b; end end'...'',
      'begin; a; rescue b; end[m]'...'',
      'begin; a; rescue b; end[m]=n'...'',
      'begin; a; rescue b; end.each'...'',


      'm ? begin; a; rescue b; end : nil'...'',
      'm ? nil : begin; a; rescue b; end'...'',
      'm && begin; a; rescue b; end'...'',
      'm and begin; a; rescue b; end'...'',
      'm or begin; a; rescue b; end'...'',
      'm || begin; a; rescue b; end'...'',

      'm ? !begin; a; rescue b; end : nil'...'',
      'm ? nil : !begin; a; rescue b; end'...'',
      'm && !begin; a; rescue b; end'...'',
      'm and !begin; a; rescue b; end'...'',
      'm or !begin; a; rescue b; end'...'',
      'm || !begin; a; rescue b; end'...'',

      'm ? (begin; a; rescue b; end) : nil'...'',
      'm ? nil : (begin; a; rescue b; end)'...'',
      'm && (begin; a; rescue b; end)'...'',
      'm and (begin; a; rescue b; end)'...'',
      'm or (begin; a; rescue b; end)'...'',
      'm || (begin; a; rescue b; end)'...'',

      'begin; a; rescue b; end and m'...'',
      'begin; a; rescue b; end && m'...'',
      'begin; a; rescue b; end or m'...'',
      'begin; a; rescue b; end || m'...'',
      'not begin; a; rescue b; end'...'',
      '!begin; a; rescue b; end'...'',
      'if m; n; elsif begin; a; rescue b; end then o end'...'',
      'begin; a; rescue b; end if m'...'',
      'm if begin; a; rescue b; end'...'',
      'm unless begin; a; rescue b; end'...'',
      'm while begin; a; rescue b; end'...'',
      'm until begin; a; rescue b; end'...'',
      'case m; when begin; a; rescue b; end; j; when n; o end'...'',
      'case m; when begin; a; rescue b; end,k; j; when n; o end'...'',
      'case m; when k,begin; a; rescue b; end; j; when n; o end'...'',
      'case m; when k,begin; a; rescue b; end,k; j; when n; o end'...'',
      '"#{begin; a; rescue b; end}"'...'',
      '"foo#{(__FILE__)}quux#{"biff"}" "baz"'...'',
      '"foo#{("bar")}quux#{"biff"}" "baz"'...'',
      '"foo#{"bar"}quux#{"biff"}" "baz"'...'',
      '"foo#{bar}quux#{"biff"}" "baz"'...'',
      '"foo#{bar}quux#{biff}" "baz"'...'',
      '"foo#{bar}quux" "baz"'...'',
      '"foo#{"bar"}" "baz"'...'',
      '"foo" "bar#{}" "baz"'...'',
      'p(&a.b)'...'',
      'p(*a+b)'...'',
      'm &begin; a; rescue b; end'...'',
      'm *begin; a; rescue b; end'...'',
      'm(&begin; a; rescue b; end)'...'',
      'm(*begin; a; rescue b; end)'...'',
      'm=*begin; a; rescue b; end'...'',
      'p begin; a; rescue b; end=>d'...'',
      'p d=>begin; a; rescue b; end'...'',
      'p d=>b'...'',
      'p d=>b,*h'...'',
      'p a,b,d=>b,*h,&g'...'',
      'p d=>b,*h,&g'...'',
      'p d=>b,&g'...'',
      'p(a,b,d=>b,*h){g}'...'',
      'p(d=>b,*h){g}'...'',
      'p(d=>b){g}'...'',
      'p a,b,d=>b,*h do g end'...'',
      'p d=>b,*h do g end'...'',
      'p d=>b do g end'...'',
      '"#{""}"'...'',
      '%[a#{"b"}c#{__FILE__}e#{"f"}]'...'',
      '/a#{"b"}c#{__FILE__}e#{"f"}/'...'',
      '/a#{"b"}c#{__FILE__}e#{"f"}/o'...'',
      '`a#{"b"}c#{__FILE__}e#{"f"}`'...'',
      '%W[a#{"b"}c#{__FILE__}e#{"f"}]'...'',
      ':"s#{1}"'...'',
      'while a; end'...'',
      'while a: end'...'',
      'while a do end'...'',
      '%W[a#{"b"}c]'...'',
      '%W[a#{"b"}]'...'',
      '%W[#{"b"}c]'...'',
      '%W[a#{" b  "}]'...'',
      'a[b=>c]'...'',
      'a[b=>c,*d]'...'',
      'a[*b]'...'',
      'a[b=>c]=d'...'',
      'a[b=>c,*d]=e'...'',
      'a[*b]=c'...'',

      'a[z,b=>c]'...'',
      'a[z,b=>c,*d]'...'',
      'a[z,*b]'...'',
      'a[z,b=>c]=d'...'',
      'a[z,b=>c,*d]=e'...'',
      'a[z,*b]=c'...'',

      'a[y,z,b=>c]'...'',
      'a[y,z,b=>c,*d]'...'',
      'a[y,z,*b]'...'',
      'a[y,z,b=>c]=d'...'',
      'a[y,z,b=>c,*d]=e'...'',
      'a[y,z,*b]=c'...'',


      'a[b=>c]+=d'...'',
      'a[b=>c,*d]+=e'...'',
      'a[*b]+=c'...'',
      'a[z,*b]+=c'...'',
      'a[y,z,*b]+=c'...'',

      'a[b=>c]||=d'...'',
      'a[b=>c,*d]||=e'...'',
      'a[*b]||=c'...'',
      'a[z,*b]||=c'...'',
      'a[y,z,*b]||=c'...'',

      'a[b=>c],z=d'...'',
      'a[b=>c,*d],z=e'...'',
      'a[*b],z=c'...'',
      'a[z,*b],z=c'...'',
      'a[y,z,*b],z=c'...'',

      'a[b=>c],z=d,y'...'',
      'a[b=>c,*d],z=e,y'...'',
      'a[*b],z=c,y'...'',
      'a[z,*b],z=c,y'...'',
      'a[y,z,*b],z=c,y'...'',

      '*a[b=>c]=d'...'',
      '*a[b=>c,*d]=e'...'',
      '*a[*b]=c'...'',
      '*a[z,*b]=c'...'',
      '*a[y,z,*b]=c'...'',


      'm,*a[b=>c]=d'...'',
      'm,*a[b=>c,*d]=e'...'',
      'm,*a[*b]=c'...'',
      'm,*a[z,*b]=c'...'',
      'm,*a[y,z,*b]=c'...'',

      'm,(a[b=>c],)=d'...'',
      'm,(a[b=>c,*d],)=e'...'',
      'm,(a[*b],)=c'...'',
      'm,(a[z,*b],)=c'...'',
      'm,(a[y,z,*b],)=c'...'',

      'for a[b=>c] in d do end'...'',
      'for a[b=>c,*d] in e do end'...'',
      'for a[*b] in c do end'...'',
      'for a[z,*b] in c do end'...'',
      'for a[y,z,*b] in c do end'...'',

      'm{|a[b=>c]| d}'...'',
      'm{|a[b=>c,*d]| e}'...'',
      'm{|a[*b]| c}'...'',
      'm{|a[z,*b]| c}'...'',
      'm{|a[y,z,*b]| c}'...'',


      'm{|(a[b=>c],)| d}'...'',
      'm{|(a[b=>c,*d,)]| e}'...'',
      'm{|(a[*b],)| c}'...'',
      'm{|(a[z,*b],)| c}'...'',
      'm{|(a[y,z,*b],)| c}'...'',





      '[b=>c]'...'',
      '[b=>c,d=>e]'...'',
      '[*b]'...'',

      '[z,*b]'...'',

      '[y,z,*b]'...'',
      'def a; l=if b; else; end; super *l; end'...'',
      'def a; super *c; end'...'',
      'def a; super b,*c; end'...'',
      'super *c'...'',
      'super b,*c'...'',
      'yield *c'...'',
      'yield b,*c'...'',
      'break *c'...'',
      'break b,*c'...'',
      'next *c'...'',
      'next b,*c'...'',
      ' T.c[:u]=w'...'',
      'next a,b,*c do d end'...'',
      'yield a,b,*c do d end'...'',
      'break a,b,*c do d end'...'',
      'super a,b,*c do d end'...'',
      'return a,b,*c do d end'...'',
      'm{|(a[b],t)| c }'...'',
      'm{|(a[b],)| c }'...'',
      'm{|(a[],)| c }'...'',
      'z[begin; a; rescue b; end]'...'',
      'z[begin; a; rescue b; end]=c'...'',
      'z[begin; a; rescue b; end]=*c'...'',
      '*z[begin; a; rescue b; end]=*c'...'',
      '(z[begin; a; rescue b; end],)=c'...'',
      '(z[begin; a; rescue b; end],*)=c'...'',
      '(z[begin; a; rescue b; end],*f),g=c'...'',

      'y{|z[begin; a; rescue b; end]|c}'...'',
      '(size-1).downto(0){|i| expr=self[i]}'...'',
      'self[]'...'',
      'self[a]'...'',
      'self[]=b'...'',
      'self[a]=b'...'',
      'm{|(a,)| }'...'',
      'm{|(a,b)| }'...'',
      'for (a,) in b do c end'...'',
      'm{|a,| }'...'',
      'for a, in b do c end'...'',
      'm{|((a,),),j| }'...'',
      'm{|((a,)),j| }'...'',
      'm{|(*h)| }'...'',
      'def (@@foo=bar).baz; quux end'...'',
      'def a; @@b=c; end'...'',
      'def a.b; @@c=d; end'...'',

      'def a(b=(@@foo=bar)) end'...'',
      'def a(b=@@foo=bar) end'...'',
      'def a b=@@foo=bar; end'...'',
      'a[-b]'...'',
      '//!~a'...'',
      '%w(1067595299  955945823  477289528 4107218783 4228976476)'...'',

      'break if /vt100/ =~ line'...'',
      'b if /vt100/ =~ line'...'',
      '/vt100/ =~ line and f'...'',
      '/vt100/ =~ line'...'',
      'BEGIN{a=1};a'...'',
      'proc{ BEGIN{a=1};a }'...'',
      'END{ BEGIN{a=1};a }'...'',
      'proc{ proc{ BEGIN{a=1};a } }'...'',
      'END{a=1};a'...'',
      'proc{ END{a=1};a }'...'',
      'BEGIN{ END{a=1};a }'...'',
      'proc{ proc{ END{a=1};a } }'...'',
      'def mo4(a, *b, &c) end'...'',
      'assert_equal "\177\377\377\377", [-2**31-1].pack("N")'...'',
      'def tt3(&b) tt2(&b) end'...'',
      'def tt3(&b) end'...'',
      'def self.defin(foo) end'...'',
      'def self.defin?(foo) end'...'',
      'def self.defined?(foo) end'...'',
      'def x.defin(foo) end'...'',
      'def x.defin?(foo) end'...'',
      'def x.defined?(foo) end'...'',
      'def defined?(foo) end'...'',

      'def self.defin(foo) hh end'...'',
      'def self.defin?(foo) hh end'...'',
      'def self.defined?(foo) hh end'...'',
      'def x.defin(foo) hh end'...'',
      'def x.defin?(foo) hh end'...'',
      'def x.defined?(foo) hh end'...'',
      'def defined?(foo) hh end'...'',

      'def self.defin(foo) nil end'...'',
      'def self.defin?(foo) nil end'...'',
      'def self.defined?(foo) nil end'...'',
      'def x.defin(foo) nil end'...'',
      'def x.defin?(foo) nil end'...'',
      'def x.defined?(foo) nil end'...'',
      'def defined?(foo) nil end'...'',

      'def self.defin(foo) () end'...'',
      'def self.defin?(foo) () end'...'',
      'def self.defined?(foo) () end'...'',
      'def x.defin(foo) () end'...'',
      'def x.defin?(foo) () end'...'',
      'def x.defined?(foo) () end'...'',
      'def defined?(foo) () end'...'',

      'def self.defin(foo) {} end'...'',
      'def self.defin?(foo) {} end'...'',
      'def self.defined?(foo) {} end'...'',
      'def x.defin(foo) {} end'...'',
      'def x.defin?(foo) {} end'...'',
      'def x.defined?(foo) {} end'...'',
      'def defined?(foo) {} end'...'',

      'def self.defin(foo) [] end'...'',
      'def self.defin?(foo) [] end'...'',
      'def self.defined?(foo) [] end'...'',
      'def x.defin(foo) [] end'...'',
      'def x.defin?(foo) [] end'...'',
      'def x.defined?(foo) [] end'...'',
      'def defined?(foo) [] end'...'',

      "$`"...'',
      "$'"...'',
      "$+"...'',
      "case\n when true; 4 end"...'',
      "case when true; 4 end"...'',
      '$2'...'',
      '$11'...'',
      '$111'...'',
      '$111111'...'',
      '$11111111111111111111111111111111111111111111111111111111111111111111'...'',
      '-2::c'...'',
      '-2.c'...'',
      '-2[3]'...'',
      '-2[3]=4'...'',
      '-2**3.1'...'',
      '-2**31'...'',
      '-2.7**31'...'',
      '-2e7**31'...'',
      '-2.7e8**31'...'',
      '-0**31'...'',
      '-0.0**31'...'',
      '-0e0**31'...'',
      '-0.0e0**31'...'',
      '$-1'...'',
      '$-9'...'',
      '$-0'...'',
      '$-a'...'',
      "$-\v"...'',
      '$-[]'...'',
      'a,b=(*c=b,a)'...'',
      
    ]
    NOTWORKINGYET=[
      #later...
    ]


   PASSTHRU_BSLASHES_ENTIRE=<<'END'
     <<-'foo'.count('\\')==0
       '
     foo

     <<-'foo'.count('\\')==1
       \'
     foo

      <<-'foo'.count('\\')==2
      \\'
      foo

      <<-'foo'.count('\\')==3
      \\\'
      foo

      <<-'foo'.count('\\')==4
      \\\\'
      foo

      <<-'foo'.count('\\')==5
      \\\\\'
      foo

      <<-"foo".count('\\')==0
      "
      foo

      <<-"foo".count('\\')==0
      \"
      foo

      <<-"foo".count('\\')==1
      \\"
      foo

      <<-"foo".count('\\')==1
      \\\"
      foo

      <<-"foo".count('\\')==2
      \\\\"
      foo

      <<-"foo".count('\\')==2
      \\\\\"
      foo

     <<-`foo`
       \`
     foo

     <<-`foo`
       \\`
     foo

     <<-"foo"
       \"
     foo

     <<-"foo"
       "
     foo

     <<-"foo"
       \\"
     foo

END

  STANZAS=PASSTHRU_BSLASHES_ENTIRE+%q[
module 
=begin =end
=end
 A; end

module A
=begin =end
=end
 ::B; end

module A::
=begin =end
=end
 B; end

=begin =end
=end

    return @senders[1] =
      2

    case
    when 0
      guecoding     
    else case
      when eucjp_match_length 
        guing
      end
    end

     %w[ ac
         df]

     begin 
       a
     rescue B=>c
       d
     else
       f
     ensure
       e
     end

     begin 
       a
     else
       f
     ensure
       e
     end

     begin 
       a
     rescue B=>c
       d
     else
       f
     end

     begin 
       a
     rescue B=>c
       d
     ensure
       e
     end

     begin 
       a
     else
       f
     end

     begin 
       a
     rescue B=>c
       d
     end

     begin 
       a
     rescue B=>c
       d
     else
       f
     end

     begin 
       a
     rescue B
       c
     end

     begin 
       a
     rescue B=>c
     end

     begin 
       a
     rescue
       b
     end

     <<heredoc
       a b c 
heredoc

     <<-heredoc
     heredoc

     <<-heredoc
       a b c 
     heredoc

     <<-"heredoc"
       a b c 
     heredoc

     <<-"heredoc"
       a b c #{d}
     heredoc

     <<-"heredoz"
       a b c #{d}
     heredoz

     <<-'heredoc'
       a b c 
     heredoc

     <<-'heredoc'
       a b c #{d}
     heredoc

     <<-`heredoc`
       a b c 
     heredoc

     <<`heredoc`
       a b c 
heredoc

     <<-`heredoc`
       a b c #{d}
     heredoc

     <<`heredoc`
       a b c #{d}
heredoc
     
     p(<<-heredoc)
       a b c 
     heredoc

     p <<-heredoc
       a b c 
     heredoc

     p <<-heredoc + "dfgsf"
       a b c 
     heredoc

     p <<-heredoc + "sdfsdF" and 5
       a b c 
     heredoc
   
     p <<-heredoc "x y z" and 5
       a b c
     heredoc

=begin
=end

=begin a
b
=end c

  if a 
  then b
  else c
  end

  if a;
  then b
  else c
  end

  if a; then b
  else c
  end

  if a
  then b else c
  end

  if a
  else c
  end

  if a; else c
  end

  if
  a
  then
  b
  end

  if
  a
  then
  b
  else
  c
  end

  <<-foo+<<-bar
    a b c
  foo
    d e f
  bar

  <<-foo+'123
  abc
  foo
  456'

  case a
  when b
  then c
  end


  case 
  a
  when 
  b
  then 
  c
  end

  case  
  a;
  when  
  b;
  then  
  c;
  end

  result=[
    MethNameToken.new(old.ident,old.offset),
    ImplicitParamListStartToken.new(input_position),
    ImplicitParamListEndToken.new(input_position),
    *ignored_tokens
   ]

   %r{^(
                class|module|end|self|true|false|nil|def|  
                __FILE__|__LINE__|(\\})\\)
              )$}x

   /sdf
    sdf
          sdfsdf
     sdfsf/

   /sdf
    sdf\\
          sdfsdf
     sdfsf/

  def evaluate rule,stack
    #dissect the rule
  rescue Exception  
    puts "error while executing rule: #{}"
  end

  def self.has_return_hash_fix?
    rl=RubyLexer.new("","return {}.size")
  end

  %[\\
]

  %q[\\
]

  %Q[\\
]

  %r[\\
]

  %s[\\
]

  %x[\\
]

  %w[\\
]

  %w[
]

  /\\
/

  %r[\\
]

  %W[\\
]

  %W[a\\
]

  %W[a b\\
]

  %W[a b \\
]

  %W[
]

"\\C-\\
"

:"
"

p "#{__FILE__}"
p "#{__LINE__}"

     a b, c
__END__
     d e, f

     a b, c
=begin x
y
=end z
__END__
     d e, f

    a{<<-f;b}
     #{b=1}
    f

    a{<<-f;b}+
     #{b=1}
    f
    c

    a{<<-f;b}\\
     #{b=1}
    f
    +c

    funcnames.collect{|fn| <<-endeval; hn }.to_s(fn,hn,gn=1)
      #{fn} #{gn} #{hn=2}
    endeval
    p fn,gn,hn

    <<-foo+"str"
    foo

BEGIN {
  puts "b1"
  local_begin1 = "local_begin1"
  $global_begin1 = "global_begin1"
  ConstBegin1 = "ConstBegin1"
}
BEGIN {
  puts "b2"
  BEGIN {
    puts "b2-1"
    local_begin2=33
  }
}
# for scope check
raise if defined?(local_begin1)
raise if defined?(local_begin2)
raise unless defined?($global_begin1)
raise unless defined?(::ConstBegin1)
local_for_end2 = "e2"
$global_for_end1 = "e1"

END {
  puts "b1"
  local_begin1 = "local_begin1"
  $global_begin1 = "global_begin1"
  ConstBegin1 = "ConstBegin1"
}
END {
  puts "b2"
  END {
    puts "b2-1"
    local_begin2=33
  }
}
# for scope check
raise if defined?(local_begin1)
raise if defined?(local_begin2)
raise unless defined?($global_begin1)
raise unless defined?(::ConstBegin1)
local_for_end2 = "e2"
$global_for_end1 = "e1"

    events=[]
    set_trace_func(Proc.new { |event, file, lineno, mid, bidning, klass|
      events << [event, lineno, mid, klass]
    })
    a = 1
    foo
    a
    b = 1 + 2
    if b == 3
      case b
      when 2
        c = "b == 2"
      when 3
        c = "b == 3"
      end
    end

proc{q=1;def q.foo; end}  #q should be varnametoken, both times
module Defined_p_syntax_tests
  def self.defined?(foo) :baz end  #should be methname
  def defined?(foo) :bar end  #should be methname
  def ameth
    p(defined? 44)  #should be keyword
    p(self.defined? 44) #should be methname
  end
end

    def x# [ruby-dev:24228]
    assert_nothing_raised {
      def temporally_method_for_test_eval_and_define_method(&block)
        lambda {
          class << Object.new; self end.__send__(:define_method, :zzz, &block)
        }
      end
      v = eval("temporally_method_for_test_eval_and_define_method {}")
      {}[0] = {}
      v.call
    }
    end

  def tt3(&block)
    tt2(raise(ArgumentError,""),&block)
  end


  def test_char
    # character constants(assumes ASCII)
    assert_equal(?a, "a"[0])
    assert_equal(?a, ?a)
    assert_equal(1, ?\\C-a)
    assert_equal(225, ?\\M-a)
    assert_equal(129, ?\\M-\\C-a)
    assert_equal(?A, "a".upcase![0])
    assert_equal(?a, "A".downcase![0])
    assert_equal("ABC", "abc".tr!("a-z", "A-Z"))
    assert_equal("ABC", "aabbcccc".tr_s!("a-z", "A-Z"))
    assert_equal("abc", "abcc".squeeze!("a-z"))
    assert_equal("ad", "abcd".delete!("bc"))
    $x = "abcdef"
    $y = [ ?a, ?b, ?c, ?d, ?e, ?f ]
    $bad = false
    $x.each_byte {|i|
      if i != $y.shift
        $bad = true
        break
      end
    }
    assert(!$bad)
    s = "a string"
    s[0..s.size]="another string"
    assert_equal("another string", s)
    s = <<EOS
#{
[1,2,3].join(",")
}
EOS
    assert_equal("1,2,3\\n", s)
    assert_equal(926381, "Just".to_i(36))
    assert_equal(-23200231779, "-another".to_i(36))
    assert_equal("ruby", 1299022.to_s(36))
    assert_equal("-hacker", -1045307475.to_s(36))
    assert_equal(265419172580680477752431643787347, "Just_another_Ruby_hacker".to_i(36))
    assert_equal("-justanotherrubyhacker", -265419172580680477752431643787347.to_s(36))
    a = []
    (0..255).each {|n|
      ch = [n].pack("C")
      a.push ch if /a#{Regexp.quote ch}b/x =~ "ab"
    }
    assert_equal(0, a.size)
  end

    /:(\\d+)/ =~ caller[0]
    file = $`
    line = $1.to_i
    code = <<"End"
    define_method("test_id2ref_#{line}") {\\
      o = ObjectSpace._id2ref(obj.object_id);\\
      assert_same(obj, o, "didn't round trip: \\#{obj.inspect}");\\
    }
End

    assert(begin
         for k,v in y
           raise if k*2 != v
         end
         true
       rescue
         false
       end)

    case
    when true
      assert(true)
    when false, nil
      assert(false)
    else
      assert(false)
    end

  def test_endblockwarn
    ruby = EnvUtil.rubybin
    # Use Tempfile to create temporary file path.
    launcher = Tempfile.new(self.class.name)
    errout = Tempfile.new(self.class.name)
    launcher << <<EOF
errout = ARGV.shift
STDERR.reopen(File.open(errout, "w"))
STDERR.sync = true
Dir.chdir(#{q(DIR)})
cmd = "\\"#{ruby}\\" \\"endblockwarn.rb\\""
system(cmd)
EOF
    launcher.close
    launcherpath = launcher.path
    errout.close
    erroutpath = errout.path
    system("#{q(ruby)} #{q(launcherpath)} #{q(erroutpath)}")
    expected = <<EOW
endblockwarn.rb:2: warning: END in method; use at_exit
(eval):2: warning: END in method; use at_exit
EOW
    assert_equal(expected, File.read(erroutpath))
    # expecting Tempfile to unlink launcher and errout file.
  end

module TYDFG  
  p a ;
end

p <<ggg; def
kleegarts() p 'kkkkkkk' end
dfgdgfdf
ggg
koomblatz!() p 'jdkfsk' end

class
A; end

module
A; end

class
A<B; end

undef 
A

undef 
A,B

def 
A; b end

alias
q
p

alias
q p

p <<'p'
\\n\\t\\r\\v\\\\
p

 p 1==2
 p 1===2
 p 1[2]  #keyword
 p 1;2  #keyword
 p 1,2  #keyword
 p 1.2

before_time = Time.now
ARGV.each do |fname|
  if fname == '-v'
    verbose = true
    next
  end
  rss = nil
  f = File.new(fname).read
  begin
    ## do validate parse
    rss = RSS::Parser.parse(f)
  rescue RSS::InvalidRSSError
    error($!) if verbose
    ## do non validate parse for invalid RSS 1.0
    begin
      rss = RSS::Parser.parse(f, false)
    rescue RSS::Error
      ## invalid RSS.
      error($!) if verbose
    end
  rescue RSS::Error
    error($!) if verbose
  end
end

      begin
        lcode = IO::readlines( @xGAsaFile.untaint ).untaint
        code = lcode.join( " " )
        @oAspHandler.load_code( code )
      rescue => e
        if Asp::Common::debug == 1 
          result = @oApacheRequest.escape_html(e.message)
          @oApacheRequest.server.log_error( "[Ruby/ASP] [%d] : (%s #%d) : %s", $$, __FILE__, __LINE__, result )
        end
        return false
      end

    begin
          xOutCode = @oXmlScript.script( xCode ) 
    rescue XmlScriptError => e
          @error_line = e.line
          @error_message = e.message
    end

        location_display = if(location.size == 1)
          location[0].sub(/\\A(.+:\\d+).*/, ' [\\1]')
        else
          "\\n#{location.join("\\n")}"
        end
        "Failure:\\n#@test_name#{location_display}:\\n#@message"

  def parse_date(aString)
    return Time.rfc822(aString) rescue Time.parse(aString)
  end

%w(
    CVS SCCS)

        raise AttributeConstraintError(
        )

        raise AttributeConstraintError( #and a comment here
        )

        raise AttributeConstraintError( #and a comment here
     #and here
        )

        raise AttributeConstraintError( #and a comment here
     #and here
=begin
=end
        )

        raise AttributeConstraintError( #and a comment here
     #and here
=begin
and here too
=end
        )

        raise AttributeConstraintError( #and a comment here
     #and here
=begin look!
and here too
=end
        )

        raise AttributeConstraintError( #and a comment here
     #and here
=begin
and here too
=end look!
        )

        raise AttributeConstraintError( #and a comment here
     #and here
=begin look!
and here too
=end look!
        )

l rescue
r

  undef new_master, new_slave, new_safe_slave
  undef new_trusted_slave, new_safeTk

"\\xa5\\xaa\\xa5\\xd6\\xa5\\xb8\\xa5\\xa7\\xa5\\xaf\\xa5\\xc8\\xbb\\xd8\\xb8\\xfe\\
\\xa5\\xb9\\xa5\\xaf\\xa5\\xea\\xa5\\xd7\\xa5\\xc8\\xb8\\xc0\\xb8\\xec\\
Ruby"

"f"
d

begin; rescue A=>b.c
end

%W[#{G}
 Z]

<<-EOS<<__LINE__
EOS

  ] ##############################END OF STANZAS#################################################
  ###############################################################################################


  SINGLE_OPEN2CLOSE={
    "'"=>"'",
    ":'"=>"'",
    '%q['=>']',
    '%s['=>']',
    '%w['=>']',
  }
  DOUBLE_OPEN2CLOSE={
    '"'=>'"',
    ':"'=>'"',
    '`'=>'`',
    '/'=>'/',
    '%['=>']',
    '%Q['=>']',
    '%r['=>']',
    '%x['=>']',
    '%W['=>']',
    ''=>'',
  }
  DOUBLE_CHAR_DECORATORS=['\c','\C-','\M-']
  SINGLE_CHAR_DECORATORS=['\\','']
  CHAR_DECORATORS=SINGLE_CHAR_DECORATORS+DOUBLE_CHAR_DECORATORS
  ESC_SEQS=[]
  (0..0x7F).each{|ch| ch=ch.chr
    SINGLE_OPEN2CLOSE.each_pair{|open,close|
      #next if "\0"==ch and /^(:|%s)/===open
      SINGLE_CHAR_DECORATORS.each{|dec| 
        ESC_SEQS << open+dec+ch+close #unless dec=='' and (open+close)[ch]
      }
    }
    DOUBLE_OPEN2CLOSE.each_pair{|open,close|
      #next if /[xcCM0-7]/===ch
      #next if "\0"==ch and /^:/===open
      CHAR_DECORATORS.each{|dec| 
        #next if dec=='' and (open+close)[ch]
        ESC_SEQS << open+dec+"\\"+ch+close
        ESC_SEQS << open+dec+ch+close #unless %r{^(/|%r)}===open
      }
      num=CHAR_DECORATORS.size
      ESC_SEQS << open+(0...rand(num)).inject(ch){|sum,i|
        CHAR_DECORATORS[rand(num)]+sum
      }+close
    }
  } unless ENV['FAST']

  from_rubinius=[]
  if File.exist? "../rubinius"
    load "../rubinius/spec/parser/fixtures/sexp_expectations.rb"
    from_rubinius=SEXP_EXPECTATIONS.values.map{|v| v["Ruby"] }
  end

  EXAMPLES=ONELINERS.map{|rng| rng.first}+STANZAS.split(/\n( *\n)+/)+TestCases::TESTCASES+
           from_rubinius + #ESC_SEQS +
           []-ERROR_EXAMPLES-FAILURE_EXAMPLES-RUBYBUG_EXAMPLES


  WRAPPERS=[ #enable at most 2 or tests take forever!!!
    '(...)',  #normal mode, should usually be enabled
#    'a rescue (...)',
#    "((...))",
#    'def ((...)).foo; end',
#    'a0 = (...) rescue b0',
#    'a0 = ((...)) rescue b0',    
#    '(...)  #with a comment',
#    "(...)#with comment and newline\n",
#    "(...)\n",
#    "(...);p __LINE__",
#    "defined? (...)",
#    "a=a (...)",
#    "b=1;b (...)",
#    "return (...)"
  ]
  INJECTABLES=[  #take it easy with these too
#    'p (1..10).method(:each)',
#    'a0 rescue b0',
#    'begin; r; t end',
#    'a=b,c=d',
  ]
  puts "warning: most data fuzzing is disabled for now"

  RUBYIDENT=/((?:$|@@?)?#{RubyLexer::LETTER}#{RubyLexer::LETTER_DIGIT}*[?!]?)/o

  def self.snippet2testmethod(snippet,wrap=nil)
    escaped=snippet.gsub(/[\\']/){"\\"+$&}
    safe=escaped.gsub(/([^ -~])/){
      x=$1[0].to_s(16)
      x.size==1 and x="0"+x
      "\\x"+x
    }
    safe.gsub! /\\\\/,"__"
    safe[/[^ -~]|\\\\/] and fail
    cp="check_parsing '#{escaped}',pt"
    cp="#{wrap}{#{cp}}" if wrap
    "
      define_method 'test_parsing_of_#{safe}' do
        #puts 'test_parsing_of_#{safe}'
        pt=ParseTree.new
        #{cp}
      end
    "
  end

  i=0
  code=EXAMPLES.map{|xmpl| 
    #remove comment if present
    /\A(.*)\s*\Z/===xmpl and xmpl=$1
    #remove x{...} wrapper if present
    /\A\s*x\s*\{(.*)\}\Z/===xmpl and xmpl=$1
#    xmpl= (xmpl[/\A\s*x\s*\{(.*)\}\s*(#.*)?\Z/,1] rescue xmpl)

    wrapped=WRAPPERS.map{|wrap|
       #apply wrapper
      maybe_slow="slow" if xmpl.size>1000
      wrap=wrap.dup
      wrap['(...)']=xmpl
      snippet2testmethod(wrap,maybe_slow)
    }
    injected=INJECTABLES.map{|inj|
      xlist=xmpl.split(RUBYIDENT)
      if xlist.size>1
        i=rand(xlist.size&~1)|1
        i&1==1 or fail
        /\A#{RUBYIDENT}\Z/o===xlist[i] or fail
        xlist[i]=inj
        maybe_slow="slow" if xmpl.size>1000
        snippet2testmethod(xlist.to_s,maybe_slow)
      end
    }
    wrapped+injected
  }.to_s
  #puts code.split("\n")[5880..5890].join("\n")
  eval code

  error_code=RUBYBUG_EXAMPLES.map{|xmpl| 
    #remove comment if present
    /\A(.*)\s*\Z/===xmpl and xmpl=$1
    #remove x{...} wrapper if present
    /\A\s*x\s*\{(.*)\}\Z/===xmpl and xmpl=$1

    snippet2testmethod(xmpl, :known_ruby_bug)
  }.to_s
  eval error_code

  error_code=ERROR_EXAMPLES.map{|xmpl| 
    #remove comment if present
    /\A(.*)\s*\Z/===xmpl and xmpl=$1
    #remove x{...} wrapper if present
    /\A\s*x\s*\{(.*)\}\Z/===xmpl and xmpl=$1

    snippet2testmethod(xmpl, :known_error)
  }.to_s
  eval error_code

  failure_code=FAILURE_EXAMPLES.map{|xmpl| 
    #remove comment if present
    /\A(.*)\s*\Z/===xmpl and xmpl=$1
    #remove x{...} wrapper if present
    /\A\s*x\s*\{(.*)\}\Z/===xmpl and xmpl=$1

    snippet2testmethod(xmpl, :known_failure)
  }.to_s
  eval failure_code

  def known_ruby_bug
    from=caller.first
    from=from[/ in `.*'\Z/] || from[/\A[^:]*:[^:]*/]
    yield
  rescue Test::Unit::AssertionFailedError=>e
    warn "a known bug in MRI reared its head in #{from}: #{e.message}"
    if defined? @@known_ruby_bugs
      @@known_ruby_bugs+=1
    else
      @@known_ruby_bugs=1
      at_exit {warn "unfixed bugs in MRI/ParseTree: #@@known_ruby_bugs"}
    end
  rescue Exception=>e
    raise
  else
    warn "expected bug in MRI in #{from}, but was fixed(?!)"
    if defined? @@known_ruby_bugs_fixed
      @@known_ruby_bugs_fixed+=1
    else
      @@known_ruby_bugs_fixed=1
      at_exit {warn "unexpectedly fixed known MRI/ParseTree bugs: #@@known_ruby_bugs_fixed"}
    end
  end

  def test_case_that_segfaults_ruby185
    assert_equal \
      [[:op_asgn1, [:call, [:vcall, :a], :b], [:zarray], :%, [:vcall, :d]]],
      RedParse.new('a.b[]%=d','-').parse.to_parsetree(:quirks)
  end

  def test_case_that_segfaults_ruby186_slash_parsetree211
    assert_equal  [[:cdecl, [:colon3, :B], [:lit,1]]],
       RedParse.new('::B=1','-').parse.to_parsetree(:quirks)
    assert_equal  [[:cdecl, [:colon2, [:const, :A], :B], [:lit,1]]],
       RedParse.new('A::B=1','-').parse.to_parsetree(:quirks)
  end
  
  def test_case_that_segfaults_ruby187_slash_parsetree220
    assert_equal  [[:iter, [:fcall, :Proc], [:block_pass, [:dasgn_curr, :b], 0]]],
       RedParse.new('Proc{|&b|}','-').parse.to_parsetree(:quirks)
  end

  def test_cases_misparsed_by_ruby186_slash_parsetree
    {"$11111111111111111111111111111111111111111111111111111111111111111111"=>[
       [:nth_ref,  11111111111111111111111111111111111111111111111111111111111111111111]],
      "c do p (110).m end"=>[
       [:iter, [:fcall, :c], nil, [:fcall, :p, [:array, [:call, [:lit, 110], :m]]]]],
      "case F;when G; else;case; when j; end;end"=>[
       [:case,  [:const, :F],  [:when, [:array, [:const, :G]], nil],
         [:case, nil, [:when, [:array, [:vcall, :j]], nil], nil]]],
      "p = p m %(1)"=>[
       [:lasgn, :p, [:fcall, :p, [:array, [:fcall, :m, [:array, [:str, "1"]]]]]]],
      "p = p m %(1) ,&t"=>[
       [:lasgn,  :p,  [:fcall,   :p,   [:array,
         [:block_pass, [:vcall, :t], [:fcall, :m, [:array, [:str, "1"]]]]]]]],
      "p = p m %(1) ,*t"=>[
       [:lasgn,  :p,  [:fcall,   :p,
         [:array, [:fcall, :m, [:argscat, [:array, [:str, "1"]], [:vcall, :t]]]]]]],
      "p = p m %(1) ,t"=>[
       [:lasgn,  :p,  [:fcall, :p, [:array, [:fcall, :m, 
         [:array, [:str, "1"], [:vcall, :t]]]]]]],
      "p = p m %(1) do end"=>[
       [:lasgn,  :p,  [:iter, [:fcall, :p, [:array, 
         [:fcall, :m, [:array, [:str, "1"]]]]], nil]]],
      "p=556;p (e) /a"=>[
       [:block,  [:lasgn, :p, [:lit, 556]],
         [:fcall, :p, [:array, [:call, [:vcall, :e], :/, [:array, [:vcall, :a]]]]]]],
      "z{|| p (1).m}"=>[
       [:iter, [:fcall, :z], 0, [:fcall, :p, [:array, [:call, [:lit, 1], :m]]]]],
      "
        <<-'foo'
        \\'
        foo
      "=>[[:str, "        \\'\n"]],
      "
        <<-'foo'
        \\\\'
        foo
      "=>[[:str, "        \\\\'\n"]],
      "
        <<-'foo'
        \\\\\\'
        foo
      "=>[[:str, "        \\\\\\'\n"]],
      "
        <<-'foo'
        \\\\\\\\'
        foo
      "=>[[:str, "        \\\\\\\\'\n"]],
      "
        <<-'foo'
        \\\\\\\\\\'
        foo
      "=>[[:str, "        \\\\\\\\\\'\n"]],
      "
        <<-'foo'
        \\\\\\\\\\\\'
        foo
      "=>[[:str, "        \\\\\\\\\\\\'\n"]],


    }.each_pair{|code,tree| 
      assert_equal tree,RedParse.new(code,'-').parse.to_parsetree(:quirks)
    }
  end

  POWERS_OF_2={}
  i=1
  while i<1_000_000_000
    POWERS_OF_2[i]=1
    i*=2
  end

  def assert_hopefully_raises_Exception xmpl
    begin
      yield
    rescue Exception
      assert true
    else
      if defined? @@missed_syntax_errors
        @@missed_syntax_errors+=1
      else
        @@missed_syntax_errors=1
        at_exit{warn "missed syntax errors: #@@missed_syntax_errors"}
      end
      #puts "warning: syntax error expected, but none was seen, expression: <<< #{xmpl} >>>" if 
      #  POWERS_OF_2[@@missed_syntax_errors]
    end
  end

  BEGIN{File.unlink "problemexprs" rescue nil}
  def problem_exprs
    @problem_exprs||=nil
    return @problem_exprs if @problem_exprs

    @problem_exprs=File.open("problemexprs","a")

  rescue Exception
    @problem_exprs=$stdout
  end

  def check_parsing xmpl,pt=ParseTree.new
    pt_opts=[:quirks]
    pt_opts<<:ruby187 if ::VERSION["1.8.7"]
    /unparse/===xmpl and warn 'unparse in parser test data!'
    problem_exprs=problem_exprs()
    nodes=warnings=warnings2=nil
=begin
      xmpl=<<-prefix+xmpl+<<-suffix
        BEGIN{throw :never_exec_parse_data_try1,1}
        BEGIN{throw :never_exec_parse_data_try2,2}
        BEGIN{throw :never_exec_parse_data_try3,3}
        BEGIN{raise "never_exec_parse_data_try4"}
        BEGIN{raise "never_exec_parse_data_try5"}
      prefix
        ;0
      suffix
=end
#        tree=nil
#        output=false
#        loops=0
      begin
#        output=
#        catch(:never_exec_parse_data_try1){
#          catch(:never_exec_parse_data_try2){
#            catch(:never_exec_parse_data_try3){
              tree,warnings=pt.parse_tree_and_warnings(xmpl)
#            }
#          }
#        }
#        break if loops+=1 > 3
      rescue Interrupt; raise
      rescue Exception=>e
        #pp e
        #pp e.backtrace
        #raise "last gasp ParseTree exec catcher failed!"
        tree=e
        tree2=nodes=h=nil
        assert_hopefully_raises_Exception(xmpl){
          nodes=RedParse.new(xmpl,"-").parse
          h=nodes.hash
          tree2,warnings2=nodes.to_parsetree_and_warnings(*pt_opts)
        }
        assert_equal h,nodes.hash if h
      else
        begin
          nodes=RedParse.new(xmpl,"-").parse
          h=nodes.hash
          tree2,warnings2=nodes.to_parsetree_and_warnings(*pt_opts)
          assert_equal h,nodes.hash
          assert_equal tree, tree2
          assert_equal warnings, warnings2 if ENV['WARN_PICKINESS']
          if warnings != warnings2
            if defined? @@mismatched_warnings
              @@mismatched_warnings+=1
            else
              @@mismatched_warnings=1
              at_exit{warn "mismatched warnings: #@@mismatched_warnings (set WARN_PICKINESS for details)"}
            end
          end
        rescue Exception=>e
          if problem_exprs
            problem_exprs.write xmpl+"\n"
            problem_exprs.flush
          end
          raise e
        else
          if false and problem_exprs and tree!=tree2
            problem_exprs.write xmpl+"\n"
            problem_exprs.flush
          end
        end #rescue false
        
      end #until output.equal? tree 

      return unless nodes
      begin
        unparsed=nodes.unparse
        if unparsed==xmpl
          assert true
          return
        end
        reparsed= RedParse.new(unparsed,"-").parse
        if nodes.delete_extraneous_ivars! != reparsed.delete_extraneous_ivars!
          assert_equal nodes.delete_linenums!, reparsed.delete_linenums!
          warn "unparser doesn't preserve linenums perfectly in #{xmpl}"
          if defined? @@unparse_mismatched_linenums
              @@unparse_mismatched_linenums+=1
          else
              @@unparse_mismatched_linenums=1
              at_exit{warn "unparse mismatched linenums: #@@unparse_mismatched_linenums"}
          end
        else
          assert true 
        end
      rescue Exception
        raise unless Exception===tree
      end

      unless Exception===tree
        tree3=reparsed.to_parsetree(*pt_opts)
        assert_equal tree, tree3
      else #missing a syntax errr, but that's been noted already
      end

#  rescue Exception=>e:
#      raise "error: #{e}:#{e.class} while testing '#{xmpl}'"
  end
end
