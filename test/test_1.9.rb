require 'test/unit'
require 'rubylexer/test/oneliners_1.9'
require "redparse"

class TestsFor1_9 < Test::Unit::TestCase
  include Ruby1_9OneLiners

  RUBY_1_9_TO_1_8_EQUIVALENCES=[
    '{a:  b}'...'{:a=>b}',
    '{a:  b, c:  d}'...'{:a=>b, :c=>d}',
    "a ? b\n :  c"..."a ? b : \n c",
    '{a:  1,b:  2,c=>3}'...'{:a=>1,:b=>2,c=>3}'

  ]

  RUBY_1_9_TO_1_8_EQUIVALENCES_BUT_FOR_STRESC=[
    '"foo"'...'"foo"',
    '"foo#{bar}"'...'"foo#{bar}"',

    '%W[foo]'...'%W[foo]',
    '%W[foo#{bar}]'...'%W[foo#{bar}]',

    '`foo`'...'`foo`',
    '`foo#{bar}`'...'`foo#{bar}`',

    '//'...'//',
    '/aa/i'...'/aa/i',
    '/a#{a}/u'...'/a#{a}/u',
    '/a#{"a"}/e'...'/a#{"a"}/e',
    '/b#{__FILE__}/s'...'/b#{__FILE__}/s',
    '/bb/m'...'/bb/m',
    '/b b/x'...'/b b/x',
    '/b#{b}/o'...'/b#{b}/o',
    '
     a
      .b'...
    '
     a\
      .b',
  ]
  RUBY_1_9_TO_1_9_EQUIVALENCES=[
    '"♥"'...'?♥',
    '"\u2666"'...'?\u2666',
  ]


  RUBY_1_9_VALID=[
    "__ENCODING__",
    'not (true).to_s',
    '"\u2665"',
    '"♥"',
    '"\u{2665 2666}"',
    '"♥♦"',
    '"\u{10fffd}"',
    '"􏿽"'    ,
    '?\u2665','?♥',
    '?\u{10fffd}','?􏿽'    ,


    'def !=(x); end',
    'def !~(x); end',
    'alias x !=',
    'alias != x',
    'alias x !~',
    'alias !~ x',
    'alias !~ !=',
    'alias != !~',
    'alias != `',
    'undef !=',
    'undef !~',
    ':!=',
    ':!~',

    'def !@; end',
    'alias x !@',
    'alias !@ x',
    'alias !@ `',
    'undef !@',
    ':!@',

    'z=*a,b,c',
    'z=a,*b,c',
    'z=a,*b,*c',
    'z=a,*b,c,*d',
    'z=*a,*b,*c,*d',
    'z=*a,*b,*c,*d,e',


    'proc{|a,*b,c|d}',
    'proc{|a,*b,(c1,*c2,c3)|d}',
    'proc{|*a,b,c|d}',
    'proc{|&a| x}',
    'proc{|a,&b| x}',
    'proc{|a,b=10| x}',
    'proc{|a=10,b| x}',

    'z(*a,b,c)',
    'z(a,*b,c)',
    'z(a,*b,*c)',
    'z(a,*b,c,*d)',
    'z(*a,*b,*c,*d)',
    'z(*a,*b,*c,*d,e)',
    'z[*a,b,c]',
    'z[a,*b,c]',
    'z[a,*b,*c]',
    'z[a,*b,c,*d]',
    'z[*a,*b,*c,*d]',
    'z[*a,*b,*c,*d,e]',
 
    'z[*a,b,c]=1',
    'z[a,*b,c]=1',
    'z[a,*b,*c]=1',
    'z[a,*b,c,*d]=1',
    'z[*a,*b,*c,*d]=1',
    'z[*a,*b,*c,*d,e]=1',
 
    '[*a,b,c]',
    '[a,*b,c]',
    '[a,*b,*c]',
    '[a,*b,c,*d]',
    '[*a,*b,*c,*d]',
    '[*a,*b,*c,*d,e]',
 
    'not(true)','not true',
    'not(+1)','not +1',
    'not (true).to_s','not (true).to_s',

    
  ]

  RUBY_1_9_INVALID=[
    '{1,2,3,4}',
    'if a: b; end',
    'unless a: b; end',
    'while a: b; end',
    'until a: b; end',
    'case z; when a: b; end',
    'proc{|a,b;c| d e f }',
  ]

  EXPECT_1_METHOD.concat [
    '1.!=2',
    '1.!~2',
    'a,*b,c=d',
    'a,*b,(c1,*c2,c3)=d',
    '*a,b,c=d',
    'proc{|a=1,*b,(c1,*c2,c3),d=2,&e;f,g,h| [ b,c3,d,e,f,g,h ]}',
    'def z(a=1,*b,(c1,*c2,c3),d=2,&e;f,g,h) [ b,f,g,h ] end',
    '$f.($x,$y)',
    '$f::($x,$y)',
    '$f.($x,$y){}',
    '$f.($x,$y) do |stuff| $yada-$yada end',
    'proc{|a=1,*b,d| b}',
    'proc{|&e;f| e }',
    'proc{|aa,a=1,az,*b,(c1,*c2,c3),da,d=2,dz,(e1=1,*e2,e3=3),&m;f,g,h| [ b,c3,d,e1,e2,e3,f,g,h,m ]}',
    'def z(aa,a=1,az,*b,(c1,*c2,c3),da,d=2,dz,(e1=1,*e2,e3=3),&m) [ b,c3,d,e1,e2,e3,m ] end',
    'aa,a=1,az,*b,(c1,*c2,c3),da,d=2,dz,(e1=1,*e2,e3=3)=k;  b,c3,d,e1,e2,e3 ',
    '->(aa,a=1,az,*b,(c1,*c2,c3),da,d=2,dz,(e1=1,*e2,e3=3),&m;f,g,h){[ b,c3,d,e1,e2,e3,f,g,h,m ]}',
    '->(a=1,*b,(c1,*c2,c3),d=2,&e;f,g,h){ [ b,c3,d,e,f,g,h ]}',
    '->(a=1,*b,(c1,*c2,c3),d=2,&e;f,g,h) do [ b,c3,d,e,f,g,h ] end',
    '->a=1,*b,(c1,*c2,c3),d=2,&e;f,g,h{ [ b,c3,d,e,f,g,h ]}',
  ]
  EXPECT_2_METHODS=EXPECT_1_METHOD.grep /->/
  EXPECT_1_METHOD.replace(EXPECT_1_METHOD-EXPECT_2_METHODS)
  EXPECT_1_METHOD.concat EXPECT_NO_METHODS.grep /->/
  EXPECT_NO_METHODS.concat [
  ]
  EXPECT_NO_METHODS.replace(EXPECT_NO_METHODS-EXPECT_1_METHOD)

  WEAK=RUBY_1_9_VALID+EXPECT_NO_METHODS+EXPECT_1_METHOD+RUBY_1_9_TO_1_9_EQUIVALENCES.map{|r| [r.first,r.last]}.flatten

  warn "do something more with WEAK cases"

  include RedParse::Nodes
  RUBY_1_9_PATTERNS={
    'not(true).to_s'=>+CallNode[+KWCallNode[nil, "not", +[+VarLikeNode["true"]], nil,nil], "to_s" ,nil,nil,nil],
    'f.(a=1,2,3)'=>+CallNode[_,"()",-{:size=>3},nil,nil]
  }

  def parser src,name
    RedParse.new(src,name,1,[],:rubyversion=>1.9,:cache_mode=>:none,:encoding=>:utf8)
  end

  EXCEPTIONS={}
  File.open("test/unparse_1.9_exceptions.txt"){|f|
    until f.eof?
      l=f.readline
      l<<f.readline until l.chomp!"\\\\\\///\n"
      EXCEPTIONS.[]= *l.split(' ====> ',2)
    end
  } if File.exist?("test/unparse_1.9_exceptions.txt")

  def confirm_error?(code,code2)
    puts 'unparse didn`t match. original:'
    puts code
    puts 'unparsed:'
    puts code2
    answer=nil
    until answer==true or answer==false
      puts 'is this difference an error? (y/n)'
      answer=gets
      answer=true if /\Ay/i===answer
      answer=false if /\An/i===answer
    end
    return answer
  end

  def assert_unparses_to pt,code
    code2=pt.unparse
    if code==code2 or EXCEPTIONS[code]==code2
      assert true
    elsif confirm_error?(code,code2)
      assert_equal code,code2 
    else
      assert true
      fail if %r{ ====> |\\\\\\///\n}===code2+code
      File.open("test/unparse_1.9_exceptions.txt","a"){|fd| fd.print code,' ====> ', code2,"\\\\\\///\n" }
    end
  end

  def test_ruby19_equivs
    RUBY_1_9_TO_1_8_EQUIVALENCES.each{|pair|
      new,old=pair.first,pair.last
      pt19=RedParse.new(new,'(eval)',1,[],:rubyversion=>1.9,:cache_mode=>:none).parse
      pt18=RedParse.new(old,'(eval)',1,[],:cache_mode=>:none).parse
      assert_equal pt18,pt19
      assert_unparses_to pt19,new
    }
  end
 
  def test_ruby19_19_equivs_but_for_open_close
    RUBY_1_9_TO_1_9_EQUIVALENCES.each{|pair|
      new,old=pair.first,pair.last
      pt1=parser(new,'(eval)').parse
      pt2=parser(old,'(eval)').parse
      assert_unparses_to pt1,new
      assert_unparses_to pt2,old
      pt1.instance_eval{@open=@close=nil if @open}
      pt2.instance_eval{@open=@close=nil if @open}
      assert_equal pt1,pt2
    }
  end
 
  def test_ruby19_equivs_but_for_stresc
    RUBY_1_9_TO_1_8_EQUIVALENCES_BUT_FOR_STRESC.each{|pair|
      new,old=pair.first,pair.last
      pt19=RedParse.new(new,'(eval)',1,[],:rubyversion=>1.9,:cache_mode=>:none).parse
      pt18=RedParse.new(old,'(eval)',1,[],:cache_mode=>:none).parse
      if pt18.instance_variable_get(:@bs_handler)==:dquote_esc_seq
        pt18.instance_variable_set :@bs_handler,:dquote19_esc_seq
      else
        pt18.instance_variable_set :@bs_handler,:Wquote19_esc_seq
        pt18.instance_variable_get(:@parses_like).each{|x|
          x.instance_variable_set :@bs_handler,:Wquote19_esc_seq if x.instance_variable_get :@bs_handler
        }
      end
      assert_equal pt18,pt19
    }
  end
 
  RUBY_1_9_VALID.each{|xmpl|
    define_method("test_valid_#{xmpl}") do
      pt19=parser(xmpl,'(eval)').parse
      assert_nil pt19.errors
      assert_unparses_to pt19,xmpl
    end
  }

  def test_ruby19_invalid
    RUBY_1_9_INVALID.each{|xmpl|
      begin
        pt19=parser(xmpl,'(eval)').parse
      rescue Exception
        assert true
        next
      end
      warn "1.9error expected, but not seen in '#{xmpl}'" unless pt19.errors
    }
  end

  def test_ruby19_patterns
    RUBY_1_9_PATTERNS.each_pair{|code,pattern|
      pt=RedParse.new(code,'(eval)',1,[],:rubyversion=>1.9,:cache_mode=>:none).parse
      assert_match pattern, pt
    }
  end





  def count_methods(tree)
    count=0
    tree.walk{|parent,i,subi,node|
      case node
      when CallSiteNode, MethodNode; count+=1
      end
      true
    }
    return count
  end

  def test_1_9
    EXPECT_NO_METHODS.each{|snippet|
      tree=RedParse.new(snippet,"-e",1,[],:rubyversion=>1.9,:cache_mode=>:none)
      begin
        tree=tree.parse
      rescue Exception=>e
        raise e,e.message+"during parsing of #{snippet}\n",e.backtrace
      end
      count=count_methods(tree)
      assert_equal count,0
    }

    EXPECT_1_METHOD.each{|snippet|
      tree=RedParse.new(snippet,"-e",1,[],:rubyversion=>1.9,:cache_mode=>:none)
      begin
        tree=tree.parse
      rescue Exception=>e
        raise e,e.message+"during parsing of #{snippet}\n",e.backtrace
      end
      count=count_methods(tree)
      assert_equal count,1
    }
  end
end
