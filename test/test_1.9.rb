require 'test/unit'
require 'rubylexer/test/oneliners_1.9'
require "redparse"

class TestsFor1_9 < Test::Unit::TestCase
  RUBY_1_9_TO_1_8_EQUIVALENCES=[
    '{a:  b}'...'{:a=>b}',
    '{a:  b, c:  d}'...'{:a=>b, :c=>d}',
    "a ? b\n :  c"..."a ? b : \n c",

    'not(true)'...'not true',
    'not(+1)'...'not +1',
    'not (true).to_s'...'not (true).to_s', #equivalent, but parser gets there different ways
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
  ]


  RUBY_1_9_VALID=[
    'not (true).to_s',
  ]

  include RedParse::Nodes
  RUBY_1_9_PATTERNS={
    'not(true).to_s'=>+CallNode[+UnOpNode["not", +VarLikeNode["true"]], "to_s"],
  }

  def test_ruby19_equivs
    RUBY_1_9_TO_1_8_EQUIVALENCES.each{|pair|
      new,old=pair.first,pair.last
      pt19=RedParse.new(new,'(eval)',1,[],:rubyversion=>1.9,:cache_mode=>:none).parse
      pt18=RedParse.new(old,'(eval)',1,[],:cache_mode=>:none).parse
      assert_equal pt18,pt19
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
 
  def test_ruby19_valid
    RUBY_1_9_VALID.each{|xmpl|
      pt19=RedParse.new(xmpl,'(eval)',1,[],:rubyversion=>1.9,:cache_mode=>:none).parse
      assert_nil pt19.errors
    }
  end

  def test_ruby19_patterns
    RUBY_1_9_PATTERNS.each_pair{|code,pattern|
      pt=RedParse.new(code,'(eval)',1,[],:rubyversion=>1.9,:cache_mode=>:none).parse
      assert_match pattern, pt
    }
  end




  include Ruby1_9OneLiners

  def count_methods(tree)
    count=0
    tree.walk{|node|
      case node
      when CallSiteNode, MethodNode; count+=1
      end
    }
    return count
  end

  def test_1_9
    EXPECT_NO_METHODS.each{|snippet|
      tree=RedParse.new(snippet,"-e").parse
      count=count_methods(tree)
      assert_equal count,0
    }

    EXPECT_1_METHOD.each{|snippet|
      tree=RedParse.new(snippet,"-e").parse
      count=count_methods(tree)
      assert_equal count,1
    }
  end
end
