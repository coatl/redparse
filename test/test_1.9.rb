require 'test/unit'
require 'rubylexer/test/oneliners_1.9'


class TestsFor1_9 < Test::Unit::TestCase
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
