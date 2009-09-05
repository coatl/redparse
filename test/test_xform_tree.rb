require 'test/unit'
require 'regxform'
require "redparse"

class XformTreeTest < Test::Unit::TestCase
  include RedParse::Nodes
  def test_replacing_with_self
    pt=RedParse.new("1").parse
    xformer=
      LiteralNode >> ParenedNode[SequenceNode[LiteralNode[42],~:self]]
    pt=pt.xform_tree!(xformer)
    assert_equal(ParenedNode[SequenceNode[LiteralNode[42],LiteralNode[1]]], pt)
  end

  def test_modifying_same_node_twice
    pt=RedParse.new("1").parse
    xformers= [
      LiteralNode >> ParenedNode[SequenceNode[StringNode['foo'],~:self]],
      LiteralNode >> ParenedNode[SequenceNode[StringNode['bar'],~:self]]
    ]
    pt=pt.xform_tree!(*xformers)
    assert_equal(ParenedNode[SequenceNode[StringNode['bar'],
                   ParenedNode[SequenceNode[StringNode['foo'],
                     LiteralNode[1]
                   ]]
                 ]], pt)
  end

  def test_modifying_same_node_thrice
    pt=RedParse.new("1").parse
    xformers= [
      LiteralNode >> ParenedNode[SequenceNode[StringNode['foo'],~:self]],
      LiteralNode >> ParenedNode[SequenceNode[StringNode['bar'],~:self]],
      LiteralNode >> ParenedNode[SequenceNode[StringNode['baz'],~:self]],
    ]
    pt=pt.xform_tree!(*xformers)
    assert_equal(ParenedNode[SequenceNode[StringNode['baz'],
                   ParenedNode[SequenceNode[StringNode['bar'],
                     ParenedNode[SequenceNode[StringNode['foo'],
                       LiteralNode[1]
                     ]]
                   ]]
                 ]], pt)
  end
end
