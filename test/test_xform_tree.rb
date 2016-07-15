=begin copyright
    redparse - a ruby parser written in ruby
    Copyright (C) 2008,2009, 2012, 2016  Caleb Clausen

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

    def mktmp
      item_that{ 
         #Process.kill "INT",0; 
         [['foo']]
      }
    end

  def test_xform_tree_doesnt_modify_original
    tree=RedParse.new('def foo.bar; baz end').parse
    tree2=Marshal.load(Marshal.dump(tree))
    tree.xform_tree!( -{:receiver => Node>>mktmp%:t} )
    assert_equal tree, tree2
  end

  def test_xform_tree_doesnt_modify_original2
    tree=RedParse.new('def foo.bar; baz end').parse
    tree2=Marshal.load(Marshal.dump(tree))
    tree.xform_tree!( -{:receiver => Node>>mktmp%:t}  >> [~:t])
    assert_equal tree, tree2
  end
end
