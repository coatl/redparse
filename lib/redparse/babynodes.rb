require 'pp'
require 'rubygems'
require 'rubylexer'
require 'reg'




class RedParse
  module Nodes
    #import token classes from rubylexer
    tokenclass=(::Token rescue RubyLexer::Token)
    ObjectSpace.each_object(Class){|k| 
      k<=tokenclass and self.const_set k.name[/[^:]+$/], k
    }

    module SimpleToLisp
      def to_lisp; to_s end
    end
    [NumberToken, SymbolToken, VarNameToken, MethNameToken].each{|tokclass|
      tokclass.send :include, SimpleToLisp
    }

    class Token
      def to_parsetree; [parsetree] end
    end

    class NumberToken
      def parsetree
        [:lit, ident]
      end
    end
  
    class KeywordToken
      def not_real!
        @not_real=true
      end
 
      def not_real?
        @not_real if defined? @not_real
      end
    end

    class SymbolToken
      def parsetree
        [:lit, ident[1..-1].to_sym]
      end
    end

    class VarNameToken
      def parsetree; 
        type=case ident[0]
          when ?$: :gvar
          when ?@: ident[1]==?@ ? :cvar : :ivar
          when ?A..?Z: :const
          else :lvar
        end
        [type,ident.to_sym]
      end
    end   

    class ExprNode
      def initialize(*data)
        @data=data
      end

      attr :data
      alias unwrap data
 
      def to_parsetree; [parsetree] end
      def parsetree
        "wrong(#{inspect})"
      end

      def parsetrees list
        !list.empty? and list.map{|node| node.parsetree}
      end 
    end
    class OpNode<ExprNode
      def initialize(expr1,op,expr2)
        super
      end

      def to_lisp
        "(#{@data[1]} #{@data[0].to_lisp} #{@data[2].to_lisp})"
      end

      def parsetree
        [:call, 
           @data[0].parsetree, 
           @data[1].ident.to_sym, 
           [:array, @data[2].parsetree]
        ]
      end
    end

    class UnOpNode<ExprNode
      def initialize(op,val)
        @ident=op.ident
        super(op,val)
      end
   
      attr :ident

      def to_lisp
        "(#{@data[0]} #{@data[1].to_lisp})"
      end

      def parsetree
        case @data[0].ident
        when /^[*&]/: "huh"
        when "!": [:not, @data[1].parsetree]
        when "defined?": [:defined, @data[1].parsetree]
        else
          [:call, @data[1].parsetree, @data[0].ident.to_sym]
        end
      end
    end

    class ParenedNode<ExprNode
      def initialize(*args)
        @data=[args[1]]
      end

      def to_lisp
        @data.first.to_lisp
      end

      def parsetree
        @data.first.parsetree
      end
    end

  end
end


