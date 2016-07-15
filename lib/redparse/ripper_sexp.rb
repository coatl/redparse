#
# $Id: sexp.rb 25189 2009-10-02 12:04:37Z akr $
#
# Copyright (c) 2004,2005 Minero Aoki
#
# This program is free software.
# You can distribute and/or modify this program under the Ruby License.
# For details of Ruby License, see ruby/COPYING.
#
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

#stolen shamelessly from ripper/sexp.rb
#(among others. the tables came from elsewhere)

class RedParse::Ripper

  SCANNER_EVENT_TABLE=
  {:CHAR=>1, :__end__=>1, :backref=>1, :backtick=>1, :comma=>1, :comment=>1, :const=>1, :cvar=>1, 
   :embdoc=>1, :embdoc_beg=>1, :embdoc_end=>1, :embexpr_beg=>1, :embexpr_end=>1, :embvar=>1, 
   :float=>1, :gvar=>1, :heredoc_beg=>1, :heredoc_end=>1, :ident=>1, :ignored_nl=>1, :int=>1, 
   :ivar=>1, :kw=>1, :label=>1, :lbrace=>1, :lbracket=>1, :lparen=>1, :nl=>1, :op=>1, :period=>1, 
   :qwords_beg=>1, :rbrace=>1, :rbracket=>1, :regexp_beg=>1, :regexp_end=>1, :rparen=>1, :semicolon=>1, 
   :sp=>1, :symbeg=>1, :tlambda=>1, :tlambeg=>1, :tstring_beg=>1, :tstring_content=>1, :tstring_end=>1, 
   :words_beg=>1, :words_sep=>1
  }

  PARSER_EVENT_TABLE= 
  {:BEGIN=>1, :END=>1, :alias=>2, :alias_error=>1, :aref=>2, :aref_field=>2, :arg_ambiguous=>0, 
   :arg_paren=>1, :args_add=>2, :args_add_block=>2, :args_add_star=>2, :args_new=>0, :array=>1, 
   :assign=>2, :assign_error=>1, :assoc_new=>2, :assoclist_from_args=>1, :bare_assoc_hash=>1, 
   :begin=>1, :binary=>3, :block_var=>2, :block_var_add_block=>2, :block_var_add_star=>2, :blockarg=>1, 
   :bodystmt=>4, :brace_block=>2, :break=>1, :call=>3, :case=>2, :class=>3, :class_name_error=>1, 
   :command=>2, :command_call=>4, :const_path_field=>2, :const_path_ref=>2, :const_ref=>1, :def=>3, 
   :defined=>1, :defs=>5, :do_block=>2, :dot2=>2, :dot3=>2, :dyna_symbol=>1, :else=>1, :elsif=>3, 
   :ensure=>1, :excessed_comma=>1, :fcall=>1, :field=>3, :for=>3, :hash=>1, :if=>3, :if_mod=>2, 
   :ifop=>3, :lambda=>2, :magic_comment=>2, :massign=>2, :method_add_arg=>2, :method_add_block=>2, 
   :mlhs_add=>2, :mlhs_add_star=>2, :mlhs_new=>0, :mlhs_paren=>1, :module=>2, :mrhs_add=>2, 
   :mrhs_add_star=>2, :mrhs_new=>0, :mrhs_new_from_args=>1, :next=>1, :opassign=>3, 
   :operator_ambiguous=>2, :param_error=>1, :params=>5, :paren=>1, :parse_error=>1, :program=>1, 
   :qwords_add=>2, :qwords_new=>0, :redo=>0, :regexp_add=>2, :regexp_literal=>2, :regexp_new=>0, 
   :rescue=>4, :rescue_mod=>2, :rest_param=>1, :retry=>0, :return=>1, :return0=>0, :sclass=>2, 
   :stmts_add=>2, :stmts_new=>0, :string_add=>2, :string_concat=>2, :string_content=>0, 
   :string_dvar=>1, :string_embexpr=>1, :string_literal=>1, :super=>1, :symbol=>1, :symbol_literal=>1, 
   :top_const_field=>1, :top_const_ref=>1, :unary=>2, :undef=>1, :unless=>3, :unless_mod=>2, 
   :until=>2, :until_mod=>2, :var_alias=>2, :var_field=>1, :var_ref=>1, :void_stmt=>0, :when=>3, 
   :while=>2, :while_mod=>2, :word_add=>2, :word_new=>0, :words_add=>2, :words_new=>0, 
   :xstring_add=>2, :xstring_literal=>1, :xstring_new=>0, :yield=>1, :yield0=>0, :zsuper=>0
  } 

  PARSER_EVENTS=PARSER_EVENT_TABLE.keys
  SCANNER_EVENTS=SCANNER_EVENT_TABLE.keys

  # [EXPERIMENTAL]
  # Parses +src+ and create S-exp tree.
  # This method is for mainly developper use.
  #
  #   require 'ripper'
  #   require 'pp
  #
  #   pp Ripper.sexp("def m(a) nil end")
  #     #=> [:program,
  #          [:stmts_add,
  #           [:stmts_new],
  #           [:def,
  #            [:@ident, "m", [1, 4]],
  #            [:paren, [:params, [[:@ident, "a", [1, 6]]], nil, nil, nil]],
  #            [:bodystmt,
  #             [:stmts_add, [:stmts_new], [:var_ref, [:@kw, "nil", [1, 9]]]],
  #             nil,
  #             nil,
  #             nil]]]]
  #
  def self.sexp(src, filename = '-', lineno = 1)
    SexpBuilderPP.new(src, filename, lineno).parse
  end

  def self.sexp_raw(src, filename = '-', lineno = 1)
    SexpBuilder.new(src, filename, lineno).parse
  end

  class SexpBuilderPP < ::RedParse::Ripper   #:nodoc:
    private

    PARSER_EVENT_TABLE.each do |event, arity|
      if /_new\z/ =~ event.to_s and arity == 0
        module_eval(<<-End, __FILE__, __LINE__ + 1)
          def on_#{event}
            []
          end
        End
      elsif /_add\z/ =~ event.to_s
        module_eval(<<-End, __FILE__, __LINE__ + 1)
          def on_#{event}(list, item)
            list.push item
            list
          end
        End
      else
        module_eval(<<-End, __FILE__, __LINE__ + 1)
          def on_#{event}(*args)
            [:#{event}, *args]
          end
        End
      end
    end

    SCANNER_EVENTS.each do |event|
      module_eval(<<-End, __FILE__, __LINE__ + 1)
        def on_#{event}(tok)
          [:@#{event}, tok, [lineno(), column()]]
        end
      End
    end
  end

  class SexpBuilder < ::RedParse::Ripper   #:nodoc:
    private

    PARSER_EVENTS.each do |event|
      module_eval(<<-End, __FILE__, __LINE__ + 1)
        def on_#{event}(*args)
          args.unshift :#{event}
          args
        end
      End
    end

    SCANNER_EVENTS.each do |event|
      module_eval(<<-End, __FILE__, __LINE__ + 1)
        def on_#{event}(tok)
          [:@#{event}, tok, [lineno(), column()]]
        end
      End
    end
  end

end
