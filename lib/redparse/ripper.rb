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
require 'redparse'
class RedParse
  class Ripper
    def initialize(text,file="(eval)",line=1,options={})
      options[:rubyversion]||=1.9
      @lineno=@column=0 #hack, for now. fix this later
      @parser=RedParse.new(text,file,line,options)
    end
    attr_accessor :lineno,:column,:parser
  
    alias [] instance_variable_get
    alias []= instance_variable_set

    def parse options={}
      @quirks=options[:quirks]
      tree=@parser
      tree=tree.parse if tree.respond_to? :parse
      on_program tree.rip(self)
    end

    def quirks?; @quirks end

    def self.instrumentSexpBuilder k
      events=k.instance_methods.grep(/\Aon_/)
      events.map!{|e| <<-"endcode" }
        alias noinst_#{e} #{e}
        def #{e}(*a)
          @record<<a.dup.unshift(:#{e})
          noinst_#{e}(*a)
        end
      endcode
      events<<<<-"endcode"
        alias noinst_parse parse
        def parse *a
          @record=[]
          result=[@record,(noinst_parse *a)]
          @record=nil
          return result
        end
      endcode
      k.module_eval events.join
    end
  end

  class LiteralNode
    def rip p
      if Symbol===val
        #p.pos huh
        p.on_symbol_literal \
          p.on_symbol \
            p.on_ident(val.to_s)
      elsif Integer===val
        p.on_int(val.to_s)
      else fail
      end
    end
  end

  class SequenceNode
    def rip p
      stmts_rip self,p
    end
  end

  class CallNode
    def rip p
      receiver=receiver()
      name=p.on_ident(name().dup)
      return p.on_var_ref(name) if p.quirks? and !receiver and !params and !block and @not_real_parens
      if receiver
        result=[@not_real_parens ? :on_command_call : :on_call, receiver.rip(p), :".", name]
      else
        result=[@not_real_parens ? :on_command : :on_fcall, name]
      end

      result=p.send(*result)
      result=p.on_method_add_arg(
        result,
        p.on_arg_paren(
          p.on_args_add_block(
            args_rip(params,p),
            false
      ))) if params

      result=p.on_method_add_block(result,p.on_brace_block(nil,block.rip(p))) if block

      return result
    end
  end

  class OpNode
    def rip p
      p.on_binary(left.rip(p), op.to_sym, right.rip(p))
    end
  end
  class AndNode
    def rip p
      p.on_binary(left.rip(p), op.to_sym, right.rip(p))
    end
  end
  class OrNode
    def rip p
      p.on_binary(left.rip(p), op.to_sym, right.rip(p))
    end
  end

  class UnOpNode
    def rip p
      p.on_unary(op.to_sym, val.rip(p))
    end
  end

  class RescueOpNode
    def rip p
      p.send( "on_#{op}_mod", rescue_with.rip(p), body.rip(p) )
    end
  end

  class WhileOpNode
    def rip p
      if BeginNode===consequent
        if @quirks
          #this is WRONG!!!, but it's how ripper works... urk
          #if expression modified by a while operator is a begin node,
          #ruby always executes the loop at least once, 
          #and doesn't check the condition til after the first execution.
          #Ripper reverses the order of condition and consequent to signal
          #an execute-at-least-once loop. But, that's not good enough.
          #because 'begin a end while b' now parses the same as 'b while begin a end'
          p.on_while_mod( condition.rip(p), consequent.rip(p) ) 
        else
          p.on_while_mod( consequent.rip(p), condition.rip(p), :loop_first )
        end
      else
        p.on_while_mod( consequent.rip(p), condition.rip(p) )
      end
    end
  end

  class UntilOpNode
    def rip p
      if BeginNode===consequent
        if @quirks
          #this is WRONG!!!, but it's how ripper works... urk
          #if expression modified by a until operator is a begin node,
          #ruby always executes the loop at least once, 
          #and doesn't check the condition til after the first execution.
          #Ripper reverses the order of condition and consequent to signal
          #an execute-at-least-once loop. But, that's not good enough.
          #because 'begin a end until b' now parses the same as 'b until begin a end'
          p.on_until_mod( condition.rip(p), consequent.rip(p) ) 
        else
          p.on_until_mod( consequent.rip(p), condition.rip(p), :loop_first )
        end
      else
        p.on_until_mod( consequent.rip(p), condition.rip(p) )
      end
    end
  end

  class IfOpNode
    def rip p
      p.on_if_mod( condition.rip(p), consequent.rip(p) )
    end
  end

  class UnlessOpNode
    def rip p
      p.on_unless_mod( condition.rip(p), consequent.rip(p) )
    end
  end

  class VarNode
    def rip p
      p.on_var_ref(
        case name[0]
        when ?$; p.on_gvar(name)
        when ?@; name[1]==?@ ? p.on_cvar(name) : p.on_ivar(name)
        when ?A..?Z; p.on_const(name)
        else p.on_ident(name)
        end
      )
    end
  end

  class ConstantNode
    def rip p
      if first
        start=p.on_var_ref \
          p.on_const(first)
        start_i=1
      else
        start=p.on_top_const_ref \
          p.on_const(self[1])
        start_i=2
      end
      (start_i...size).inject(start){|sum,i| 
        p.on_const_path_ref(sum,p.on_const(self[i])) 
      }
    end
  end

  class StringNode
    def rip p
      list=self.dup
      list.shift if String===list.first and list.first.empty?
      p.on_string_literal \
        list.inject(p.on_string_content){|sum,chunk|
          p.on_string_add sum,
            if String===chunk
              p.on_tstring_content chunk
            else
              p.on_string_embexpr chunk.rip(p)
            end
        }
    end
  end

  class Node
    def rip_and_rescues p
        unless rescues.empty?
          r=rescues.map{|resc| resc.rip(p)}
          r.each_with_index{|x,i| x<<r[i+1] unless i+1==r.size }
          r=r.first
        end
        p.on_bodystmt(
          force_stmt_list_rip(body,p),
          r,
          else_&&else_.rip(p),
          ensure_&&p.on_ensure(force_stmt_list_rip(ensure_,p))
        )
    end
    def rip_explode! init,receiver=self,&block
      receiver.inject(init,&block)
    end
    def stmts_rip list,p
      list.inject(p.on_stmts_new){|sum,expr| p.on_stmts_add(sum,expr.rip(p)) }
    end
    def force_stmt_list_rip expr,p
      if SequenceNode===expr
        expr.rip(p)
      else
        stmts_rip [expr],p
      end
    end
    def args_rip list,p
      list.inject(p.on_args_new){|sum,param| 
        p.on_args_add(sum,param.rip(p)) 
      }
    end
  end

  class ClassNode
    def rip p
      p.on_class( name.rip(p), parent&&parent.rip(p),  rip_and_rescues(p) )
    end
  end

  class ModuleNode
    def rip p 
      p.on_module( name.rip(p), rip_and_rescues(p) )
    end
  end

  class MetaClassNode
    def rip p
      p.on_sclass( object.rip(p), rip_and_rescues(p) )
    end
  end
  
  class MethodNode
    def get_while params,should_be
      param=nil
      result=[]
      result<<yield( param )while should_be===params.first and param=params.shift
    ensure
      return result unless result.empty?
    end

    def rip p
      params=args ? args.dup : []
      params2=[]
      #param=nil

      params2.push get_while(params,VarNode){|param| p.on_ident param.name}
      params2.push get_while(params,AssignNode){|param| [p.on_ident(param.left.name), param.right.rip(p)]}
      get_while(params,UnaryStarNode){|param| params2.push p.on_rest_param p.on_ident param.val.name; break }
      params2.push get_while(params,VarNode){|param| p.on_ident param.name}
      get_while(params,UnaryAmpNode){|param| params2.push p.on_blockarg p.on_ident param.val.name; break }

      params=p.on_params( *params2 )
      params=p.on_paren( params ) if has_parens?

      result=[p.on_ident(name), params, rip_and_rescues(p)]
      result.unshift receiver.rip(p), p.on_period(".") if receiver
      p.on_def( *result )
    end
  end

  class BeginNode
    def rip p
      p.on_begin rip_and_rescues(p)
    end
  end

  class RescueNode
    def rip p
      p.on_rescue( 
        exceptions.empty??nil:exceptions.map{|ex| ex.rip(p)}, 
        name&&p.on_var_field(p.on_ident(name.name)), 
        force_stmt_list_rip(action,p)
      )
    end
  end

  class IfNode
    def rip p
      elses=p.on_else(force_stmt_list_rip(otherwise,p)) if otherwise
      elsifs.reverse_each{|ei| 
       elses=p.on_elsif(
          ei.condition.rip(p),
          force_stmt_list_rip(ei.consequent,p),
          elses
        )
      }
      p.on_if(
        condition.rip(p),
        force_stmt_list_rip(consequent,p),
        elses
      )
    end
  end

  class LoopNode
    def rip p
      event= @reverse ? :on_until : :on_while
      p.send(event, condition.rip(p), force_stmt_list_rip(body,p))
    end
  end

  class ParenedNode
    def rip p 
      list= SequenceNode===val ? val : [val]
      p.on_paren stmts_rip list,p
    end
  end

  class TernaryNode
    def rip p
      p.on_ifop condition.rip(p), consequent.rip(p), otherwise.rip(p)
    end
  end

  class RangeNode
    def rip p
      dots= exclude_end? ? :on_dot3 : :on_dot2
      p.send dots, left.rip(p), right.rip(p)
    end
  end


  class ArrayLiteralNode
    def rip p
      p.on_array(args_rip(self,p))
    end
  end

  class HashLiteralNode
    def rip p
      list=[]
      (0...size).step(2){|i| list.push p.on_assoc_new(self[i].rip(p),self[i+1].rip(p)) }
      p.on_hash((p.on_assoclist_from_args(list) unless empty?))
    end
  end

end
