class RedParse
=begin
3 LR-Parsing Mechanics
We briefly explain the fundamentals of shift-reduce parsing (which represents 
the LR(1) family) without going into any more detail than necessary for 
subsequent exposition. LALR(1) parsers like yacc simulate, either directly or 
indirectly, a very simple automaton with a stack of automaton states [FL88]. 
(Parsers generated by yacc also maintain a semantic stack, but since that stack 
grows in parallel with the state stack, we only describe the use of the state
stack here.) Simulating the automaton requires two mechanisms: one for 
determining the action, which is determined by the current input symbol and the 
state on the top of the stack, and one for determining state transitions based 
on the current top of stack and a grammar symbol. At parser-generation time 
LALR(1) grammar analysis builds these tables, called action and goto, 
respectively. (The analysis is necessary regardless of whether a table-driven 
or hard-coded parser is desired.) Functionally, these tables have the following 
signatures.

goto: state x symbol -> state
action: state x token -> {shift,reduce_y,accept,error}

There are only four possible actions: reduce, shift, accept, and error. Reduce 
actions are parameterized by the grammar production being reduced. Actions are 
described below. let TOS be the state on the top of the stack, and let 
la_identity be the current lookahead token.

shift  A shift pushes goto[TOS,la_identity] onto the stack, and updates 
la_identity by advancing the lexical analyzer.

reduce_y  A reduction processes production Y : X -> x_1...x_n, which 
requires popping n states off the stack, followed by pushing goto[TOS, X]. 
(The semantic action of the parser relating to this production would be 
executed prior to popping states off the stack.)

accept  An accept signals a successful parse.

error  An error requires error reporting and/or recovery.


4 Simple Implementation
mule creates a single parsing routine, yyparse(), that simulates the LALR(1) 
parser directly in ANSI C, without interpreting any tables. The routine has 
five simple parts: initialization, automata states, reduction actions, 
nonterminal transitions, and error recovery. Although very similar to the
inverted table structure in [Pfa90], this structure avoids the duplication 
of semantic action routines. Another diverence is the yacc-compatible error 
recovery. The structure is simple, with all code being generated from a tiny 
set of small, well-defined templates that directly mirror the grammar or 
LALR(1) automaton. Since both the state stack and the semantic stack grow in 
unison, we wrap the stack entries into a single structure, StackType.

4.1 Initialization
The initalization phase simply sets up bookkeeping and data structures for 
subsequent automata simulation. It is grammar-independent.
=end

def init_code
"
#define YYABORT do { \\
  free(start_stack);return -1; \\
  } while(0)
#define YYACCEPT do { \\
  YYSTYPE result=SEMANTIC_STACK; \\
  free(start_stack); \\
  return result; \\
  } while(0)
/*#define yyclearin_token = yylex(&la_token)*/
#define yyerrok yyerrorstatus = 3
#define YYERROR goto user_error_handler
#define YYRECOVERING() (yyerrorstatus <= 2)


typedef VALUE YYSTYPE;

#if 0
typedef struct stackType{
  int state;// State stack element.
} StackType;
typedef struct {
  VALUE semantic;
} SemanticStackType;
#else
typedef int StackType;
typedef VALUE SemanticStackType;
#end
int yyparse(void){
  YYSTYPE la_token;// Semantic value computed by yylex().
  int la_identity;
  unsigned yyerrorstatus = 3;// Initialize error-recovery counter.
  YYSTYPE yyredval;// Variable holds semantic value of$$.
  VALUE semantic_stack; /*Array of Node|Token*/
//  SemanticStackType *semantic_stack_start;
  StackType *stack_start;// Stack.
  unsigned i=0;
  unsigned stack_size=64;
  
  stack_start=realloc(NULL,sizeof(StackType)*stack_size); 
  if (stack_start==NULL) MALLOC_ERROR(); 
  semantic_stack=rb_ary_new();
//  semantic_stack_start=realloc(NULL,sizeof(SemanticStackType)*stack_size); 
//  if (semantic_stack_start==NULL) MALLOC_ERROR(); 

  la_identity = yylex(&la_token); /* Get 1st token.*/

  goto shift_state_#{str2cname all_states.first.name};/* Start state.*/
"
end


def state_utils
"
#define MALLOC_ERROR() huh
#define RESERVE_STACK_SLOT() \\
  if (++i >= stack_size){ \\
    unsigned new_stack_size=stack_size*2; \\
    stack_start=realloc(stack_start,sizeof(StackType)*new_stack_size); \\
    if (stack_start==NULL) MALLOC_ERROR(); \\
    //semantic_stack_start=realloc(semantic_stack_start,sizeof(SemanticStackType)*new_stack_size); \\
    //if (semantic_stack_start==NULL) MALLOC_ERROR(); \\
    stack_size=new_stack_size; \\
  }

#define GET_TOKEN() \\
  do { \\
    SEMANTIC_STACK_SET(la_token); /*Put lexical semantic entry on stack.*/ \\
    la_identity = yylex(&la_token); /* Advance lexical analysis.*/ \\
    yyerrorstatus++; /* Update error-recovery counter.*/ \\
  } while(0)

#define STACK stack_start[i]
#define SEMANTIC_STACK rb_ary_get(semantic_stack,rb_int2fixnum(i))
#define SEMANTIC_STACK_SET(x) rb_ary_set(semantic_stack,rb_int2fixnum(i),x)
#define OLDSTACK stack_start[i-1]
"
end

def action2c(action)
             case action
             when Rule; "goto reduce_#{str2cname action.name};"
             when nil,:error;  "goto error_handler;"
             when ParserState;  "goto shift_state_#{str2cname action.name};"
             when :accept; "YYACCEPT;"
             when MultiReduce; action.action2c
             when MultiShift; action.action2c
#             when StackMonkey; action.action2c
             else fail "unexpected action type: #{action.class} = #{action}"
             end
end

=begin
4.2 Hard-coded States
For each automata state, mule creates code responsible for simulating the action of that state based
on the current input token. All transitions
into
a given state are labeled with the same grammar
symbol. States labeled with a token are called
shift states
and they require extra code to advance
the lexical analyzer. The template of this code for state N is
=end

def state(state_n,n)
#n=state_n.small_int
name=state_n.name
"
shift_state_#{name}: 
  GET_TOKEN();  /*modifies token, la_token*/
state_#{name}:  /*state_#{n}:*/
  STACK = #{n};

  RESERVE_STACK_SLOT();
state_action_#{name}: /* Error-recovery entry point.*/
/*state_action_#{n}:*/
  switch (la_identity){
    #{state_n.actions.map do |tok,action|
        %[  case #{str2cname(tok)}: #{action2c action}]
      end.join(%[\n])
    }
    default: #{action2c state_n.actions.default}
  }
"
rescue Exception=>e
  backtrace.unshift("exception in state #{name} #{e.class}:#{e}").join("\n")
end

=begin
The state number is stored in the stack, followed by possibly invoking the lexical analyzer. The
three optional lines store the semantic value of the current token, advance the lexical analyzer, and
do error-recovery bookkeeping. Incrementing the stack pointer completes the push. The case arms
of the switch are determined by the
action
table computed by the LALR(1) analysis; for each
condition met in the comments, a case arm must be generated. Default actions were developed for
compressing table-driven parsers, and can be similarly employed here for generating the switchs
default [FL88].

4.3 Reduction Actions
One piece of code is generated for each production. Its template is given below.
=end

def repl(rule,m)
  repl=rule.replacement
  case repl
  when :shift,:accept #do nothing?
  when Class
    %[static VALUE repl_#{rule.name}=rb_const_lookup(rb_const_lookup(kNIL,"RedParse"),"#{repl.name});\n]
  when StackMonkey
    huh
  else
    huh
  end
end

def reduce(rule,m)
  repl=rule.replacement
  pattern=rule.pattern
  numpatterns=pattern.subregs.size

  leader="\nreduce_#{rule.name}:\n/*reduce_#{m}:*/ /*Production #{m}: #{rule.to_s} */\n"
  if StackMonkey===repl
    userhook="
      huh rb_proc_call(stack_monkey_#{rule.name},semantic_stack);
      huh /*objects removed from semantic stack get pushed back onto lexer input*/
      i -= #{repl.backup_count};
      
      goto nonterminal_switcher_#{str2cname repl.huh}; 
      // Compute transition on some old node from the (semantic) stack.
      huh//need a node (or token?) number to goto address mapper (switch stmt) here    
    "
  elsif :accept==repl
    return leader+"YYACCEPT;"
  end
  %[
    #{leader}#{userhook}
    yyredval=huh rb_call(repl_#{rule.name},huh("create"),semantic_stack,huh2ruby(-#{numpatterns}));

    i -= #{numpatterns}; /* Pop patterns from stack.*/
  
    SEMANTIC_STACK_SET(yyredval); /* Copy ($$) onto semantic stack.*/
    goto nonterminal_#{str2cname repl.name}; /* Compute transition on produced node type.*/
  ]
  #end

rescue Exception=>e
  backtrace.unshift("exception in reduce of rule #{rule.name} #{e.class}:#{e}").join("\n")
end

=begin
User actions are associated with reductions, and the code corresponding to a given production
is expanded in-place. After the user code, the symbols associated with right-hand side of the
production are popped, followed by copying $$ onto the semantic stack. Finally, there is a jump to
the code that will compute the appropriate state given the left-hand side symbol of this production.

4.4 Nonterminal Transitions
For each nonterminal, code is produced to compute (and jump to) the appropriate state given the
current state. This simple switch statement is given below.
=end

def nonterminal(j)
"
nonterminal_#{str2cname j.name}:  /*nonterminal_#{j.small_int}:*/
  switch (OLDSTACK){   // Top of stack.
    #{
      all_states.map_with_index do|state,k|
        %[  case #{k}: goto state_#{str2cname state.goto[j].name};\n]
      end
    }
  }
"
rescue Exception=>e
  backtrace.unshift("exception in node(nonterminal) #{j.name} #{e.class}:#{e}").join("\n")
end

=begin
The case arms of the switch statement are taken directly from the goto table 
that was computed by the LALR(1) grammar analysis. Because this switch cannot 
fail, no default entry is needed. However, making the most common case arm the 
default is a trivial time and space optimization.
=end

def generate_c output
  output<< init_code
  output<< state_utils
  (0...RULES().size).each_with_index{|i,m| output<< (reduce i,m) }
  node_types.each{|nt| output<< (nonterminal nt) }
  map_with_index(all_states){|st,i| output<< (state st,i) }
  #output<< error_handler  #disabled, i have rules for error recovery
  output<< "}"
end




=begin
4.5 Error Recovery
yacc's error recovery mechanism is rather idiosyncratic. In fact, examining two books, [LMB92]
and [ASU86], and the output generated by yacc yields three dierent descriptions of the recovery
mechanism. We have tried to be faithful to the output of yacc.
Fortunately, the mechanism has few consequences to the generation of the rest of the hard-
coded parser. The only change to the parser is the maintenance of the variable, yyerrorstatus.
Although relatively short, the code below is very subtle, like the explanation of yacc's error
recovery mechanism. The code is given only for completeness.
=end

def error_handler
%[
error_handler:
  if (yyerrorstatus > 2){
    yyerror("syntax error");
  }
user_error_handler:
  if (yyerrorstatus == 0){
    huh if (la_identity == 0) YYABORT;// End of input.
    la_identity = yylex(&la_token);
    switch (OLDSTACK){
    #{@states.map{|state| 
        i=state.small_int
        "case #{i}: goto state_action_#{str2cname state.name};\n"
      }
    }
  }else{
    yyerrorstatus = 0;
    while (stack != stack_start){
      switch (OLDSTACK){
      case N: goto state_M;// iff M = goto[N,error].
      .
      .
      .
      }
      stack--;
    }
    YYABORT;// Empty stack.
  }
]
end


  CHARMAPPINGS={
  ?`=>'bquote',  ?~=>'tilde',  ?!=>'bang',    ?@=>'at',
  ?#=>'num',     ?$=>'dollar', ?%=>'percent', ?^=>'caret',
  ?&=>'and',     ?*=>'star',   ?(=>'lparen',  ?)=>'rparen',
  ?-=>'minus',   ?+=>'plus',   ?==>'equals',
  ?{=>'lbrace',  ?}=>'rbrace', ?[=>'lbrack',  ?]=>'rbrack',
  ?|=>'or',      ?\\=>'bslash',?:=>'colon',   ?;=>'semicolon',
  ?"=>'dquote',  ?'=>'squote', ?,=>'comma',   ?.=>'dot',
  ?<=>'less',    ?>=>'more',   ??=>'q',       ?/=>'y',
  ?\s=>'space',
  ?X=>'x',
  }
  STRMAPPINGS={
    '::'=>"XX",
    '++'=>"Xeval",
    '--'=>"Xsingleton",
    '[]'=>"Xbrackets",
    '->'=>"Xcalling",
  }
  STRMAP_REX=/#{STRMAPPINGS.keys.map{|x| Regexp.quote x}.join "|"}/
  def self.str2cname str
    str.gsub(STRMAP_REX){|str| STRMAPPINGS[str] } \
       .gsub(/(?!#{LETTER_DIGIT}).|[X]/o){|ch| 
         "X"+  esc=CHARMAPPINGS[ch[0]] ? esc : ch[0].to_s(16)
       } 
  end
  def str2cname(str) RedParse.str2cname(str) end


=begin
The case arms are the only part of the code that depends on the automaton. Any state that has
an outgoing transition on yacc's special error symbol will have a case arm in the second switch
statement.

Our error recovery implementation assumes that at most
MAX UINT-3
tokens will ever be shifted between syntactic
errors | given that this number is 4,294,967,293 on a 32-bit machine, we feel this is safe. Even then, the mechanism
is only flawed for the last 3 tokens out of every
MAX UINT
tokens, and furthermore, the assumption cannot disturb
a correct parse, only error-recovery processing. Making it completely safe would be trivial, but would require a
conditional increment at each shift, which we consider too costly for the benefit.
=end
end
