require 'redparse'

class ParseTree
  def initialize

  end

  def parse_tree_for_string(source,
                            filename = '(string)', line = 1, verbose = true)
    @parser=RedParse.new(source,filename,line)
    @parser.parse.to_parsetree
  end
end
