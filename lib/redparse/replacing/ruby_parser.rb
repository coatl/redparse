class RubyParser
  def initialize

  end

  def parse code,file="(eval)",line=1
    huh #should translate to unified format here too
    @parser=RedParse.new(code,file,line)
    @parser.parse.to_parsetree
  end
end
