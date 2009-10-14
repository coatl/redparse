=begin           
    redparse - a ruby parser written in ruby
    Copyright (C) 2008  Caleb Clausen

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
=end


=begin 
rp-locatetest passes all the ruby files it can find on the system (using 
the unix command locate) to bin/redparse --vsparsetree. In other words, 
it's comparing the output of RedParse (when cvt'd to a ParseTree-style 
tree) to the actual output of ParseTree.

I usually run it like this:

  nice ruby test/rp-locatetest.rb >test.log 2>&1 &

That runs rp-locatetest at low priority, in the background, saving 
stdout and stderr to test.log.

You can then trawl through test.log, looking for lines that don't start 
with 'no differences in '. (Other than the page of warnings in the 
beginning, which is repeated every so often.) Or, you can look for 
non-commented lines in the file 'problemfiles'. (Which is just a list of 
files that caused failures in rp-locatetest.)

Ok, this is more complicated than it ought to be. Sorry.

rp-locatetest takes a LONG time to run, and hogs the cpu at 100%. That's 
why I was using nice. So, make sure it's ok to do that on whatever 
server you run it on. I always get impatient and kill it after some 
hours or days. On a subsequent run it'll pick up again where it left 
off.

Right now, about 0.5% of my ruby files cause problems for rp-locatetest.  
Most of those failures correspond to known problems (the file is listed 
at the end of the README and/or there's a case in test_redparse.rb for 
that failure).
=end

$VERBOSE=1
$Debug=1

#require 'test/code/rubylexervsruby'
#require 'test/code/strgen'
require "redparse/problemfiles"

class RedParse
module LocateTest
begin
  require 'rubygems'
  require 'rubylexer'
  require 'test/code/locatetest'
  RL=RubyLexer::LocateTest
rescue Exception:
  RL=Module.new
  def RL.const_missing(k) [] end
end

#ENV['RUBY']||='ruby'
$RUBY=ENV['RUBY']||'ruby'
#test $RUBY || export RUBY=ruby

#$RUBYLEXERVSRUBY="#$RUBY test/code/rubylexervsruby.rb"

RUBY_VERSION[/^1\.8\.6/] or raise 'need ruby>= 1.8.6'



#if RUBY_VERSION --version|grep '^ruby 1\.6'; then
#  echo 'error: need ruby 1.8'; exit
#fi


RLROOT= (File.dirname $0)+'/../rubylexer'
#cd `dirname -- $0`

=begin if locate fails, we should use the algorithm from this sh code

#also look in bin and lib directories
file -L `echo $PATH":/sbin:/usr/sbin"|tr : "\n"|sort -u|xargs -i echo "{}/*"`| \
 grep "ruby[^:]*script"|cut -d: -f1 > test/results/rubyexelibs

ruby -e 'print ($:.sort.uniq+[""]).join"\n"'|xargs -i ls "{}/*.rb" >> test/results/rubyexelibs

   for i in `cat test/results/rubyexelibs`; do
      $RUBYLEXERVSRUBY $i;
   done

=end

puts "hack hack hack: absolute path in ruby test file list"
puts "hack hack hack: skipping japanese/zipcodes.rb; too big!"
TEST_THESE_FIRST= DATA.read.split.map{|n| n[1..-1]}

[
  "/home/caleb/rubies/ruby-1.8.7/instruby.rb",
  "/home/caleb/sandbox/rubylexer/test/data/oneliners.rb",
  "/home/caleb/sandbox/rubylexer/test/data/heremonsters_dos.rb",
  "/home/caleb/sandbox/rubylexer/test/data/whatnot.rb",
  "/home/caleb/sandbox/rubylexer/test/data/pre.rb",
  "/home/caleb/sandbox/rubylexer/test/data/stanzas.rb",
  "/home/caleb/sandbox/rubylexer/test/data/ws_strdelim.rb",
  "/home/caleb/sandbox/rubylexer/test/data/unending_stuff.rb",
  "/home/caleb/sandbox/rubylexer/test/data/stuff2.rb",
  "/home/caleb/sandbox/rubylexer/test/data/strdelim_crlf.rb",
  "/home/caleb/sandbox/rubylexer/jewels/installpkg-0.0.1/lib/.install-pkg.rb.swp",
  "/var/lib/gems/1.8/gems/rubylexer-0.6.2/testdata/p.rb",

  "/home/caleb/sandbox/rubylexer/jewels/samizdat-0.6.1/samizdat/lib/samizdat/storage.rb",
#  "/home/caleb/sandbox/rubylexer/jewels/math3d-0.04/tests/make_tests.rb",
  "/home/caleb/sandbox/rubylexer/jewels/QuickBaseClient.rb/quickbasecontactsappbuilder.rb",
#  "/home/caleb/sandbox/rubylexer/jewels/QuickBaseClient.rb/quickbaseclient.rb",
  "/home/caleb/sandbox/rubylexer/jewels/ruby-ivy_0.1.0/ruby-ivy/examples/._000-IVYTranslater.rb",
  "/home/caleb/sandbox/rubylexer/jewels/ruby-ivy_0.1.0/ruby-ivy/examples/._002-ApplicationList.rb",
  "/home/caleb/sandbox/rubylexer/jewels/ruby-ivy_0.1.0/ruby-ivy/examples/._001-UnBind.rb",
  "/home/caleb/sandbox/rubylexer/jewels/ruby-ivy_0.1.0/ruby-ivy/._extconf.rb",
#  "/home/caleb/sandbox/rubylexer/jewels/smf-0.15.10/sample/virtual-samp.rb",
#  "/home/caleb/sandbox/rubylexer/jewels/syntax/syntax.rb",
#  "/home/caleb/sandbox/rubylexer/jewels/rex-1.0rc1/rex/packages/rex/test/rex-20060511.rb",
#  "/home/caleb/sandbox/rubylexer/jewels/rex-1.0rc1/rex/packages/rex/test/rex-20060125.rb",
#  "/home/caleb/sandbox/rubylexer/jewels/japanese-zipcodes-0.0.20080227/lib/japanese/zipcodes.rb", #too big!!!!

#  "/home/caleb/sandbox/rubylexer/jewels/ruby-asp_0.4.1/ruby-asp/lib/asp/params.rb",
#  "/home/caleb/sandbox/rubylexer/jewels/ruby-asp_0.4.1/ruby-asp/bin/ruby-asp.rb",
  "/home/caleb/sandbox/rubylexer/jewels/ruby-asp_0.4.1/ruby-asp/setup.rb",
#  "/home/caleb/sandbox/rubylexer/jewels/ruby-asp_0.4.1/ruby-asp/lib/asp/goto.rb",
#  "/home/caleb/sandbox/rubylexer/jewels/ruby-asp_0.4.1/ruby-asp/lib/asp/globalasa.rb",
  "/home/caleb/sandbox/rubylexer/jewels/ruby-asp_0.4.1/ruby-asp/lib/apache/aspruby-run.rb",
  "/home/caleb/sandbox/rubylexer/jewels/osc-0.1.3/install.rb",
  "/home/caleb/sandbox/rubylexer/jewels/osc-0.1.3/install.rb",
#  "/home/caleb/sandbox/rubylexer/jewels/gallery-0.0.8/interface/admin.rb",
#  "/home/caleb/sandbox/rubylexer/jewels/gallery-0.0.8/lib/gallery/base-administrator.rb",
#  "/home/caleb/sandbox/rubylexer/jewels/gallery-0.0.8/lib/rubyphoto/imlib.rb",
#  "/home/caleb/sandbox/rubylexer/jewels/gallery-0.0.8/lib/rubyphoto/album.rb",
  "/home/caleb/sandbox/rubylexer/jewels/strscan-0.6.7/install.rb",
  "/home/caleb/sandbox/rubylexer/jewels/opml-0.0.20060519/setup.rb",
#  "/home/caleb/sandbox/rubylexer/jewels/diakonos-0.8.4/lib/diakonos.rb",
#  "/home/caleb/sandbox/rubylexer/jewels/diakonos-0.8.4/lib/diakonos/buffer.rb",
#  "/home/caleb/sandbox/rubylexer/jewels/tengen-0.1.0/lib/tengen/ui/main_app.rb",
  "/home/caleb/sandbox/rubylexer/jewels/rwiki-2.1.1/interface/rw-mail.rb",
  "/home/caleb/sandbox/rubylexer/jewels/rwiki-2.1.1/installer/rwiki-installer.rb",
#  "/home/caleb/sandbox/rubylexer/jewels/rwiki-2.1.1/lib/rwiki/db/cvs.rb",
#  "/home/caleb/sandbox/rubylexer/jewels/rwiki-2.1.1/lib/rwiki/db/svn.rb",
  "/home/caleb/sandbox/rubylexer/jewels/rwiki-2.1.1/lib/rwiki/hotpage.rb",
#  "/home/caleb/sandbox/rubylexer/jewels/rwiki-2.1.1/site/ruby-lang/migrate.rb",
#  "/home/caleb/sandbox/rubylexer/jewels/yajb-0.8.1/src/ruby/bstream.rb",
#  "/home/caleb/sandbox/rubylexer/jewels/yajb-0.8.1/src/ruby/jlambda.rb",
#  "/home/caleb/sandbox/rubylexer/jewels/yajb-0.8.1/src/ruby/comm_xmlrpc.rb",
  "/home/caleb/sandbox/rubylexer/jewels/yajb-0.8.1/lib/yajb/jbridge.rb",
]
pf=ProblemFiles.new
RUBYLIST=(pf.badlist+
  TEST_THESE_FIRST+RL::TEST_THESE_FIRST+
  RL::RUBYBINS+
[ RLROOT+"/test/data/p.rb", 
  *Dir["test/data/*.rb"]+
  `(
    locate /inline.rb /tk.rb;
    locate examples/examples_test.rb;
    locate .rb; 
    locate rakefile; locate Rakefile; locate RAKEFILE; 
  )|egrep -v '(/test/(results|data)/|.broken$)'`.
    split("\n")
].sort_by{rand})-pf.goodlist + pf.goodlist
RUBYLIST.reject!{|x| %r{japanese/zipcodes\.rb$}===x } #65M of ruby src!!!
RUBYLIST.reject!{|x| /\A\s*\Z/===x }
RUBYLIST.uniq!

def self.main
testdata=RUBYLIST.dup
until testdata.empty?
  chunk=testdata.slice!(0...20)

  chunk=chunk.select{|fn| system "#$RUBY -c #{fn} >/dev/null 2>&1" }

#  puts "testing: "+chunk.join(" ")
  chunk.empty? and next
  system $RUBY, "-Ilib", "bin/redparse", "--update-problemfiles", "--ignore-silly-begins", "--unparse", 
    "-q", "--vsparsetree", *chunk
  exit 2 if $?>>8 == 2  #exit if child got ^c
end
end

#for i in test/data/p.rb `(locate /tk.rb;locate examples/examples_test.rb;#locate .rb; locate rakefile; locate Rakefile; locate RAKEFILE)|egrep -v '/#test/results/'`; do
#  $RUBYLEXERVSRUBY $i
#done
end
end

RedParse::LocateTest.main if $0==__FILE__
__END__
/home/caleb/sandbox/rubylexer/test/data/heremonsters_dos.rb
/home/caleb/sandbox/rubylexer/jewels/QuickBaseClient.rb/quickbasecontactsappbuilder.rb
/home/caleb/rubies/186/lib/ruby/1.8/ftools.rb
/home/caleb/rubies/186/lib/ruby/1.8/rdoc/generators/template/html/html.rb
/home/caleb/rubies/186/lib/ruby/1.8/yaml/types.rb
/home/caleb/rubies/186/lib/ruby/gems/1.8/gems/ParseTree-2.1.1/lib/parse_tree.rb
/home/caleb/rubies/186/lib/ruby/gems/1.8/gems/cursor-0.9/duck.rb
/home/caleb/rubies/186/lib/ruby/site_ruby/1.8/rubygems/source_index.rb
/home/caleb/rubies/187/lib/ruby/1.8/ftools.rb
/home/caleb/rubies/187/lib/ruby/1.8/rdoc/generators/template/html/html.rb
/home/caleb/rubies/187/lib/ruby/1.8/yaml/types.rb
/home/caleb/rubies/187/lib/ruby/gems/1.8/gems/ParseTree-2.2.0/lib/parse_tree.rb
/home/caleb/rubies/187/lib/ruby/gems/1.8/gems/cursor-0.9/duck.rb
/home/caleb/rubies/187/lib/ruby/site_ruby/1.8/rubygems/source_index.rb
/home/caleb/rubies/190/lib/ruby/1.9.0/rdoc/generators/template/html/html.rb
/home/caleb/rubies/190/lib/ruby/1.9.0/rubygems/source_index.rb
/home/caleb/rubies/190/lib/ruby/1.9.0/yaml/types.rb
/home/caleb/rubies/190/lib/ruby/gems/1.9.0/gems/ParseTree-2.1.1/lib/parse_tree.rb
/home/caleb/rubies/190/lib/ruby/site_ruby/1.9.0/rubygems/source_index.rb
/home/caleb/rubies/ruby-1.8.6/lib/ftools.rb
/home/caleb/rubies/ruby-1.8.6/lib/rdoc/generators/template/html/html.rb
/home/caleb/rubies/ruby-1.8.6/lib/yaml/types.rb
/home/caleb/rubies/ruby-1.8.6/test/fileutils/test_fileutils.rb
/home/caleb/rubies/ruby-1.8.7/lib/ftools.rb
/home/caleb/rubies/ruby-1.8.7/lib/rdoc/generators/template/html/html.rb
/home/caleb/rubies/ruby-1.8.7/lib/yaml/types.rb
/home/caleb/rubies/ruby-1.8.7/sample/test.rb
/home/caleb/rubies/ruby-1.8.7/test/fileutils/test_fileutils.rb
/home/caleb/rubies/ruby-1.8.7/test/ruby/test_proc.rb
/home/caleb/rubies/ruby-1.8.7-preview2/lib/ftools.rb
/home/caleb/rubies/ruby-1.8.7-preview2/lib/rdoc/generators/template/html/html.rb
/home/caleb/rubies/ruby-1.8.7-preview2/lib/yaml/types.rb
/home/caleb/rubies/ruby-1.8.7-preview2/test/fileutils/test_fileutils.rb
/home/caleb/rubies/ruby-1.9.0-0/lib/rdoc/generators/template/html/html.rb
/home/caleb/rubies/ruby-1.9.0-0/lib/rubygems/source_index.rb
/home/caleb/rubies/ruby-1.9.0-0/lib/yaml/types.rb
/home/caleb/sandbox/rubylexer/jewels/alib-0.5.1/lib/alib-0.5.1/util.rb
/home/caleb/sandbox/rubylexer/jewels/stick-1.3.3/test/test_matrix.rb
/home/caleb/sandbox/rubylexer/jewels/rwdtinker-1.67/ev/rwd.rb
/home/caleb/sandbox/rubylexer/jewels/rubyslippers-1.03/extras/rconftool.rb
/home/caleb/rubies/ruby-1.9.0-0/test/fileutils/test_fileutils.rb
/home/caleb/rubies/ruby-1.9.0-0/test/rubygems/test_gem_commands_cert_command.rb
/home/caleb/sandbox/rubylexer/jewels/rubysync-0.2.1/lib/ruby_sync/connectors/base_connector.rb
/home/caleb/sandbox/rubylexer/jewels/rubyslippers-1.03/ev/rwd.rb
/home/caleb/sandbox/rubylexer/jewels/karmasphere-client-0.6.4/lib/karmasphere/query.rb
/home/caleb/sandbox/rubylexer/jewels/roby-0.7.2/test/test_transactions.rb
/home/caleb/sandbox/rubylexer/jewels/rwddemo-0.92/extras/rconftool.rb
/home/caleb/sandbox/rubylexer/jewels/ambition-0.5.2/app_generators/ambition_adapter/ambition_adapter_generator.rb
/usr/lib/ruby/1.8/tk/canvastag.rb
/home/caleb/rubies/rubygems-1.1.1/test/test_gem_commands_cert_command.rb
/home/caleb/sandbox/rubylexer/jewels/udat-1.4.1/lib/udat.rb
/home/caleb/sandbox/rubylexer/jewels/rmail-1.0.0/test/testmessage.rb
/home/caleb/sandbox/rubylexer/jewels/camping-1.5/examples/campsh.rb
/home/caleb/sandbox/rubylexer/jewels/clickatell-0.4.1/lib/clickatell.rb
/home/caleb/sandbox/rubylexer/jewels/rfeedparser-0.9.940/lib/rfeedparser.rb
/home/caleb/sandbox/rubylexer/jewels/passenger-2.0.1/lib/passenger/abstract_server.rb
/home/caleb/sandbox/rubylexer/jewels/rubywebdialogs/rubywebdialogs.rb
/usr/lib/ruby/1.8/qwik/util-css.rb
/home/caleb/sandbox/rubylexer/jewels/aspectr-0.3.7/lib/aspectr.rb
/home/caleb/sandbox/rubylexer/jewels/optiflag-0.6.5/examples/example_2_6.rb
/home/caleb/sandbox/rubylexer/jewels/rwdhypernote-0.10/ev/rwd.rb
/home/caleb/sandbox/rubylexer/jewels/rubywebdialogs-0.2.0/realstuff.rb
/home/caleb/sandbox/rubylexer/jewels/stick-1.3.3/lib/stick/matrix.rb
/usr/share/hiki/hiki/docdiff/charstring.rb
/home/caleb/sandbox/rubylexer/jewels/rb2html-2.4/rb2html/haskell_lexer.rb
/home/caleb/sandbox/rubylexer/jewels/mspire-0.3.9/lib/spec_id/srf.rb
/home/caleb/sandbox/rubylexer/jewels/rb-wartslib-0.9.14/lib/wartslib/wl-file.rb
/home/caleb/sandbox/rubylexer/jewels/sqliki_generator-0.0.4/templates/lib_sanitize_html.rb
/home/caleb/sandbox/rubylexer/jewels/rwdshell-1.00/extras/rconftool.rb
/home/caleb/sandbox/rubylexer/jewels/main-2.8.0/lib/main/base.rb
/home/caleb/sandbox/rubylexer/jewels/aws-s3-0.5.1/lib/aws/s3/base.rb
/usr/lib/ruby/1.8/ftools.rb
/home/caleb/sandbox/rubylexer/jewels/pseudoxml-0.1.0/RAKEFILE
/usr/lib/ruby/1.8/rdoc/generators/template/html/html.rb
/home/caleb/sandbox/rubylexer/jewels/typo-5.0.3.98.1/vendor/plugins/rspec/spec/spec/mocks/mock_spec.rb
/home/caleb/sandbox/rubylexer/jewels/rubytorrent-0.3/lib/rubytorrent/server.rb
/home/caleb/sandbox/rubylexer/jewels/optiflag-0.6.5/testcases/tc_values_as_hash.rb
/home/caleb/sandbox/rubylexer/jewels/optiflag-0.6.5/examples/example_3.rb
/home/caleb/sandbox/rubylexer/jewels/Nephila-0.6.0/config/environment.rb
/usr/share/rails/railties/builtin/rails_info/rails/info.rb
/home/caleb/sandbox/rubylexer/jewels/ruby-rpm-1.2.3/lib/rpm.rb
/home/caleb/sandbox/rubylexer/jewels/optiflag-0.6.5/testcases/tc_bug_one.rb
/home/caleb/sandbox/rubylexer/jewels/optiflag-0.6.5/testcases/tc_basic_char_flags.rb
/usr/lib/ruby/1.8/rant/c/include.rb
/home/caleb/sandbox/rubylexer/jewels/ya2yaml-0.26/lib/ya2yaml.rb
/home/caleb/sandbox/rubylexer/jewels/spree-0.2.0/vendor/rails/activesupport/lib/active_support/multibyte/handlers/utf8_handler.rb
/home/caleb/sandbox/rubylexer/jewels/rq-3.4.0/rails/vendor/rails/actionpack/test/controller/fragment_store_setting_test.rb
/home/caleb/sandbox/rubylexer/jewels/typo-5.0.3.98.1/config/environment.rb
/home/caleb/sandbox/rubylexer/jewels/date4-0.1.27/lib/date4/date.rb
/usr/share/skktools/filters/skkdictools.rb
/home/caleb/sandbox/rubylexer/jewels/aeditor-1.9/lib/aeditor/test_lexer.rb
/home/caleb/sandbox/rubylexer/jewels/optiflag-0.6.5/quick.rb
/home/caleb/sandbox/rubylexer/jewels/rabbit-0.5.6/lib/rabbit/image/eps.rb
/home/caleb/sandbox/rubylexer/jewels/gecoder-with-gecode-0.8.2/lib/gecoder/interface/constraints/set/connection.rb
/home/caleb/sandbox/rubylexer/jewels/onebody-0.3.0/vendor/rails/actionpack/test/controller/caching_test.rb
/home/caleb/sandbox/rubylexer/jewels/rwdhypernote-0.10/extras/rconftool.rb
/home/caleb/sandbox/rubylexer/jewels/Ron-0.1.0/lib/ron.rb
/home/caleb/sandbox/rubylexer/jewels/radiant-0.6.7/vendor/rails/railties/lib/initializer.rb
/usr/share/rails/activesupport/lib/active_support/multibyte/handlers/utf8_handler.rb
/home/caleb/sandbox/rubylexer/jewels/rwdgutenberg-0.07/extras/rconftool.rb
/home/caleb/sandbox/rubylexer/jewels/inline-0.1.0/lib/inline/editor.rb
/home/caleb/sandbox/rubylexer/jewels/xx-2.1.0/lib/xx-2.1.0.rb
/home/caleb/sandbox/rubylexer/jewels/ok-extensions-1.0.15/lib/extensions/fixnum.rb
/home/caleb/sandbox/rubylexer/jewels/aspectr-0-3-5/lib/aspectr.rb
/home/caleb/sandbox/rubylexer/jewels/rubygems-update-1.2.0/lib/rubygems/test_utilities.rb
/home/caleb/sandbox/rubylexer/jewels/whistle-0.1.1/vendor/ruby-feedparser-0.5-stripped/lib/feedparser/sgml-parser.rb
/home/caleb/sandbox/rubylexer/jewels/hpricot-0.6/lib/hpricot/modules.rb
/home/caleb/sandbox/rubylexer/jewels/ruby-web-1.1.1/lib/web/htmlparser/sgml-parser.rb
/usr/lib/ruby/1.8/rant/import/c/dependencies.rb
/home/caleb/sandbox/rubylexer/jewels/dbi-0.1.0/lib/dbd/FrontBase.rb
/home/caleb/sandbox/rubylexer/jewels/live_console-0.1.0/lib/live_console.rb
/home/caleb/sandbox/rubylexer/jewels/html-table-1.3.1/test/tc_header.rb
/home/caleb/sandbox/rubylexer/jewels/optiflag-0.6.5/testcases/tc_enumerated_value_validation.rb
/home/caleb/sandbox/rubylexer/jewels/bigtinker-0.93/ev/rwd.rb
/home/caleb/sandbox/rubylexer/jewels/xmlscan-0.2.3/tests/visitor.rb
/home/caleb/sandbox/rubylexer/jewels/dm-core-0.9.2/lib/dm-core/types/discriminator.rb
/home/caleb/sandbox/rubylexer/jewels/sup-0.5/lib/sup/imap.rb
/home/caleb/sandbox/rubylexer/jewels/optiflag-0.6.5/examples/example_2_2.rb
/home/caleb/sandbox/rubylexer/jewels/authorails-1.0.0/lib/initializer.rb
/usr/lib/ruby/1.9.0/tk/textmark.rb
/var/lib/gems/1.8/gems/Ron-0.1.0/lib/ron.rb
/home/caleb/sandbox/rubylexer/jewels/optiflag-0.6.5/examples/example_2.rb
/home/caleb/sandbox/rubylexer/jewels/allinoneruby-0.2.11/realstuff.rb
/home/caleb/sandbox/rubylexer/jewels/typo-5.0.3.98.1/db/migrate/040_attach_content_to_blog.rb
/home/caleb/sandbox/rubylexer/jewels/bijou-0.1.0/lib/bijou/processor.rb
/home/caleb/sandbox/rubylexer/jewels/rq-3.4.0/lib/rq/lockfile.rb
/usr/lib/ruby/1.9.0/tkextlib/vu/pie.rb
/usr/lib/ruby/1.9.0/optparse.rb
/home/caleb/sandbox/reg/regcompiler.rb
/home/caleb/sandbox/rubylexer/jewels/rubygame-2.3.0/lib/rubygame/rect.rb
/usr/lib/ruby/1.9.0/tk/canvastag.rb
/home/caleb/sandbox/rubylexer/jewels/html-table-1.3.1/test/tc_foot.rb
/home/caleb/sandbox/rubylexer/jewels/optiflag-0.6.5/examples/example_2_3.rb
/home/caleb/sandbox/rubylexer/jewels/html-table-1.3.1/test/tc_caption.rb
/home/caleb/sandbox/rubylexer/jewels/extract-curves-0.1.1/ruby_libs/pav/string/observable.rb
/home/caleb/sandbox/rubylexer/jewels/starling-0.9.3/lib/starling/runner.rb
/usr/lib/ruby/1.9.0/rubygems/source_index.rb
/home/caleb/sandbox/rubylexer/jewels/RbYAML-0.2.0/lib/rbyaml/types.rb
/home/caleb/sandbox/rubylexer/jewels/rio-0.4.1/doc/generators/template/html/rio.rb
/home/caleb/sandbox/rubylexer/jewels/rwdaddresses-1.03/extras/vpim/rrule.rb
/usr/lib/ruby/1.8/feed2imap/sgml-parser.rb
/usr/lib/ruby/1.8/htree/modules.rb
/home/caleb/sandbox/rubylexer/jewels/Dnsruby-1.0/test/tc_tsig.rb
/usr/lib/ruby/1.8/yaml/types.rb
/home/caleb/sandbox/rubylexer/jewels/rubyscript2exe/rubyscript2exe.rb
/home/caleb/sandbox/rubylexer/jewels/rfeedparser-ictv-0.9.931/lib/rfeedparser.rb
/usr/lib/ruby/1.8/amazon.rb
/home/caleb/sandbox/redparse/redparse.rb
/home/caleb/sandbox/rubylexer/jewels/rq-3.4.0/rails/vendor/rails/railties/builtin/rails_info/rails/info.rb
/home/caleb/sandbox/rubylexer/jewels/extract_curves-0.0.1/ruby_libs/pav/string/observable.rb
/home/caleb/sandbox/rubylexer/jewels/simplemapper-0.0.6/lib/simple_mapper/default_plugins/oauth.rb
/usr/share/kagemai/lib/xmlscan/parser.rb
/home/caleb/sandbox/rubylexer/jewels/text-hyphen-1.0.0/lib/text/hyphen/language/id.rb
/home/caleb/sandbox/rubylexer/jewels/rant-0.5.7/lib/rant/c/include.rb
/home/caleb/sandbox/rubylexer/jewels/cursor-0.9/duck.rb
/home/caleb/sandbox/rubylexer/jewels/ruby-nxt-0.8.1/examples/commands.rb
/var/lib/gems/1.8/gems/cursor-0.9/duck.rb
/home/caleb/sandbox/rubylexer/jewels/voruby-1.1.1/lib/voruby/simple/parameters.rb
/home/caleb/sandbox/rubylexer/jewels/Dnsruby-1.0/lib/Dnsruby/resource/A.rb
/home/caleb/sandbox/rubylexer/jewels/catori-0.2.6/lib/taglib.rb
/home/caleb/sandbox/rubylexer/jewels/gettext-1.91.0/test/test_rails_caching.rb
/home/caleb/sandbox/rubylexer/jewels/rwdschedule-1.02/extras/rconftool.rb
/home/caleb/sandbox/rubylexer/jewels/spree-0.2.0/vendor/rails/actionpack/lib/action_controller/request.rb
/home/caleb/sandbox/rubylexer/jewels/rubum-0.01/rubum_standalone/ruby/rubum/rdf/lib/.svn/text-base/basicrdf.rb.svn-base
/home/caleb/sandbox/rubylexer/jewels/rabbit-0.5.6/lib/rabbit/image/base.rb
/usr/lib/ruby/1.9.0/resolv.rb
/home/caleb/sandbox/rubylexer/jewels/sqliki-0.0.4/templates/lib_sanitize_html.rb
/home/caleb/sandbox/rubylexer/jewels/whistle-0.1.1/lib/resource.rb
/home/caleb/sandbox/rubylexer/jewels/onebody-0.3.0/vendor/rails/actionpack/lib/action_controller/routing/route.rb
/home/caleb/sandbox/rubylexer/jewels/html-table-1.3.1/examples/intermediate2.rb
/usr/share/hiki/hiki/plugin.rb
/home/caleb/sandbox/rubylexer/jewels/xmlparser-0.6.8/xmlparser/samples/digesttest.rb
/home/caleb/sandbox/rubylexer/jewels/puppet-0.24.4/lib/puppet/file_serving/indirection_hooks.rb
/home/caleb/sandbox/rubylexer/jewels/assert_xpath-0.4.2/lib/assert_xpath.rb~
/home/caleb/sandbox/rubylexer/jewels/basset-1.0.1/lib/basset/classification_evaluator.rb
/usr/lib/ruby/1.8/hpricot/traverse.rb
/home/caleb/sandbox/rubylexer/jewels/net-netrc-0.2.1/lib/net/netrc.rb
/home/caleb/sandbox/rubylexer/jewels/etc-0.2.0/lib/etc.rb


