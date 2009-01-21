# modified from http://21ccw.blogspot.com/2008/04/using-rake-for-erlang-unit-testing.html
require 'rake/clean'
require 'pp'

INCLUDE    = File.dirname(__FILE__) + "/include"
ERLC_FLAGS = "-I#{INCLUDE} +warn_unused_vars +warn_unused_import"

SRC        = FileList['src/*.erl']
SRC_OBJ    = SRC.pathmap("%{src,ebin}X.beam")

TEST       = FileList['test/src/*.erl']
TEST_OBJ   = TEST.pathmap("%{src,ebin}X.beam")

CLEAN.include("ebin/*.beam", "test/ebin/*.beam")

directory 'ebin'
directory 'test/ebin'

rule( ".beam" => ["%{ebin,src}X.erl"] ) do |t|
  testing  = t.source =~ /test\// ? true : false
  eunit    = testing ? "-D EUNIT "  : ""
  ebin_dir = testing ? "test/ebin"  : "ebin"
  cmd = "erlc #{eunit}-pa ebin -W #{ERLC_FLAGS} -o #{ebin_dir} #{t.source}"
  puts cmd
  sh cmd
end

desc "Compile everything"
task :compile   => ["src:compile", "test:compile"]
task :recompile => ["clean", "src:compile", "test:compile"]

namespace :src do
  desc "Compile src"
  task :compile => ['ebin'] + SRC_OBJ
end

namespace :test do
  desc "Compile tests"
  task :compile => ['test/ebin'] + TEST_OBJ
end

desc "Run all tests"
task :run_tests => :compile do
  puts "Modules under test:"
  TEST_OBJ.each do |obj|
    obj[%r{.*/(.*).beam}]
    mod = $1
    test_cmd = "erl -pa ebin -pa test/ebin -run #{mod} test -run init stop"
    puts test_cmd
    test_output = `#{test_cmd}`
    
    puts test_output if Rake.application.options.trace

    if /\*failed\*/ =~ test_output
      test_output[/(Failed.*Aborted.*Skipped.*Succeeded.*$)/]
    else
      test_output[/1>\s*(.*)\n/]
    end

    puts "#{mod}: #{$1}"
  end
end
