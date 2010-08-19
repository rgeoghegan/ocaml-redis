# Copyright (C) 2009 Rory Geoghegan - r.geoghegan@gmail.com
# Released under the BSD license. See the LICENSE.txt file for more info.

desc "Compile the library"
task :library

desc "Create the binary used for testing"
task :test_binaries

desc "Run unit tests"
task :test => [:test_binaries] do
    sh "./build/tests/run_test_redis_util"
    sh "./build/tests/all_other_tests"
end

desc "Run smoke test"
task :smoke_test => ["build/tests/smoke_test"] do
    sh "./build/tests/smoke_test"
end

desc "Delete droppings"
task :clean do
    rm_rf "build"
end

desc "Start a Redis Server"
task :start_redis => "/tmp/redis" do
    sh "redis-server redis.conf"
end

desc "Build html documentation"
task :docs => "doc/index.html"

directory "build"
directory "/tmp/redis"

# Utility code

class OcamlFile < String
    def with_prefix_and_extension prefix, extension
        if prefix != "" then
            prefix = "#{prefix}/"
        end
        pathmap("#{prefix}%f").ext(extension)
    end
    def cmx prefix="build"
        with_prefix_and_extension prefix, "cmx"
    end
    def cma prefix="build"
        with_prefix_and_extension prefix, "cma"
    end
    def mli prefix="src"
        with_prefix_and_extension prefix, "mli"
    end
    def cmi prefix="build"
        with_prefix_and_extension prefix, "cmi"
    end
    def ml prefix="src"
        with_prefix_and_extension prefix, "ml"
    end
    def dest prefix="build"
        pathmap("#{prefix}/%n") # remove ext
    end
    def o prefix="build"
        with_prefix_and_extension prefix, "cmx"
    end
end
    
def compile source, dest
    sh "ocamlopt -w X -c -I build -o #{dest} #{source}"
end

# Library

external_libs = ["unix.cmxa", "str.cmxa"].join(" ")

file "build/redis.cmx" => ["src/redis.ml", "build/redis_common.cmx", "build/redis.cmi"] do
    compile "src/redis.ml", "build/redis"
end

file "build/redis_common.cmx" => "src/redis_common.ml" do
    compile "src/redis_common.ml", "build/redis_common"
end
file "build/redis.cmi" => "src/redis.mli" do
    compile "src/redis.mli", "build/redis.cmi"
end

task :library => "build/redis.cmx"

# Test files

script = OcamlFile.new("tests/script.ml")
directory "build/tests"

# Redis_utils is a bit funny because we need to test more functions than the default mli files gives us access to
test_objects = "build/tests/test_objects"
directory test_objects
test_redis_util = OcamlFile.new("tests/test_redis_util.ml")

file "build/tests/run_test_redis_util" => ["build/tests/script.cmx", test_objects, "build/tests/test_redis_util.cmx", "#{test_objects}/redis_common.cmx", "build/tests/run_test_redis_util.ml"] do
    sh %W{
        ocamlopt
        -o build/tests/run_test_redis_util
        -I build/tests
        #{external_libs}
        build/tests/script.cmx
        #{test_objects}/redis_common.cmx
        build/tests/test_redis_util.cmx
        build/tests/run_test_redis_util.ml
    }.join(" ")
end
file "build/tests/test_redis_util.cmx" => ["tests/test_redis_util.ml", "build/tests/script.cmx", "#{test_objects}/redis_common.cmx"] do
    sh %W{
        ocamlopt -c
        -o build/tests/test_redis_util
        -I build/tests
        -I build/tests/test_objects
        tests/test_redis_util.ml
    }.join(" ")
end
file "#{test_objects}/redis_common.cmx" => "#{test_objects}/redis_common.ml" do
    compile "#{test_objects}/redis_common.ml", "#{test_objects}/redis_common"
end
file "#{test_objects}/redis_common.ml" => "src/redis_common.ml" do
    cp "src/redis_common.ml", "#{test_objects}/redis_common.ml"
end
file "build/tests/run_test_redis_util.ml" => "tests/test_redis_util.ml" do
    sh "ruby tests/create_test.rb tests/test_redis_util.ml > build/tests/run_test_redis_util.ml"
end

task :test_binaries => "build/tests/run_test_redis_util"

## Now for all other tests
#all_other_tests = OcamlFile.new("build/tests/all_other_tests.ml")
test_files = FileList["tests/test_*.ml"].exclude(/test_redis_util.ml$/)

test_files.each do |test_file|
    file test_file.pathmap("build/tests/%n.cmx") => [test_file, "build/redis.cmx", "build/tests/script.cmx"] do
        sh %W{
            ocamlopt
            -w X -c
            -I build
            -I build/tests
            -o #{test_file.pathmap("build/tests/%n")}
            #{test_file}
        }.join(" ")
    end
    file "build/tests/all_other_tests" => test_file.pathmap("build/tests/%n.cmx")
    file "build/tests/all_other_tests.ml" => test_file
end

file "build/tests/all_other_tests.ml" do
    sh %W{
        ruby
        tests/create_test.rb
        #{test_files.join(" ")}
        > build/tests/all_other_tests.ml
    }.join(" ")
end

file "build/tests/all_other_tests" => ["build/tests/all_other_tests.ml", :library] do
    sh %W{
        ocamlopt
        -w X
        -I build
        -I build/tests
        -o build/tests/all_other_tests
        #{external_libs}
        build/redis_common.cmx
        build/redis.cmx
        build/tests/script.cmx
        #{test_files.map{|f| f.pathmap("build/tests/%n.cmx")}.join(" ")}
        build/tests/all_other_tests.ml
    }.join(" ")
end

task :test_binaries => "build/tests/all_other_tests"
#test_files.each do |test_file|
#    file test_file.cmx("build/tests") => [test_file, script.cmx("build/tests"), :library] do
#        sh "ocamlopt -w X -c -I build -I build/tests -o #{test_file.cmx("build/tests")} #{test_file}"
#    end
#    multitask :individual_test_objects => test_file.cmx("build/tests")
#end
#
#file all_other_tests.dest("build/tests") => all_other_tests do
#    sh "ocamlopt -o #{all_other_tests.dest("build/tests")} -I build/tests -I build #{external_libs} #{script.cmx("build/tests")} #{source.cmx} #{test_files.map{|f| f.cmx("build/tests")}.join(" ")} #{all_other_tests}"
#end
#file all_other_tests => :individual_test_objects do
#    sh "ruby tests/create_test.rb #{test_files.join(" ")} > #{all_other_tests}"
#end
#
## Smoke test
#file "build/tests/smoke_test" => [:library, "build/tests/smoke_test.cmx", "build/tests"] do
#    sh "ocamlopt -I build -o build/tests/smoke_test #{external_libs} #{source.cmx} build/tests/smoke_test.cmx"
#end
#
#file "build/tests/smoke_test.cmx" => [:library, "tests/smoke_test.ml", "build/tests"] do
#    compile "tests/smoke_test.ml", "build/tests/smoke_test"
#end
#
file "build/tests/script.cmx" => ["build/tests", "tests/script.ml"] do
    sh %W{
        ocamlopt -c
            -o build/tests/script
            tests/script.ml
    }.join(" ")
end

# Documentation
directory "doc"
file "doc/index.html" => ["doc", "src/redis.mli"] do
    sh "ocamldoc -html -d doc src/redis.mli"
end
