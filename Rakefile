# Copyright (C) 2009 Rory Geoghegan - r.geoghegan@gmail.com
# Released under the BSD license. See the LICENSE.txt file for more info.

desc "Compile the library"
task :library

desc "Create the binary used for testing"
task :test_binaries

desc "Run unit tests"
task :test => [:test_binaries] do
    sh "./build/tests/test_redis_util"
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
source = OcamlFile.new("src/redis.ml")

file source.cmx => [source.ml, source.cmi, "build"] do
    compile source.ml, source.dest
end
file source.cmi => [source.mli, "build"] do
    compile source.mli, source.cmi
end
task :library => [source.cmx, source.cmi]

# Test files

script = OcamlFile.new("tests/script.ml")
directory "build/tests"

# Redis_utils is a bit funny because we need to test more functions than the default mli files gives us access to
test_objects = "build/tests/test_objects"
directory test_objects
test_redis_util = OcamlFile.new("tests/test_redis_util.ml")
file test_redis_util.dest("build/tests") => [script.cmx("build/tests"), test_objects, test_redis_util, source] do
    cp source, source.ml(test_objects)
    compile source.ml(test_objects), source.dest(test_objects)
    sh "ruby tests/create_test.rb #{test_redis_util} > build/tests/run_test_redis_util.ml"
    sh "ocamlopt -c -o #{test_redis_util.dest("build/tests")} -I build/tests -I #{test_objects} #{test_redis_util}"
    sh "ocamlopt -o #{test_redis_util.dest("build/tests")} -I build/tests #{external_libs} #{script.cmx("build/tests")} #{source.cmx(test_objects)} #{test_redis_util.cmx("build/tests")} build/tests/run_test_redis_util.ml"
end
multitask :test_binaries => "build/tests/test_redis_util"

# Now for all other tests
all_other_tests = OcamlFile.new("build/tests/all_other_tests.ml")
test_files = FileList["tests/test_*.ml"].exclude(/test_redis_util.ml$/).map{|f| OcamlFile.new(f)}
test_files.each do |test_file|
    file test_file.cmx("build/tests") => [test_file, script.cmx("build/tests"), :library] do
        sh "ocamlopt -w X -c -I build -I build/tests -o #{test_file.cmx("build/tests")} #{test_file}"
    end
    multitask :individual_test_objects => test_file.cmx("build/tests")
end

multitask :test_binaries => all_other_tests.dest("build/tests")
file all_other_tests.dest("build/tests") => all_other_tests do
    sh "ocamlopt -o #{all_other_tests.dest("build/tests")} -I build/tests -I build #{external_libs} #{script.cmx("build/tests")} #{source.cmx} #{test_files.map{|f| f.cmx("build/tests")}.join(" ")} #{all_other_tests}"
end
file all_other_tests => :individual_test_objects do
    sh "ruby tests/create_test.rb #{test_files.join(" ")} > #{all_other_tests}"
end

# Smoke test
file "build/tests/smoke_test" => [:library, "build/tests/smoke_test.cmx", "build/tests"] do
    sh "ocamlopt -I build -o build/tests/smoke_test #{external_libs} #{source.cmx} build/tests/smoke_test.cmx"
end

file "build/tests/smoke_test.cmx" => [:library, "tests/smoke_test.ml", "build/tests"] do
    compile "tests/smoke_test.ml", "build/tests/smoke_test"
end

file script.cmx("build/tests") => ["build/tests", "tests/script.ml"] do
    sh "ocamlopt -c -o #{script.dest("build/tests")} #{script}"
end

# Documentation
directory "doc"
file "doc/index.html" => ["doc", "src/redis.mli"] do
    sh "ocamldoc -html -d doc src/redis.mli"
end
