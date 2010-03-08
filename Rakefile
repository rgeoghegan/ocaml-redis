# Copyright (C) 2009 Rory Geoghegan - r.geoghegan@gmail.com
# Released under the BSD license. See the LICENSE.txt file for more info.

desc "Compile the library"
task :library => ["build/redis.cmxa"]

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
task :start_redis do
    sh "redis-server etc/redis.conf"
end

desc "Create an ocaml top level that uses Redis"
task :toplevel => "build/redis_ocaml_toplevel"

directory "build"

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
end
    
def compile source, dest
    sh "ocamlopt -w X -c -I build -o #{dest} #{source}"
end

# Library

redis_util = OcamlFile.new("src/redis_util.ml")
external_libs = ["Unix.cmxa", "Str.cmxa", "nums.cmxa"].join(" ")
lib_objs = FileList.new("src/redis*.ml").map{|f| OcamlFile.new(f)}

lib_objs.each do |lib|
    file lib.cmx => [lib.ml, lib.cmi, "build"] do
        compile lib.ml, lib.dest
    end
    file lib.cmi => [lib.mli, "build"] do
        compile lib.mli, lib.cmi
    end
    multitask :lib_obj_files => lib.cmx
end
lib_objs.reject{|l| l == redis_util}.each do |lib|
    file lib.cmx => redis_util.cmx
    file lib.cmi => redis_util.cmi
end

file "build/redis.cmxa" => [:lib_obj_files] do
    objs = lib_objs.reject{|l| l == redis_util}.map{|l| l.cmx}
    sh "ocamlopt -w X -a -I build -o build/redis.cmxa build/redis_util.cmx #{objs.join(" ")}"
end

# Test files

script = OcamlFile.new("tests/script.ml")
directory "build/tests"

# redis_utils is a bit funny because we need to test more methods than the default mli files gives us access to
test_objects = "build/tests/test_objects"
directory test_objects
test_redis_util = OcamlFile.new("tests/test_redis_util.ml")
file test_redis_util.dest("build/tests") => [script.cmx("build/tests"), test_objects, test_redis_util, redis_util] do
    cp redis_util, redis_util.ml(test_objects)
    compile redis_util.ml(test_objects), redis_util.dest(test_objects)
    sh "ruby tests/create_test.rb #{test_redis_util} > build/tests/run_test_redis_util.ml"
    sh "ocamlopt -c -o #{test_redis_util.dest("build/tests")} -I build/tests -I #{test_objects} #{test_redis_util}"
    sh "ocamlopt -o #{test_redis_util.dest("build/tests")} -I build/tests #{external_libs} #{script.cmx("build/tests")} #{redis_util.cmx(test_objects)} #{test_redis_util.cmx("build/tests")} build/tests/run_test_redis_util.ml"
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
    sh "ocamlopt -o #{all_other_tests.dest("build/tests")} -I build/tests -I build #{external_libs} #{script.cmx("build/tests")} build/redis.cmxa #{test_files.map{|f| f.cmx("build/tests")}.join(" ")} #{all_other_tests}"
end
file all_other_tests => :individual_test_objects do
    sh "ruby tests/create_test.rb #{test_files.join(" ")} > #{all_other_tests}"
end

# Smoke test
file "build/tests/smoke_test" => [:library, "tests/smoke_test.ml", "build/tests"] do
    sh "ocamlopt -I build -o build/tests/smoke_test #{external_libs} build/redis.cmxa tests/smoke_test.ml"
end

file script.cmx("build/tests") => ["build/tests", "tests/script.ml"] do
    sh "ocamlopt -c -o #{script.dest("build/tests")} #{script}"
end

# Miscellaneous

file "build/redis_ocaml_toplevel" => :lib do
    sh "ocamlmktop -o build/redis_ocaml_toplevel #{external_libs.to_cma("")} lib/redis.cmxa"
end
