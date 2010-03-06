# Copyright (C) 2009 Rory Geoghegan - r.geoghegan@gmail.com
# Released under the BSD license. See the LICENSE.txt file for more info.

desc "Compile the library"
task :library => ["build/redis.cmxa"]

desc "Create the binary used for testing"
task :test_binaries # "build/test/test_everything_else"] do

desc "Run unit tests"
task :test => [:test_binaries] do
    sh "./build/tests/test_redis_util"
    #sh "./build/test/test_everything_else"
end

desc "Run smoke test"
task :smoke_test => ["build/smoke_test"] do
    sh "./build/smoke_test"
end

desc "Delete droppings"
task :clean do
    rm_rf "build"
end

desc "Start a Redis Server"
task :start_redis do
    sh "redis-server var/run/redis.conf"
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
file test_redis_util.dest("build/tests") => [script.cmx("build/tests"), test_objects, test_redis_util] do
    cp redis_util, redis_util.ml(test_objects)
    compile redis_util.ml(test_objects), redis_util.dest(test_objects)
    sh "ruby tests/create_test.rb #{test_redis_util} > build/tests/run_test_redis_util.ml"
    sh "ocamlopt -c -o #{test_redis_util.dest("build/tests")} -I build/tests -I #{test_objects} #{test_redis_util}"
    sh "ocamlopt -o #{test_redis_util.dest("build/tests")} -I build/tests #{external_libs} #{script.cmx("build/tests")} #{redis_util.cmx(test_objects)} #{test_redis_util.cmx("build/tests")} build/tests/run_test_redis_util.ml"
end
multitask :test_binaries => "build/tests/test_redis_util"

#lib_objs.zip(lib_objs.to_mli("build/test_includes"), lib_objs.to_cmi("build/test_includes")) do |ml, mli, cmi|
#    file mli => [ml, "build/test_includes"] do
#        sh "exec ocamlopt -I build -i #{ml} > #{mli}"
#    end
#    file cmi => [mli, "build/test_includes"] do
#        compile mli,cmi
#    end
#    multitask :test_headers => cmi
#end
#    
#test_files = OcamlFileList.new("tests/test*.ml")
#file "build/test" => [:library, "build/all_test.ml", "build/script.cmx", :all_test_binaries] do
#    sh "ocamlopt -w X -I build/test_includes -o build/test #{external_libs.to_cmxa("")} build/redis.cmxa build/script.cmx #{test_files.to_cmx} build/all_test.ml"
#end
#
#file "build/all_test.ml" => (["tests/create_test.rb"] + test_files.to_cmx) do
#    sh "ruby tests/create_test.rb #{test_files} > build/all_test.ml"
#end
#
#file "build/smoke_test" => [:library, "tests/smoke_test.ml"] do
#    compile "tests/smoke_test.ml", "build/smoke_test"
#    sh "ocamlopt -I build -o build/smoke_test #{external_libs.to_cmxa("")} build/redis.cmxa build/smoke_test.cmx"
#end
#
#test_files.zip(test_files.to_cmx, test_files.to_dest) do |ml, cmx, dest|
#    file cmx => ["build", "build/script.cmx", :library, :test_headers, ml] do
#        sh "ocamlopt -w X -c -I build/test_includes -I build -o #{dest} #{ml}"
#    end
#    multitask :all_test_binaries => cmx
#end
#
file script.cmx("build/tests") => ["build/tests", "tests/script.ml"] do
    sh "ocamlopt -c -o #{script.dest("build/tests")} #{script}"
end

# Miscelanious

file "build/redis_ocaml_toplevel" => :lib do
    sh "ocamlmktop -o build/redis_ocaml_toplevel #{external_libs.to_cma("")} lib/redis.cmxa"
end
