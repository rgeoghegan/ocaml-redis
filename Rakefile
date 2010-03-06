# Copyright (C) 2009 Rory Geoghegan - r.geoghegan@gmail.com
# Released under the BSD license. See the LICENSE.txt file for more info.

desc "Compile the library"
task :library => ["build/redis.cmxa"]

desc "Create the binary used for testing"
task :test_binary => "build/test"

desc "Run unit tests"
task :test

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
    def ml
        to_s
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
external_libs = ["Unix.cmxa", "Str.cmxa", "nums.cmxa"]
lib_objs = FileList.new("src/redis*.ml").map{|f| OcamlFile.new(f)}

redis_util_cmx = "build/redis_util.cmx"
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

#directory "build/test_includes"
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
#file "build/script.cmx" => [:library, "build", "tests/script.ml"] do
#    sh "ocamlopt -I build -c -o build/script tests/script.ml"
#end

# Miscelanious

file "build/redis_ocaml_toplevel" => :lib do
    sh "ocamlmktop -o build/redis_ocaml_toplevel #{external_libs.to_cma("")} lib/redis.cmxa"
end
