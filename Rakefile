# Copyright (C) 2009 Rory Geoghegan - r.geoghegan@gmail.com
# Released under the BSD license. See the LICENSE.txt file for more info.

desc "Compile the library"
task :library => ["lib/Redis.cmxa"]

desc "Create the binary used for testing"
task :test_binary => "build/test"

desc "Run unit tests"
task :test => :test_binary do
    sh "./build/test"
end

desc "Run smoke test"
task :smoke_test => ["build/smoke_test"] do
    sh "./build/smoke_test"
end

desc "Delete droppings"
task :clean do
    rm_rf "build"
    rm_rf "lib"
end

desc "Start a Redis Server"
task :start_redis do
    sh "redis-server var/run/redis.conf"
end

desc "Create an ocaml top level that uses Redis"
task :toplevel => "build/redis_ocaml_toplevel"

directory "build"
directory "lib"

# Utility code

class OcamlFileList < FileList
    def with_prefix_and_extension prefix, extension
        if prefix != "" then
            prefix = "#{prefix}/"
        end
        self.map do |filename|
            filename.pathmap("#{prefix}%f").ext(extension)
        end
    end
    def to_cmx prefix="build"
        with_prefix_and_extension prefix, "cmx"
    end
    def to_cmxa prefix="build"
        with_prefix_and_extension prefix, "cmxa"
    end
    def to_cma prefix="build"
        with_prefix_and_extension prefix, "cma"
    end
    def to_mli prefix="src"
        with_prefix_and_extension prefix, "mli"
    end
    def to_cmi prefix="build"
        with_prefix_and_extension prefix, "cmi"
    end
    def to_dest prefix="build"
        self.map do |filename|
            filename.pathmap("#{prefix}/%n") # remove ext
        end
    end
end

def compile source, dest
    sh "ocamlopt -w X -c -I build -o #{dest} #{source}"
end

# Library

external_libs = OcamlFileList.new(["Unix", "Str", "nums"])
lib_objs = OcamlFileList.new("src/redis*.ml")

redis_util_cmx = "build/redis_util.cmx"
lib_objs.zip(lib_objs.to_dest, lib_objs.to_cmx, lib_objs.to_mli, lib_objs.to_cmi) do |ml, dest, cmx, mli, cmi|
    file cmx => [ml, cmi, "build"] do
        compile ml, dest
    end
    if dest !~ /redis_util$/ then
        file cmx => redis_util_cmx
    end
        
    file cmi => [mli, "build"] do
        compile mli, cmi
    end
    multitask :lib_obj_files => cmx
end

file "lib/Redis.cmxa" => ["lib", :lib_obj_files] do
    objs = lib_objs.to_cmx.reject{|obj| obj.match(/redis_util.cmx$/)}
    sh "ocamlopt -w X -a -o lib/Redis.cmxa build/redis_util.cmx #{objs}"
end

# Test files

directory "build/test_includes"
lib_objs.zip(lib_objs.to_mli("build/test_includes"), lib_objs.to_cmi("build/test_includes")) do |ml, mli, cmi|
    file mli => [ml, "build/test_includes"] do
        sh "exec ocamlopt -I build -i #{ml} > #{mli}"
    end
    file cmi => [mli, "build/test_includes"] do
        compile mli,cmi
    end
    multitask :test_headers => cmi
end
    
test_files = OcamlFileList.new("tests/test*.ml")
file "build/test" => [:library, "build/all_test.ml", "build/script.cmx", :all_test_binaries] do
    sh "ocamlopt -w X -I build/test_includes -o build/test #{external_libs.to_cmxa("")} lib/Redis.cmxa build/script.cmx #{test_files.to_cmx} build/all_test.ml"
end

file "build/all_test.ml" => (["tests/create_test.rb"] + test_files.to_cmx) do
    sh "ruby tests/create_test.rb #{test_files} > build/all_test.ml"
end

file "build/smoke_test" => [:library, "tests/smoke_test.ml"] do
    compile "tests/smoke_test.ml", "build/smoke_test"
    sh "ocamlopt -I build -o build/smoke_test #{external_libs.to_cmxa("")} lib/Redis.cmxa build/smoke_test.cmx"
end

test_files.zip(test_files.to_cmx, test_files.to_dest) do |ml, cmx, dest|
    file cmx => ["build", "build/script.cmx", :library, :test_headers, ml] do
        sh "ocamlopt -w X -c -I build/test_includes -I build -o #{dest} #{ml}"
    end
    multitask :all_test_binaries => cmx
end

file "build/script.cmx" => [:library, "build", "tests/script.ml"] do
    sh "ocamlopt -I build -c -o build/script tests/script.ml"
end

# Miscelanious

file "build/redis_ocaml_toplevel" => :lib do
    sh "ocamlmktop -o build/redis_ocaml_toplevel #{external_libs.to_cma("")} lib/redis.cmxa"
end
