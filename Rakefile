desc "Compile the library"
task :redis_lib

desc "Run unit tests"
task :test => [:clean, :test_binary] do
    sh "./build/test"
end

# Test files

desc "Create the binary used for testing"
task :test_binary => [:redis_lib, "build/all_test.cmx"] do
    sh "ocamlopt -I build -o build/test unix.cmxa redis.cmx test_redis.cmx all_test.cmx"
end

file "build/all_test.cmx" => [:all_test_binaries, "build/all_test.ml"] do
    sh "ocamlopt -c -I build -o build/all_test build/all_test.ml"
end

file "build/all_test.ml" => ["tests/create_test.rb"] do
    test_files = FileList["tests/test*.ml"].join(" ")
    sh "ruby tests/create_test.rb #{test_files} > build/all_test.ml"
end


# Build binaries
directory "build"

FileList.new("src/*.ml").each do |filename|
    build_obj = filename.pathmap("build/%f").ext("cmx")
    file build_obj => ["build", filename] do
        sh "ocamlopt -c -o #{build_obj.pathmap("build/%n")} #{filename}"
    end
    multitask :redis_lib => build_obj
end

FileList.new("tests/*.ml").each do |filename|
    build_obj = filename.pathmap("build/%f").ext("cmx")
    file build_obj => ["build", filename] do
        sh "ocamlopt -c -I build -o #{build_obj.pathmap("build/%n")} #{filename}"
    end
    original_obj = filename.pathmap("build/%{test_,}f").ext("cmx")
    file build_obj => original_obj
    multitask :all_test_binaries => build_obj
end

desc "Delete droppings"
task :clean do
    rm_rf "build"
end
