desc "Compile the library"
task :library => ["lib/Redis.cmxa"]

desc "Run unit tests"
task :test => :test_binary do
    sh "./build/test"
end

desc "Run smoke test"
task :smoke_test => ["build/smoke_test"] do
    sh "./build/smoke_test"
end

build_objs = "Unix.cmxa lib/Redis.cmxa"

# Test files

desc "Create the binary used for testing"
task :test_binary => "build/test"

test_objs = FileList.new("tests/test*.ml").map do |filename|
    filename.pathmap("build/%f").ext("cmx")
end
file "build/test" => ([:library, "build/all_test.ml", "build/script.cmx"] + test_objs) do
    sh "ocamlopt -I build -o build/test Unix.cmxa lib/Redis.cmxa build/script.cmx #{test_objs.join(" ")} build/all_test.ml"
end

file "build/all_test.ml" => ["tests/create_test.rb"] do
    test_files = FileList["tests/test*.ml"].join(" ")
    sh "ruby tests/create_test.rb #{test_files} > build/all_test.ml"
end

file "build/smoke_test" => [:library, "tests/smoke_test.ml"] do
    sh "ocamlopt -I build -o build/smoke_test #{build_objs} tests/smoke_test.ml"
end


# Build binaries
directory "build"

all_object_files = FileList.new("src/*.ml").map do |filename|
    [filename, filename.pathmap("build/%f").ext("cmx")]
end
all_object_files.each do |src, obj|
    file obj => ["build", src] do
        sh "ocamlopt -I build -c -o #{obj} #{src}"
    end
    if src != "src/redis_util.ml" then
        file obj => ["build/redis_util.cmx"]
    end
    if src != "src/redis.ml" then
        multitask "build/redis.cmx" => obj
    end
end


directory "lib"
file "lib/Redis.cmxa" => ["lib", "build/redis_util.cmx", "build/redis.cmx"] do
    objs = FileList.new("src/redis*.ml").map do |filename|
        filename.pathmap("build/%f").ext("cmx")
    end
    objs.reject! do |filepath|
        filepath == "build/redis_util.cmx"
    end
    sh "ocamlopt -c -I build -o build/redis src/redis.ml"
    sh "ocamlopt -a -o lib/Redis.cmxa build/redis_util.cmx #{objs.join(" ")}"
end
    
task :blah do
    objs = FileList.new("src/redis_*.ml").map do |filename|
        filename.pathmap("build/%f").ext("cmx")
    end
    puts objs
end
    

file "build/script.cmx" => [:library, "build", "tests/script.ml"] do
    sh "ocamlopt -I build -c -o build/script tests/script.ml"
end
    
FileList.new("tests/test_*.ml").each do |filename|
    build_obj = filename.pathmap("build/%f").ext("cmx")
    original_obj = filename.pathmap("build/%{test_,}f").ext("cmx")
    file build_obj => ["build", "build/script.cmx", original_obj, filename] do
        sh "ocamlopt -c -I build -o #{build_obj.pathmap("build/%n")} #{filename}"
    end
    multitask :all_test_binaries => build_obj
end

desc "Delete droppings"
task :clean do
    rm_rf "build"
end
