let () =
  let db = Core.Database.of_file "sample.xml" in
  Core.Database.print db
