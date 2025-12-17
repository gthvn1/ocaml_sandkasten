let () =
  let db = Database.of_file "sample.xml" in
  Database.print db
