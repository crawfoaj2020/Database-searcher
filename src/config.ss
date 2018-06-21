(library (config)
  (export
   setup-config-db
   )
  (import
   (chezscheme)
   (swish imports)
   )

  (define schema-name 'config)
  (define schema-version "2018-06-21")

  (define (setup-config-db)
    (match (log-db:version schema-name)
      [,@schema-version (create-db)]
      [#f
       (log-db:version schema-name schema-version)
       (create-db)]
      [,version (raise `#(unsupported-db-version ,schema-name ,version))]))    

  (define (create-db)
    (void))
  )
