# fumarks

## about

fumarks is a single-user Ruby command-line application to store, tag
and lookup your bookmarks.

## build system

### clean

```
rake clean   # note: deletes the db directory
```

### run the database server (requires MongoDB)
```
rake run_db
```

### for running tests
```
rake run_db   # in a different shell
rake test     # note: will clear the database before starting tests
```

## example usage

### display help

```
fumarks.rb
```

### add a bookmark

```
fumarks.rb add "google.com" "Google Homepage" "google,search" "ggl"
```
