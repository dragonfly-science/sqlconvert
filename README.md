# Convert SQL Server dump scripts into PostgreSQL

This script was developed to solve the simple problem of getting data from SQL Server to PostgreSQL. It is very simple, and probably doesn't do what you want. But in case it does, here you are.

## Building and installing

You can build the executable using cabal:

    user@computer:~/$ cabal install

That will create an executable called `dist/build/sqlconvert/sqlconvert`. It can be copied to another machine, for example to `/usr/local/bin/sqlconvert`

## Using it

The script expects an ASCII formatted SQL Server dump file. UTF-8 coming next. It reads standard in and writes standard out. So, for example:

    user@computer:~/$ createdb mydb
    user@computer:~/$ time cat mydb-sql-server.sql | sqlconvert 2> errors.txt | psql -d mydb -f -

It is also quite a good idea to set the `--set ON_ERROR_STOP=1` flag on the psql command.

The database is assumed to have already been created.

## How it works

It only reads certain information from the dump script: all the `CREATE TABLE` statements, the `ALTER TABLE` statements, and the `INSERT` statements.

The insert statements are rolled up into PostgreSQL's   `COPY` statement, which is much faster.

It also drops the not null modifier on sql server's `timestamp` fields (which is really a counter). And it sets any `Photo` fields to be nullable, and the type to `text`. 

SQL Server `DateTime` fields are converted into `TIMESTAMP WITHOUT TIME ZONE` fields.
