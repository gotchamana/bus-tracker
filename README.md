# Bus tracker

## Development

### Application server restarts on file change

```bash
watchexec \
    --shell none \
    --restart \
    --watch app \
    --watch lib \
    --watch data/config.json \
    -- cabal run -O0
```

### Database setup (PostgreSQL)

#### Commands

```bash
# Initialize the database
pg_ctl initdb --pgdata=data/database/data

# Start the database
pg_ctl start --pgdata=data/database/data --log=data/database/pg.log

# Stop the database
pg_ctl stop --pgdata=data/database/data

# Create user
createuser --host=$PWD/data/database bus

# Create database
createdb --host=$PWD/data/database --owner=bus bus-tracker

# Connect to the database
psql --host=$PWD/data/database --username=bus --dbname=bus-tracker
```

#### Configuration

When start the PostgreSQL server, you may encounter the error:

```text
could not create lock file "/run/postgresql/.s.PGSQL.5432.lock": No such file or directory
```

Instead of manually creating the `/run/postgresql` directory, you can modify the
`postgresql.conf` in the data directory (e.g. `data/database/data`), change the
`unix_socket_directories` to other existing directory.

### Database migration

Change directory into `liquibase` and run the liquibase command:

```bash
# See the update sql
liquibase update-sql

# Apply the changes
liquibase update
```

### Module dependency rules

- `Bus.Logger` must not import any other `Bus` modules
- Modules under `Bus.Database` must not import `Bus` modules outside `Bus.Database`
- Modules under `Bus.Util` must not import `Bus` modules outside `Bus.Util`

## TODO

- RBAC authorization
- Add command line option `--config`
- Add basic single line comment feature for JSON config file
- Add `basePathSegments` config option
- Add default config
- Add log file rotation
- Bus api
