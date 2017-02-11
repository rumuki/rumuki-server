module Migrations
       (main)
       where

import           Control.Monad.Logger (LoggingT)
import           Database.Persist.Postgresql (ConnectionPool, runSqlPool, pgConnStr)
import           Import
import           System.IO.Silently
import qualified Moo.Core as M
import qualified Moo.Main as M
import qualified Data.Text as T
import qualified Control.Exception as E (catch)
import           Database.Persist.Sql (showMigration, printMigration)
import Database.Schema.Migrations.Backend.HDBC (hdbcBackend)
import Database.HDBC.PostgreSQL (connectPostgreSQL)
import           System.Exit

main :: AppSettings -> ConnectionPool -> LoggingT IO ()
main appSettings pool = do

  -- If we are testing, then best to silence any annoying messages
  let isTesting = appIsTesting appSettings

  -- Delegate any manually written migrations to the
  -- dbmigrations package first (AKA moo)
  ------------------------------------------------------------------------------

  $(logInfo) "Running any new migrations in `./migrations` folder"
  let dbConf = appDatabaseConf appSettings
      _connectionString = T.unpack . decodeUtf8 . pgConnStr $ dbConf
      _migrationStorePath = "./migrations"
      _linearMigrations = False
      _timestampFilenames = False
      configuration = M.Configuration {..}

  connection <- liftIO $ connectPostgreSQL _connectionString
  let backend = hdbcBackend connection
      parameters = M.makeParameters configuration backend

  liftIO $ E.catch
    ((if isTesting then hSilence [stdout, stderr] else id)
     $ M.mainWithParameters ["upgrade"] parameters)
    catchExit

  -- Now diff database with schema. If there are any differences, *show*
  -- the actions that need to be taken by the user and abort.
  ------------------------------------------------------------------------------
  flip runSqlPool pool $ do
    requiredMigrations <- showMigration migrateAll
    unless (null requiredMigrations) $ do
      liftIO $ putStrLn "Migrations required. Consider using these:"
      printMigration migrateAll
      error "Schema and database mismatch (more migrations needed)"

catchExit :: ExitCode -> IO ()
catchExit ExitSuccess = return ()
catchExit (ExitFailure _) = error "Migrations failed to run successfully"
