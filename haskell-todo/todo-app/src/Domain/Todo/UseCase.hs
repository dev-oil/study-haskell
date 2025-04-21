module Domain.Todo.UseCase where

import Control.Error.Util ((??))
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import Data.Time (getCurrentTime)
import Data.ULID (getULID)
import Domain.Todo.Task (Task, mkTask)

createTask :: (MonadIO m) => Text -> m (Either Text Task)
createTask content = runExceptT $ do
  taskId <- liftIO getULID
  now <- liftIO getCurrentTime
  mkTask taskId content now ?? "Task content must be less than 12 characters"