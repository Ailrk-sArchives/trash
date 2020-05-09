module refactorWithTraverse where

data Query      = Query
data SomeObj    = SomeObj
data IoOnlyObj  = IoOnlyObj
data Err        = Err

decodeFn :: String -> Either Err SomeObj
decodeFn = undefined

fetchFn :: Query -> IO [String]
fetchFn = undefined

makeIoOnlyObj :: [SomeObj] -> IO [(SomeObj, IoOnlyObj)]
makeIoOnlyObj = undefined

-- before
pipelineFnOld :: Query
              -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFnOld query = do
    a <- fetchFn query
    case sequence (map decodeFn a) of
      (Left err) -> return $ Left $ err
      (Right res) -> do
          a <- makeIoOnlyObj res
          return $ Right a


-- new
pipelineFnNew :: Query
              -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFnNew query = do
    a <- fetchFn query
    traverse makeIoOnlyObj (mapM decodeFn a)


-- point free
pipelineFnM :: Query
            -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFnM = (traverse makeIoOnlyObj . mapM decodeFn =<<) fetchFn

-- traverse is more generic mapM
pipelineFn :: Query
           -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn = (traverse makeIoOnlyObj
             . traverse decodeFn =<<)
             . fetchFn

